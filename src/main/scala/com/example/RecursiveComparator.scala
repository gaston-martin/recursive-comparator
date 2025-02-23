package com.example

import com.typesafe.scalalogging.StrictLogging
import org.joda.time.DateTime

import java.lang.reflect.Modifier
import java.{math => jm}
import scala.{math => sm}


class RecursiveComparator(ignoredFields: Seq[String] = Seq.empty[String]) extends StrictLogging {

  /**
    * Given two instances of either simple or composite classes along with the name of the property
    * holding a reference to the instances, or an empty name if it's the root of the tree
    * perform a recursive comparison of both instances yielding a collection of ComparisonResult
    * Any property in the reference tree can be set to be ignored when a diff is found by constructing
    * the class with a collection of property paths, where each inner field in the tree is delimited with a dot
    * @param left an instance of the same class as the right. Used for expected
    * @param right an instnance of the same class as the left. Used for actual.
    * @param propertyName an optional property name (used for recursion and ignoring of some diffs)
    * @param lookingForCandidates an optional boolean telling if this recursive call is being used to match elements of a sequence
    * @param level an integer for accounting of the number of levels.
    * @return a Sequence of ComparisonResult, where the relevant are the ComparisonDiff and ComparisonDiffIgnored
    */
  def recursiveCompare(left: Any, right: Any, propertyName: String = "", lookingForCandidates: Boolean = false, level: Int = 0): Seq[ComparisonResult] = {
    left match {
      // Comparación Simple de "primitivos" o Value Objects
      case element: Any if isSimplyComparable(element) => Seq(compareSimple(left, right, propertyName))
      // Comparación de BigDecimals
      case _: sm.BigDecimal => Seq(compareBigDecimal(left, right, propertyName))
      case _: jm.BigDecimal => Seq(compareBigDecimal(left, right, propertyName))
      // Comparación de Opcionales
      case _: Option[_] =>
        (left, right) match {
          case (Some(leftValue), Some(rightValue)) => recursiveCompare(leftValue, rightValue, propertyName) // Ambos Some
          case (None, None) => Seq(ComparisonOk(propertyName)) // Ambos None
          case _ => Seq(mismatchOption(left.asInstanceOf[Option[_]], right.asInstanceOf[Option[_]], propertyName)) // Uno Some y el otro None
        }
      // Comparación de Enums
      case _: scala.Enumeration#Value =>
        Seq(compareSimple(left, right, propertyName))
      // Comparación de Case Objects (Sealed Traits con Case Objects)
      case obj if Option(obj).exists(objt => isCaseObject(objt.getClass))  =>
        Seq(compareSimple(left, right, propertyName))
      // Comparación de nulls
      case obj if Option(obj).isEmpty => Seq(compareNull(left,right, propertyName))
      // Comparación de DateTimes (joda time)
      case dt: DateTime => Seq(compareDateTime(dt, right.asInstanceOf[DateTime], propertyName))
      // Comparación de Collections (pueden estar desordenadas)
      case _: Seq[_] =>
        compareSeq(left, right, propertyName)
      // Comparaciones Recursivas
      // Case Clases (heredan de Product)
      case _: scala.Product =>
        breakDownAndCompare(left, right, propertyName, lookingForCandidates, level)
      // Este caso es igual que las Case Clases pero para clases regulares con atributos.
      case _ =>
        breakDownAndCompare(left, right, propertyName, lookingForCandidates, level)
    }
  }

  // Funciones Auxiliares

  /**
    * Given two instances of a composite class, either a Case Class or a regular POJO class with comparable attributes
    * break down the class into its instance attributes, and compare each one individually
    * @param left an instance of a composite class, either a Case Class or a regular class
    * @param right an instance of a composite class, either a Case Class or a regular class
    * @param propertyName the name of the property holding a reference to the instances. It's expected to pile up with upper levels in the recursion.
    * @return
    */
  private def breakDownAndCompare(left: Any, right: Any, propertyName: String, lookingForCandidates: Boolean = false, level: Int = 0): Seq[ComparisonResult] = {
    left.getClass.getDeclaredFields.filter { field =>
      !Modifier.isStatic(field.getModifiers) // No me interesan los static fields
    }.flatMap { field =>
      field.setAccessible(true) // Los fields son privados en runtime
      val fieldName = field.getName
      val leftFieldValue = if (left == null) null else field.get(left)
      val rightFieldValue = if (right == null) null else field.get(right)
      val propertyPath = Seq(propertyName, fieldName).filter(_.nonEmpty).mkString(".")
      recursiveCompare(leftFieldValue, rightFieldValue, propertyPath)
    }.filter(comparison => comparison.isDiff() || comparison.isIgnored())
      // este map es para debuggear
//      .map {e=>
//        if (e.isDiff()){
//          val looking = lookingForCandidates
//          val indentation = "  " * level
//          println(s"$indentation $looking $propertyName: $e" )
//        }
//        e
//      }
      .toSeq // Discard recursive Oks
  }

  /**
    * Given two sequences (collections) of any kind of elements perform a comparison
    * of the two, encompassing both elements that are simple and elements that are composite (case classes)
    * If all elements match by equals, no need to go any further. This includes the empty vs empty case.
    * If any element of the two collections mismatch by any property value, then it's needed to ponder if the property
    * shall be ignored, so the elements can be compared as if they were the very same with a diff accounted for.
    * If any element is remaining (on any side) after the comparison it's considered a diff. This might include
    * repeated elements on one side that are not repeated on the other side. If the same element is repeated on
    * both collections it's not accounted as a diff.
 *
    * @param left a Seq[Any] is expected as the left side of the comparison
    * @param right a Seq[Any] is expected as the right side of the comparison
    * @param propertyName The name of the property being evaluated, for building of the properties path.
    * @return a flattened collection of ComparisonResult even if the collection contains elements of a composite class (case class)
    */
  private def compareSeq(left: Any, right: Any, propertyName: String): Seq[ComparisonResult] = {
    // Para las collections en el propertyName voy a agregar un .[] como hint.
    val modifiedPropertyName = s"$propertyName.[]"

    // Cast de left y right a collections para poder usar los métodos de Seq()
    val leftCollection = Option(left).getOrElse(Seq.empty[Any]).asInstanceOf[Seq[_]]
    val rightCollection = Option(right).getOrElse(Seq.empty[Any]).asInstanceOf[Seq[_]]

    // Si ambas listas son iguales (incluye empty vs empty) devuelvo un OK.
    if (leftCollection == rightCollection) {
      Seq(ComparisonOk(modifiedPropertyName))
      // Si es una lista de "primitivos" (AnyVal + String + Dates + BigDecimal)
    } else if (hasSimpleElements(leftCollection ++ rightCollection) || hasBigDecimalElements(leftCollection ++ rightCollection)) {
      val mutableLeft = leftCollection.toBuffer
      val mutableRight = rightCollection.toBuffer

      // Necesito averiguar las diferencias a izquierda y derecha
      // Todos lo que sobren después de buscar los que coincidan, son diffs.
      // Esto no se puede hacer con colecciones inmutables, de todas maneras esta clase entera es temporal.
      leftCollection.foreach { leftElement =>
        if (mutableRight.contains(leftElement)) {
          // Voy achicando las colecciones por el problema de que puede haber repetidos y lo queremos detectar.
          mutableLeft -= leftElement
          mutableRight -= leftElement
        }
      }
      val leftDiffs = mutableLeft.toArray.map(arrayElement => ComparisonDiff(modifiedPropertyName, arrayElement.toString, ""))
      val rightDiffs = mutableRight.toArray.map(arrayElement => ComparisonDiff(modifiedPropertyName, "", arrayElement.toString))
      if (shouldIgnore(propertyName) || shouldIgnore(modifiedPropertyName)) {
        (leftDiffs ++ rightDiffs).map(diffToIgnored)
      } else {
        leftDiffs ++ rightDiffs
      }
    } else {
      // Colección (Seq) de Case Clases (o Clases con atributos)
      val mutableRightCollection = rightCollection.toBuffer
      val leftDiffs: Seq[ComparisonResult] = leftCollection.flatMap { leftElement =>
        // No se cual es el elemento a matchear de la derecha
        val candidate: Option[Any] = searchMatchingElement(modifiedPropertyName, leftElement, mutableRightCollection) // Devuelve un optional
        candidate match {
          // Si no encuentro elemento candidato en la derecha, es una diff.
          case None => Seq(ComparisonDiff(modifiedPropertyName, leftElement.toString, ""))
          case Some(rightElement) =>
            // r es el candidato que quiero usar para comparar de la derecha
            mutableRightCollection -= rightElement
            recursiveCompare(leftElement, rightElement, modifiedPropertyName) // Podria devolver un ignored
              .filter(result => result.isDiff() || result.isIgnored())
        }
      }

      // Lo que quedo sin usar de la derecha, sobra y es un diff
      val rightDiffs: Seq[ComparisonDiff] = mutableRightCollection.toList.map { element =>
        ComparisonDiff(modifiedPropertyName, "", element.toString)
      }

      if (shouldIgnore(propertyName) || shouldIgnore(modifiedPropertyName)) {
        // Convierto los ComparisonDiff a ComparisonDiffIgnored?
        (leftDiffs ++ rightDiffs).map {
          case diff: ComparisonDiff => diffToIgnored(diff)
          case diffIgnored: ComparisonDiffIgnored => diffIgnored
        }
      } else {
        // Devuelvo todos los diffs
        leftDiffs ++ rightDiffs
      }
    }
  }

  private def isCaseObject(clazz: Class[_]): Boolean = clazz.getName.endsWith("$")

  /**
    * Given two simple value objects such as primitives, enums, and dates
    * compare the elements against each other and return an abstraction of either
    * ComparisonOk, ComparisonDiff or ComparisonDiff if a diff is found but
    * the property is set to be ignored.
    *
    * @param left         a value object
    * @param right        a value object
    * @param propertyPath the path of the property (i.e prop.subproperty.subproperty)
    * @return a ComparisonResult depending of the comparison.
    */
  private def compareSimple(left: Any, right: Any, propertyPath: String): ComparisonResult = {
    left == right match {
      case true => ComparisonOk(propertyPath)
      case false if shouldIgnore(propertyPath) => ComparisonDiffIgnored(propertyPath, s"$left", s"$right")
      case false => ComparisonDiff(propertyPath, s"$left", s"$right")
    }
  }

  private def compareNull(left: Any, right: Any, propertyPath: String): ComparisonResult = {
    (left, right) match {
      case (null, null) => ComparisonOk(propertyPath)
      case _ if shouldIgnore(propertyPath) => ComparisonDiffIgnored(propertyPath, s"$left", s"$right")
      case _ => ComparisonDiff(propertyPath, s"$left", s"$right")
    }
  }

  /**
    * Given BigDecimal (either Scala or Java)
    * compare the elements against each other and return an abstraction of either
    * ComparisonOk, ComparisonDiff or ComparisonDiff if a diff is found but
    * the property is set to be ignored.
    * Note that BigDecimal("1.0") is not equals to BigDecimal("1.00") due to different scale
    * hence the comparison must be made with the compare() or compareTo() method.
    * @param left
    * @param right
    * @param propertyPath
    * @return
    */
  private def compareBigDecimal(left: Any, right: Any, propertyPath: String): ComparisonResult = {
    val comparison = left match {
       case _: sm.BigDecimal =>
         left.asInstanceOf[sm.BigDecimal].compare(right.asInstanceOf[sm.BigDecimal])
       case _: jm.BigDecimal =>
         left.asInstanceOf[jm.BigDecimal].compareTo(right.asInstanceOf[jm.BigDecimal])
    }
    comparison == 0 match {
      case true => ComparisonOk(propertyPath)
      case false if shouldIgnore(propertyPath) => ComparisonDiffIgnored(propertyPath, s"$left", s"$right")
      case false => ComparisonDiff(propertyPath, s"$left", s"$right")
    }
  }

  private def compareDateTime(left: DateTime, right: DateTime, propertyPath: String): ComparisonResult = {
    (Option(left), Option(right)) match {
      case (None, None) => ComparisonOk(propertyPath)
      case (None, Some(_)) if shouldIgnore(propertyPath)=> ComparisonDiffIgnored(propertyPath, "null", s"$right")
      case (None, Some(_)) => ComparisonDiff(propertyPath, "null", s"$right")
      case (Some(_), None) if shouldIgnore(propertyPath)=> ComparisonDiffIgnored(propertyPath, s"$left", "null")
      case (Some(_), None) => ComparisonDiff(propertyPath, s"$left", "null")
      case (Some(_), Some(_)) => left.compareTo(right) == 0 match {
        case true => ComparisonOk(propertyPath)
        case false if shouldIgnore(propertyPath) => ComparisonDiffIgnored(propertyPath, s"$left", s"$right")
        case false => ComparisonDiff(propertyPath, s"$left", s"$right")
      }
    }
  }

  /**
    * Given a property path (ie. "routes.[].id") figure out if the diff
    * must be ignored
    *
    * @param propertyPath a String representing the paths of the properties from the root object. Separated by dots.
    * @return
    */
  private def shouldIgnore(propertyPath: String): Boolean = ignoredFields.contains(propertyPath)

  /**
    * Given two optionals known to differ, convert them to a ComparisonDiff or ComparisonDiffIgnored
    * depending of the given propertyPath belonging to the ignoredFields given at construct time.
    *
    * @param left         an Optional known to be different from right
    * @param right        an Optional known to be different from left
    * @param propertyPath the path of the properties for checking if the diff shoud be ignored.
    * @return Either a ComparisonDiff or a ComparisonDiffIgnored
    */
  private def mismatchOption(left: Option[_], right: Option[_], propertyPath: String): ComparisonResult = {
    if (shouldIgnore(propertyPath)) {
      ComparisonDiffIgnored(propertyPath, left.map(_.toString).getOrElse("None"), right.map(_.toString).getOrElse("None"))
    } else {
      ComparisonDiff(propertyPath, left.map(_.toString).getOrElse("None"), right.map(_.toString).getOrElse("None"))
    }
  }

  /**
    * Given an element of Any type, tell if it can be compared by ==
    * or a recursive comparison is needed
    * The choice is made based on its type
    *
    * @param element the element to be tested
    * @return a boolean telling either true or false
    */
  private def isSimplyComparable(element: Any): Boolean = {
    element match {
      case _: String => true
      case _: Long => true
      case _: Int => true
      case _: Boolean => true
      case _: Double => true
      case _: java.math.BigDecimal => false // Explicitly disable == comparison for BigDecimals
      case _: scala.math.BigDecimal => false // Explicitly disable == comparison for BigDecimals
      case _: DateTime => false // timezones might differ even if the moment in time is the same
      case _ => false
    }
  }

  /**
    * Given a Collection of Any kind, considering that all the elements are the same kind,
    * and that there is no generics info about the elements available at runtime
    * analyze the firt element of the collection with pattern matching and determine
    * whether it's a value object or a complex class (i.e a Case Class).
    * It's done because complex classes need a recursive analysis instead of being directly
    * compared
    *
    * @param collection A collection (typically a Seq) constructed from the concatenation of both left and right objects
    * @return a Boolean telling if it's a simple value to be directly compared or a complex object that needs recursion
    */
  private def hasSimpleElements(collection: Seq[_]): Boolean = {
    // Como se borra el tipo genérico en runtime no puedo saber el tipo si no extraigo un elemento
    val firstElement = collection.headOption
    firstElement match {
      case None => true // Lista vacía, por lo tanto puedo compararlas por ==
      case Some(value) if isSimplyComparable(value) => true
      case Some(value) if isCaseObject(value.getClass) => true
      case _ => false // Comparar por recursion
    }
  }

  /**
    * BigDecimal can fail even if both are the same number, due to different scales (i.e x.0 vs x.00)
    * Detect if the collection comprises BigDecimals (both java and scala)
    * @param collection
    * @return
    */
  private def hasBigDecimalElements(collection: Seq[_]): Boolean = {
    val firstElement = collection.headOption
    firstElement match {
      case None => true // Lista vacía, por lo tanto puedo compararlas por ==
      case Some(value) => value match {
        case _: java.math.BigDecimal => true
        case _: scala.math.BigDecimal => true
        case _ => false
      }
    }
  }

  /**
    * Converts a ComparisonDiff to a ComparisonDiffIgnored by reusing their same fields
    *
    * @param diff an instance of ComparisonDiff
    * @return an instance of ComparisonDiffIgnored with the very same values.
    */
  private def diffToIgnored(diff: ComparisonDiff): ComparisonDiffIgnored = ComparisonDiffIgnored(diff.propertyPath, diff.left, diff.right)

  // Este método es para hacer diffs entre dos elementos de dos listas, que pueden tener
  // sus elementos desordenados (Roma vs Airbase!) y ademas pueden diferir en algún atributo
  // por lo cual un simple listB.contains(elementA) no funciona. (Contains() es sólo exact match)
  // Elije un elemento candidato de la segunda lista. Si hay uno idéntico devuelve ese
  // Si hay mas de uno idéntico devuelve cualquiera (no importa, sería un repetido)
  // Si hay diferencias, y están listadas para ignorar, es como si coincidieran.
  // Si hay diferencias y no están listadas para ignorar, se devuelve None y vas a tener dos diff
  // una de (left; nada) y la otra de (nada;right) porque no pude matchear (left; right)
  // Si la lista esta vacía devuelve un None
  private def searchMatchingElement(propertyName: String, element: Any, listOfCandidates: Seq[Any]): Option[Any] = {
    // Recorrer la lista de candidatos
    val comparisons: Seq[(Any, Int, Int)] = listOfCandidates.map { candidate =>
      // Para cada candidato pide los diffs.
      // Obtiene una Tuple2 (Candidato, Seq(Comparaciones))
      val diffsInMap: Map[Boolean, Seq[ComparisonResult]] = recursiveCompare(element, candidate, propertyName, true)
        .filter(result => !result.isOk()) // las OK no me sirven
        .groupBy(diff => diff.isIgnored()) // Convierto a mapa (true -> las ignoradas, false -> las diffs problemáticas
      // la high order function de map devuelve una tupla de (Candidato, diffs ignoradas, diffs reales)
      (candidate, diffsInMap.get(true).size, diffsInMap.get(false).size)
    }
    // Los elementos que no tienen diff son mejores candidatos que los que tienen alguna diff

    val candidatesChosen: Seq[Any] = comparisons
      .filter(_._3 == 0) // no quiero diffs. Si un elemento tiene diffs se descarta.
      .sortBy(_._2) // Ordeno por diffs ignored menor a mayor (0 gana)
      .map(_._1) // Como me voy a quedar con el primer elemento de la tupla (el candidato), descarto la info de cantidad de diffs

    // Me quedo con el primer elemento o un None si la lista quedó vacía.
    val maybeCandidate: Option[Any] = candidatesChosen.headOption
    maybeCandidate match {
      case None => None // No hay candidatos, por la comparación left vs right voy a generar 2 diffs (left vs nada y nada vs right)
      case candidate: Some[Any] => candidate // Devolver el candidato
    }
  }
}

// Decidí reportar los left y right como Strings para no diferir los puntos en donde proceso los resultados
// de esta manera el Any.toString se invoca para crear/instanciar la diff.
trait ComparisonResult {
  val propertyPath: String
  def isOk(): Boolean
  def isDiff(): Boolean
  def isIgnored(): Boolean
}

case class ComparisonOk(propertyPath: String) extends ComparisonResult {
  override def isOk(): Boolean = true
  override def isDiff(): Boolean = false
  override def isIgnored(): Boolean = false
}

case class ComparisonDiff(propertyPath: String, left: String = "", right: String = "") extends ComparisonResult {
  override def isOk(): Boolean = false
  override def isDiff(): Boolean = true
  override def isIgnored(): Boolean = false
}

case class ComparisonDiffIgnored(propertyPath: String, left: String = "", right: String = "") extends ComparisonResult {
  override def isOk(): Boolean = false
  override def isDiff(): Boolean = false
  override def isIgnored(): Boolean = true
}
