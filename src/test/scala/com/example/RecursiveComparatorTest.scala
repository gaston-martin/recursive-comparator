package com.example

import com.example.{ComparisonDiff, ComparisonDiffIgnored, ComparisonOk, RecursiveComparator}
import org.joda.time.DateTime
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfter, FunSpec, Matchers}

class RecursiveComparatorTest extends FunSpec
  with Matchers
  with MockitoSugar
  with BeforeAndAfter {

  val templateSimpleCaseClass: Simple = Simple(string = "simple",
    int = 1,
    boolean = true,
    long = 12345678L,
    double = 200.0,
    javaBigDecimal = new java.math.BigDecimal("3333.33"),
    scalaBigDecimal = scala.math.BigDecimal("800.01"),
    jodaDate = org.joda.time.DateTime.now(),
    status = SampleStatusEnum.OK, // Enumeration
    direction = SampleDirection.North, // Case Object
    optional = None,
    sequence = Seq.empty[String])


  val templateThirdLevelCaseClass: ThirdLevel = ThirdLevel(innerString = "Moore", innerLong = 31337L, seq = Seq.empty[String], seqComposite = Seq.empty[Somebody], seqObject = Seq.empty[ClassSimple])
  val templateSecondLevelCaseClass: SecondLevel = SecondLevel(innerString = "Roger", thirdLevel = templateThirdLevelCaseClass)
  val templateCompositeClass: Composite = Composite(id = 1, boolean = false, secondLevel = templateSecondLevelCaseClass)
  val someDate: DateTime = DateTime.now()
  val someOtherDate: DateTime = someDate.plusHours(1)
  val templateClassSimple1 = new ClassSimple(string = "abc", int = 111, boolean = true, long = 1L, double = 0.0, javaBigDecimal = new java.math.BigDecimal("11.00"), scalaBigDecimal = scala.BigDecimal("12.00"), jodaDate = someDate)
  val templateClassSimple2 = new ClassSimple(string = "def", int = 111, boolean = true, long = 1L, double = 0.0, javaBigDecimal = new java.math.BigDecimal("11.00"), scalaBigDecimal = scala.BigDecimal("12.00"), jodaDate = someOtherDate)
  val templateClassSimple3 = new ClassSimple(string = "ghi", int = 222, boolean = false, long = 2L, double = 2.0, javaBigDecimal = new java.math.BigDecimal("13.00"), scalaBigDecimal = scala.BigDecimal("13.00"), jodaDate = someDate)
  val templateClassSimple4 = new ClassSimple(string = "abc", int = 111, boolean = true, long = 1L, double = 0.0, javaBigDecimal = new java.math.BigDecimal("11.00"), scalaBigDecimal = scala.BigDecimal("12.00"), jodaDate = someDate)


  describe("Two case classes without sub levels") {
    val left = templateSimpleCaseClass.copy()
    val right = templateSimpleCaseClass.copy()
    val comparator = new RecursiveComparator()

    describe("When there are no differences") {
      it("should have no differences reported") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }
    }

    describe("When there are two empty Seq") {
      val left = templateSimpleCaseClass.copy(sequence = Seq.empty[String])
      val right = templateSimpleCaseClass.copy(sequence = Seq.empty[String])

      it("should have no differences reported") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }
    }

    describe("When there are two identical Seq") {
      val left = templateSimpleCaseClass.copy(sequence = Seq("a", "b", "c"))
      val right = templateSimpleCaseClass.copy(sequence = Seq("a", "b", "c"))

      it("should have no differences reported") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }
    }

    describe("When there are two identical Seq with different order") {
      val left = templateSimpleCaseClass.copy(sequence = Seq("a", "b", "c"))
      val right = templateSimpleCaseClass.copy(sequence = Seq("b", "c", "a"))

      it("should have no differences reported") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }
    }

    describe("When there are differences") {
      it("should report the differences in a String field") {
        val left = templateSimpleCaseClass.copy(string = "alice")
        val right = templateSimpleCaseClass.copy(string = "bob")
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("string", "alice", "bob")))
      }

      it("should report the differences in a Long field") {
        val left = templateSimpleCaseClass.copy(long = 3333333L)
        val right = templateSimpleCaseClass.copy(long = 4444444L)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("long", "3333333", "4444444")))
      }

      it("should report the differences in an Int field") {
        val left = templateSimpleCaseClass.copy(int = 1)
        val right = templateSimpleCaseClass.copy(int = 2)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("int", "1", "2")))
      }

      it("should report the differences in a Boolean field") {
        val left = templateSimpleCaseClass.copy(boolean = true)
        val right = templateSimpleCaseClass.copy(boolean = false)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("boolean", "true", "false")))
      }

      it("should report the differences in a Double field") {
        val left = templateSimpleCaseClass.copy(double = 111.11)
        val right = templateSimpleCaseClass.copy(double = 222.22)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("double", "111.11", "222.22")))
      }

      it("should report the differences in a java BigDecimal field") {
        val left = templateSimpleCaseClass.copy(javaBigDecimal = new java.math.BigDecimal("444"))
        val right = templateSimpleCaseClass.copy(javaBigDecimal = new java.math.BigDecimal("555"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("javaBigDecimal", "444", "555")))
      }

      it("should not report a differences in a java BigDecimal field with same value a different scale") {
        val left = templateSimpleCaseClass.copy(javaBigDecimal = new java.math.BigDecimal("1.0"))
        val right = templateSimpleCaseClass.copy(javaBigDecimal = new java.math.BigDecimal("1"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }

      it("should report the differences in a scala BigDecimal field") {
        val left = templateSimpleCaseClass.copy(scalaBigDecimal = BigDecimal("600"))
        val right = templateSimpleCaseClass.copy(scalaBigDecimal = BigDecimal("700"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("scalaBigDecimal", "600", "700")))
      }

      it("should not report the differences in a scala BigDecimal field with same value and different scale") {
        val left = templateSimpleCaseClass.copy(scalaBigDecimal = BigDecimal("1.0"))
        val right = templateSimpleCaseClass.copy(scalaBigDecimal = BigDecimal("1.00"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }

      it("should report the differences in a DateTime field") {
        val leftDate = DateTime.now()
        val rightDate = leftDate.plusHours(1)
        val left = templateSimpleCaseClass.copy(jodaDate = leftDate)
        val right = templateSimpleCaseClass.copy(jodaDate = rightDate)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("jodaDate", leftDate.toString(), rightDate.toString())))
      }

      it("should report the differences in a DateTime field will null values") {
        val leftDate = null.asInstanceOf[DateTime]
        val rightDate = DateTime.now()
        val left = templateSimpleCaseClass.copy(jodaDate = leftDate)
        val right = templateSimpleCaseClass.copy(jodaDate = rightDate)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("jodaDate", "null", rightDate.toString())))

        val anotherDiff = comparator.recursiveCompare(right, left)
        assert(anotherDiff.size == 1)
        assert(anotherDiff.contains(ComparisonDiff("jodaDate", rightDate.toString(), "null")))
      }

      it("should report differences in a reference field with null values") {
        val withNull = templateCompositeClass.copy(secondLevel = null)
        val anotherWithNull = withNull.copy()
        val withoutNulls = templateCompositeClass
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(withNull, withoutNulls)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("secondLevel", "null", withoutNulls.secondLevel.toString())))

        val anotherDiff = comparator.recursiveCompare(withoutNulls, withNull)
        assert(anotherDiff.size == 6)
        assert(anotherDiff.contains(ComparisonDiff("secondLevel.innerString", "Roger", "null")))
        assert(anotherDiff.contains(ComparisonDiff("secondLevel.thirdLevel.innerString", "Moore", "null")))
        assert(anotherDiff.contains(ComparisonDiff("secondLevel.thirdLevel.innerLong", "31337", "null")))
        assert(anotherDiff.contains(ComparisonDiff("secondLevel.thirdLevel.seq", "List()", "null")))
        assert(anotherDiff.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite", "List()", "null")))
        assert(anotherDiff.contains(ComparisonDiff("secondLevel.thirdLevel.seqObject", "List()", "null")))

        // No differences for both nulls
        val diffOfBothNulls = comparator.recursiveCompare(withNull, anotherWithNull)
        assert(diffOfBothNulls.isEmpty) // Eventualmente podria devolver los OK para las case class
      }

      it("should report the differences in an Enum field") {
        val left = templateSimpleCaseClass.copy(status = SampleStatusEnum.OK)
        val right = templateSimpleCaseClass.copy(status = SampleStatusEnum.NOK)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("status", "OK", "NOK")))
      }

      it("should report the differences in a Case Object field") {
        val left = templateSimpleCaseClass.copy(direction = SampleDirection.East)
        val right = templateSimpleCaseClass.copy(direction = SampleDirection.West)
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("direction", "East", "West")))
      }

      it("should report the differences in an Optional field") {
        val left = templateSimpleCaseClass.copy(optional = Some("White"))
        val right = templateSimpleCaseClass.copy(optional = Some("Black"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("optional", "White", "Black")))
      }

      it("should report the differences in Seq[String] field") {
        val left = templateSimpleCaseClass.copy(sequence = Seq("a", "b"))
        val right = templateSimpleCaseClass.copy(sequence = Seq("b", "c"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiff("sequence.[]", "a", "")))
        assert(diffs.contains(ComparisonDiff("sequence.[]", "", "c")))
      }

      it("should report the differences in Seq[String] field due to repeated fields (at right)") {
        val left = templateSimpleCaseClass.copy(sequence = Seq("a"))
        val right = templateSimpleCaseClass.copy(sequence = Seq("a", "a"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("sequence.[]", "", "a")))
      }

      it("should report the differences in Seq[String] field due to repeated fields (at left)") {
        val left = templateSimpleCaseClass.copy(sequence = Seq("a", "a"))
        val right = templateSimpleCaseClass.copy(sequence = Seq("a"))
        val comparator = new RecursiveComparator()

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("sequence.[]", "a", "")))
      }
    }

    describe("When there are differences set to be ignored") {
      it("should report ignored difference for a single ignored field") {
        val left = templateSimpleCaseClass.copy(string = "Jimmy")
        val right = templateSimpleCaseClass.copy(string = "Saul")
        val fieldsToIgnore = Seq("string")
        val comparator = new RecursiveComparator(fieldsToIgnore)

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiffIgnored("string", "Jimmy", "Saul")))
      }

      it("should report ignored differences for two ignored fields") {
        val left = templateSimpleCaseClass.copy(string = "Jimmy", int = 1)
        val right = templateSimpleCaseClass.copy(string = "Saul", int = 2)
        val fieldsToIgnore = Seq("string", "int")
        val comparator = new RecursiveComparator(fieldsToIgnore)

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiffIgnored("string", "Jimmy", "Saul")))
        assert(diffs.contains(ComparisonDiffIgnored("int", "1", "2")))
      }

      it("should report ignored differences for an ignored Seq field") {
        val left = templateSimpleCaseClass.copy(sequence = Seq("x", "y", "z"))
        val right = templateSimpleCaseClass.copy(sequence = Seq("r", "s", "t"))
        val fieldsToIgnore = Seq("sequence")
        val comparator = new RecursiveComparator(fieldsToIgnore)

        val diffs = comparator.recursiveCompare(left, right)

        assert(diffs.size == 6)
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "x", "")))
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "y", "")))
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "z", "")))
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "", "r")))
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "", "s")))
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "", "t")))

      }

      it("should report ignored differences for an ignored Seq field.[] (alternate field name with brackets)") {
        val left = templateSimpleCaseClass.copy(sequence = Seq("x", "y", "z"))
        val right = templateSimpleCaseClass.copy(sequence = Seq("r", "s", "t"))
        val fieldsToIgnore = Seq("sequence.[]")
        val comparator = new RecursiveComparator(fieldsToIgnore)

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 6)
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "x", "")))
        assert(diffs.contains(ComparisonDiffIgnored("sequence.[]", "", "t")))
      }

      it("should report ignored differences for an ignored field of a composite object encompassed in a Seq") {
        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqComposite = Seq(Somebody("Jane", 27), Somebody("Archie", 40), Somebody("Carla", 30)))))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqComposite = Seq(Somebody("Jane", 28), Somebody("Archie", 41), Somebody("Carla", 31)))))
        val fieldsToIgnore = Seq("secondLevel.thirdLevel.seqComposite.[].age")
        val comparator = new RecursiveComparator(fieldsToIgnore)

        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 3)
        assert(diffs.size == 3)
        assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
        assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[].age", "27", "28")))
        assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[].age", "40", "41")))
        assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[].age", "30", "31")))
      }
    }
  }

  describe("Two case classes with sub levels (nested classes)") {
    describe("When there are no differences") {
      val left = templateCompositeClass
      val right = templateCompositeClass
      val comparator = new RecursiveComparator()

      it("should report no differences") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.isEmpty)
      }
    }

    describe("When there are differences at sub levels' value properties") {
      val left = templateCompositeClass
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(innerString = "Julianne", thirdLevel = templateThirdLevelCaseClass
            .copy(innerLong = 73313L)))
      val comparator = new RecursiveComparator()

      it("should report differences for nested attributes") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiff("secondLevel.innerString", "Roger", "Julianne")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.innerLong", "31337", "73313")))
      }
    }


    describe("When there are differences in sequences of value objects") {
      val left = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seq = Seq("alfa", "beta", "delta"))))
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seq = Seq("alfa", "beta", "charly"))))
      val comparator = new RecursiveComparator()

      it("should report differences for third-level nested seq(of String) attribute") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seq.[]", "delta", "")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seq.[]", "", "charly")))
      }
    }

    describe("When there are differences in seq of case classes with 1 element") {
      it("should report differences for third-level nested seq(Composite) attribute") {
        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqComposite = Seq(Somebody("John", 30)))))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqComposite = Seq(Somebody("Richard", 50)))))
        val comparator = new RecursiveComparator()
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "Somebody(John,30)", "")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "", "Somebody(Richard,50)")))
      }
    }

    describe("When there are differences in seq of case classes with more elements in actual") {
      val left = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Archie", 40)))))
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Jane", 27), Somebody("Archie", 40), Somebody("Carla", 30)))))
      val comparator = new RecursiveComparator()

      it("should report differences for third-level nested seq(Composite) attribute properly") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "", "Somebody(Jane,27)")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "", "Somebody(Carla,30)")))

      }
    }

    describe("When there are differences in seq of case classes with less elements than actual") {
      val left = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Jane", 27), Somebody("Archie", 40), Somebody("Carla", 30)))))
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Archie", 40)))))
      val comparator = new RecursiveComparator()

      it("should report differences for third-level nested seq(Composite) attribute properly") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 2)
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "Somebody(Jane,27)", "")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "Somebody(Carla,30)", "")))
      }
    }

    describe("When there are differences in seq of case classes with multiple elements (of same size)") {
      val left = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Jane", 27), Somebody("Archie", 40), Somebody("Carla", 30)))))
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Archie", 40), Somebody("Donald", 65), Somebody("Joe", 73)))))
      val comparator = new RecursiveComparator()

      it("should report differences for third-level nested seq(Composite) attribute properly") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 4)
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "Somebody(Jane,27)", "")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "Somebody(Carla,30)", "")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "", "Somebody(Donald,65)")))
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqComposite.[]", "", "Somebody(Joe,73)")))
      }
    }

    describe("When there are differences in seq of objects") {
      val obj1 = templateClassSimple1
      val obj2 = templateClassSimple2
      val obj3 = templateClassSimple3

      val left = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqObject = Seq(obj1, obj2, obj3))))
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqObject = Seq(obj1, obj3))))
      val comparator = new RecursiveComparator()

      it("should report differences for third-level nested Seq(Object) attributes properly") {
        val diffs = comparator.recursiveCompare(left, right)
        assert(diffs.size == 1)
        assert(diffs.contains(ComparisonDiff("secondLevel.thirdLevel.seqObject.[]",
          obj2.toString, "")))
      }
    }

    describe("Two case clases with sub levels (nested classes) and comparator set to ignore fields") {
      describe("When a second level value object attribute is set to be ignored and there is a diff") {
        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(innerString = "Ludwig", thirdLevel = templateThirdLevelCaseClass
              .copy()))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(innerString = "Wolfgang", thirdLevel = templateThirdLevelCaseClass
              .copy()))
        val comparator = new RecursiveComparator(Seq("secondLevel.innerString"))

        it("should report differences as ignored") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.innerString", "Ludwig", "Wolfgang")))
        }
      }

      describe("When a third level value object attribute is set to be ignored and there is a diff") {
        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(innerString = "Jackson", innerLong = 5)))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(innerString = "Jordan", innerLong = 5)))
        val comparator = new RecursiveComparator(Seq("secondLevel.thirdLevel.innerString"))

        it("should report differences as ignored") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.innerString", "Jackson", "Jordan")))
        }
      }

      describe("When a third level seq[String] attribute is set to be ignored and there is a diff") {
        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seq = Seq("Splinter", "Leonardo", "Raphael", "Donatello", "Michelangelo"))))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seq = Seq("Splinter"))))
        val comparator = new RecursiveComparator(Seq("secondLevel.thirdLevel.seq"))

        it("should report differences as ignored") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
          assert(diffs.size == 4)
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seq.[]", "Leonardo", "")))
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seq.[]", "Raphael", "")))
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seq.[]", "Donatello", "")))
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seq.[]", "Michelangelo", "")))
        }
      }

      describe("When a third level seq[Somebody] attribute is set to be ignored and there is a diff") {
        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqComposite = Seq(Somebody("Wes Anderson", 53), Somebody("Joel Coen", 68), Somebody("Christopher Nolan", 52)))))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqComposite = Seq(Somebody("Christopher Nolan", 52), Somebody("Joel Coen", 68)))))
        val comparator = new RecursiveComparator(Seq("secondLevel.thirdLevel.seqComposite"))

        it("should report differences as ignored") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
          assert(diffs.size == 1)
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[]", "Somebody(Wes Anderson,53)", "")))
        }
      }

      describe("When a third level seq[ClassSimple] attribute is set to be ignored and there is a diff") {
        val obj1 = templateClassSimple1
        val obj2 = templateClassSimple2
        val obj3 = templateClassSimple3

        val left = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqObject = Seq(obj1, obj2, obj3))))
        val right = templateCompositeClass
          .copy(secondLevel = templateSecondLevelCaseClass
            .copy(thirdLevel = templateThirdLevelCaseClass
              .copy(seqObject = Seq(obj1, obj3))))
        val comparator = new RecursiveComparator(Seq("secondLevel.thirdLevel.seqObject.[]"))

        it("should report differences as ignored") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
          assert(diffs.size == 1)
          assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqObject.[]",
            obj2.toString, "")))
        }
      }
    }

    describe("Two regular clases with value attributes") {
      describe("When objects match") {
        // Regular Classes don't have a copy() method
        // and clone() fails with stackoverflow exception. Hence use a second object
        // instantiated with same parameters
        val left = templateClassSimple1
        val right = templateClassSimple4 // Copy of 1
        val comparator = new RecursiveComparator()
        it("should report no differences") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.isEmpty)
        }
      }
      describe("When objects don't match") {
        val left = templateClassSimple1
        val right = templateClassSimple2
        val comparator = new RecursiveComparator()
        it("should report the differences") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.size == 2)
          assert(diffs.contains(ComparisonDiff("string", "abc", "def")))
          assert(diffs.contains(ComparisonDiff("jodaDate", someDate.toString(), someOtherDate.toString())))
        }
      }

      describe("When objects don't match but diffs are accounted by ignored fields") {
        val left = templateClassSimple1
        val right = templateClassSimple3
        val comparator = new RecursiveComparator(Seq("string", "int", "long", "boolean", "double", "javaBigDecimal", "scalaBigDecimal"))
        it("should report all differences as ignored") {
          val diffs = comparator.recursiveCompare(left, right)
          assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
          assert(diffs.size == 7)
          assert(diffs.contains(ComparisonDiffIgnored("string", "abc", "ghi")))
          assert(diffs.contains(ComparisonDiffIgnored("int", "111", "222")))
          assert(diffs.contains(ComparisonDiffIgnored("long", "1", "2")))
          assert(diffs.contains(ComparisonDiffIgnored("boolean", "true", "false")))
          assert(diffs.contains(ComparisonDiffIgnored("double", "0.0", "2.0")))
          assert(diffs.contains(ComparisonDiffIgnored("javaBigDecimal", "11.00", "13.00")))
          assert(diffs.contains(ComparisonDiffIgnored("scalaBigDecimal", "12.00", "13.00")))
        }
      }
    }
    ignore("troubleshoot"){
      val left = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Jane", 27), Somebody("Archie", 40), Somebody("Carla", 30)))))
      val right = templateCompositeClass
        .copy(secondLevel = templateSecondLevelCaseClass
          .copy(thirdLevel = templateThirdLevelCaseClass
            .copy(seqComposite = Seq(Somebody("Jane", 28), Somebody("Archie", 41), Somebody("Carla", 31)))))
      val fieldsToIgnore = Seq("secondLevel.thirdLevel.seqComposite.[].age")
      val comparator = new RecursiveComparator(fieldsToIgnore)

      val diffs = comparator.recursiveCompare(left, right)
      assert(diffs.size == 3)
      assert(diffs.forall(d => d.isInstanceOf[ComparisonDiffIgnored]))
      assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[].age", "27", "28")))
      assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[].age", "40", "41")))
      assert(diffs.contains(ComparisonDiffIgnored("secondLevel.thirdLevel.seqComposite.[].age", "30", "31")))
    }
  }
}

case class Simple(string: String,
                  int: Int,
                  boolean: Boolean,
                  long: Long,
                  double: Double,
                  javaBigDecimal: java.math.BigDecimal,
                  scalaBigDecimal: scala.math.BigDecimal,
                  jodaDate: DateTime,
                  status: SampleStatusEnum.SampleStatus,
                  direction: SampleDirection,
                  optional: Option[String],
                  sequence: Seq[String]
                 )

case class Composite(id: Int,
                     boolean: Boolean,
                     secondLevel: SecondLevel
                    )

case class SecondLevel(innerString: String,
                       thirdLevel: ThirdLevel)

case class ThirdLevel(innerString: String, innerLong: Long, seq: Seq[String], seqComposite: Seq[Somebody], seqObject: Seq[ClassSimple])

case class Somebody(name: String, age: Int)


class ClassSimple(string: String,
                  int: Int,
                  boolean: Boolean,
                  long: Long,
                  double: Double,
                  javaBigDecimal: java.math.BigDecimal,
                  scalaBigDecimal: scala.math.BigDecimal,
                  jodaDate: DateTime
                 ) {

  override def toString = s"$string, $int, $boolean, $long, $double, $javaBigDecimal, $scalaBigDecimal, $jodaDate"
}


object SampleStatusEnum extends Enumeration {
  type SampleStatus = Value
  val OK, NOK, PENDING, RUNNING = Value
}

sealed trait SampleDirection

object SampleDirection {
  case object North extends SampleDirection

  case object South extends SampleDirection

  case object East extends SampleDirection

  case object West extends SampleDirection
}


