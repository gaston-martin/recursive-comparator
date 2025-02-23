# recursive-comparator

This is a recursive comparator for objects in Scala

The main idea was to get the same info from two services in which the later service is a new version of the former

After requesting the same entity from both services, a comparison could be made to make sure the new service holds backwards compatibility with the original one. 

This has been part of a greater project which I can't disclose. 

The RecursiveComparatorTest class shows how tu use the comparator (RecursiveComparator)
