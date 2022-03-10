import scala.collection.immutable.{AbstractMap, SeqMap, SortedMap}

object map {

  // empty map
  val emap = Map.empty[Int, String]
  // rerieve value for key returns option
  val v: Option[String] = emap.get(1)
  // returns string if default is provided , obviously
  val default: String = emap.getOrElse(1, "default")
  // natural transformation from Map to List and list to Map , they are isomorphic
  val l: List[(Int, String)] = emap.toList
  val m: Map[Int, String] = l.toMap
  // unary operator to convert from map of one type to another using evidence a.k.a runtime proof
  val x: Map[_, _] = ??? //emap.toMap[_, _]
// modify value at key
  val newMap = emap.updated(1, "")
  emap.transform((k, v) => k + v)

}
