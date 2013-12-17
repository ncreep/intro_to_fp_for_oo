package ncreep

import scala.collection.mutable.ListBuffer

object ListExamples {
  case class Bug(c: Code)
  case class Feature(c: Code)
  
  val bugs: List[Bug] = List(Bug("null"), Bug("???"), Bug("var x = 3"))
  val features: List[Feature] = bugs map (b => Feature(b.c))
  val features2: List[Feature] = for (b <- bugs) yield Feature(b.c)
  
  val bugsIter = bugs.iterator
  val featureBuffer = ListBuffer[Feature]()
  while(bugsIter.hasNext) {
    featureBuffer += Feature(bugsIter.next().c)
  }
  val features3 = featureBuffer.toList
  
  case class Programmer(assignedBugs: List[Bug])
  
  val p1 = Programmer(List(Bug("null"), Bug("???"), Bug("var x = 3")))
  val p2 = Programmer(List(Bug("foo.asInstanceOf[Bar]"), Bug("1 = 2")))
  val ps = List(p1, p2)
  
  val allFeatures: List[Feature] = ps flatMap (p => p.assignedBugs map (b => Feature(b.c)))
  val allFeatures2: List[Feature] = for {
    p <- ps
    b <- p.assignedBugs
  } yield Feature(b.c)

  def main(args: Array[String]): Unit = {
    println(features)
    println(allFeatures2)
  }
}