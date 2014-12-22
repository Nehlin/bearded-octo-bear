package automaton

class State(n:String) {
  val name = n
  var transitions = scala.collection.mutable.Set[Transition]()

  def addTransition(t:Transition) = transitions += t
  def removeTransition(t:Transition) = transitions -= t
  def removeTransitions() = transitions = scala.collection.mutable.Set[Transition]()
  def sortedTransitions : Array[Transition] = transitions.toArray.sortBy(_.toString)

  override def toString = {
    val nStr = name + " :\n"
    val tStr = (for (t <- transitions) yield "  " + t + "\n").mkString
    nStr + tStr
  }
}
