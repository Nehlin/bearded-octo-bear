package yautomaton.state

import dk.brics.automaton.State

class YState(n:String) extends State {
  val name = n

  def vrok = {
    "sjab"
  }

  override def toString = {
    var str = name + " "
    str = str + (if (isAccept) "[accept]" else "[reject]")
    str = str + ":\n"
    val transitions = (for (t <- getTransitions.toArray) yield "  " + t + "\n").mkString
    str + transitions
  }
}

object YState {
  def gnorbo = "yee-ha, 1-2-3"
}