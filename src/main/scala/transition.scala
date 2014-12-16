package automaton.transition

import automaton.state.State

abstract class TransitionCondition(ch:Char) {
  val c = ch
}
case class Send(channel:String, message:String) extends TransitionCondition('S') {
  override def toString = {channel + "!" + message}
}
case class Receive(channel:String, message:String) extends TransitionCondition('R') {
  override def toString = {channel + "?" + message}
}
case class Nop() extends TransitionCondition('N') {
  override def toString = {"Nop"}
}

class Transition(t:State, cond:TransitionCondition) {
  val to = t
  val condition = cond

  override def toString = condition.c + "(" + cond + ")" + " -> " + to.name
}