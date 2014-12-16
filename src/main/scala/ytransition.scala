package yautomaton.transition

import yautomaton.state.YState
import dk.brics.automaton.Transition

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

class YTransition(to:YState, cond:TransitionCondition) extends Transition(cond.c, to) {
  val condition = cond

  override def toString = condition.c + "(" + cond + ")" + " -> " + to.name
}
