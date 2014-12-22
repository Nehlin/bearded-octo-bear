package automaton

abstract class TransitionCondition(ch: Char) {
  val c = ch
}
case class Send(channel: String, message: String) extends TransitionCondition('S') {
  override def toString = {channel + "!" + message}
}
case class Receive(channel: String, message: String) extends TransitionCondition('R') {
  override def toString = {channel + "?" + message}
}
case class Nop() extends TransitionCondition('N') {
  override def toString = {"Nop"}
}

class Transition(f: State, t: State, cond: TransitionCondition) {
  val from = f
  val to = t
  val condition = cond

  f.addTransition(this)

  override def toString = from.name + " =" + cond + "=> " + to.name
}