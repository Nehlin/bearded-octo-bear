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

// TODO: remove this
class Test(fe: String, te: String, ind: Int) {
  val f = fe
  val t = te
  val index = ind

  override def toString = f + " " + t + " " + index
}

class Transition(fName: String, fIndex: Option[Int], tName: String, tIndex: Option[Int], cond: TransitionCondition) {

  def this(fState: State, tState: State, cond: TransitionCondition) = {
    this(fState.nameString, fState.index, tState.nameString, tState.index, cond)
  }

  def this(fName: String, tName: String, cond: TransitionCondition) = {
    this(fName, None, tName, None, cond)
  }

  val fromName = fName
  val fromIndex = fIndex
  val toName = tName
  val toIndex = tIndex
  val condition = cond

  def from(fIndex: Int) = State.name(fromName, Some(fIndex))
  def from = State.name(fromName, fromIndex)
  def to(tIndex: Int) = State.name(toName, Some(tIndex))
  def to = State.name(toName, toIndex)

  def copy: Transition = new Transition(fromName, fromIndex, toName, toIndex, condition)
  def copy(newIndex: Int): Transition = new Transition(fromName, Some(newIndex), toName, Some(newIndex), condition)

  override def toString = from + " =" + cond + "=> " + to

  /**
   * A string for comparing equality between transitions. Iff t1.compareId == t2.compareId, t1 and t2 represent the
   * same transition.
   *
   * @return the comparison string
   */
  def compareId = (from, to, condition)
}
