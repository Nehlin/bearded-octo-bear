package automaton

/*
 * Transitions are the same as edges in the graph. They consist of three things:
 * - A source, called "from".
 * - A target, called "to".
 * - A condition, which is either send, receive or nop (no operation). Send and receive contains a channel and a
 * message while nop contains nothing.
 *
 * "from" and "to" represents states, yet they only represent the states by name and index. It is the responsibility of
 * the automaton to connect transitions to actual states.
 */

abstract class TransitionCondition(ch: Char) {
  val c = ch
}
case class Send(channel: String, message: String) extends TransitionCondition('S') {
  override def toString = {channel + "!" + message}
}
case class Receive(channel: String, message: String) extends TransitionCondition('R') {
  override def toString = {channel + "?" + message}
}
case class Push(message: String) extends TransitionCondition('U') {
  override def toString = {"push(" + message + ")"}
}
case class Pop(message: String) extends TransitionCondition('O') {
  override def toString = {"pop(" + message + ")"}
}
case class Nop() extends TransitionCondition('N') {
  override def toString = {"Nop"}
}

/**
 * A transition is the same as an edge in the graph. Transitions can be either sends, receives or no operation (nop).
 *
 * @param fName   Name of the source of the transition.
 * @param fIndex  Optional. Index of the source of the transition.
 * @param tName   Name of the target of the transition.
 * @param tIndex  Optional. Index of the target of the transition.
 * @param cond    Condition for the transition.
 */
class Transition(fName: String, fIndex: Option[Int], tName: String, tIndex: Option[Int], cond: TransitionCondition) {

  /**
   * Constructor to create a transition from existing states, ignoring indices of the states (or using states without
   * indices).
   *
   * @param fState  Source-state of the transition.
   * @param tState  Target-state of the transition.
   * @param cond    Condition for the transition.
   */
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

  /**
   * Generates the name of a state having the same name as the source of the transition, but a new index.
   *
   * @param fIndex  The new index for the state name
   * @return        The name of a state with name = the from-name from the transition and index = fIndex.
   */
  def from(fIndex: Int):String = State.name(fromName, Some(fIndex))

  /**
   * @return The name of the source state of the transition
   */
  def from:String = State.name(fromName, fromIndex)

  /**
   * Generates the name of a state having the same name as the target of the transition, but a new index.
   *
   * @param tIndex  The new index for the state name
   * @return        The name of a state with name = the to-name from the transition and index = fIndex.
   */
  def to(tIndex: Int):String = State.name(toName, Some(tIndex))

  /**
   * @return The name of the target state of the transition
   */
  def to:String = State.name(toName, toIndex)

  /**
   * Creates a perfect copy of the transition.
   * @return A copy of the transition.
   */
  def copy: Transition = new Transition(fromName, fromIndex, toName, toIndex, condition)

  def nopCopy: Transition = new Transition(fromName, fromIndex, toName, toIndex, Nop())

  /**
   * Creates a copy of the transition where the indices of the from- and to-states are set to newIndex.
   *
   * @param newIndex  The new index for the the from- and to-states.
   * @return          A copy of the transition with a new index for the states.
   */
  def copy(newIndex: Int): Transition = new Transition(fromName, Some(newIndex), toName, Some(newIndex), condition)

  override def toString = from + " " + cond + "> " + to

  /**
   * A tuple for comparing equality between transitions. Iff t1.compareId == t2.compareId, t1 and t2 represent the
   * same transition.
   *
   * @return the comparison tuple.
   */
  def compareId = (from, to, condition)

  override def equals(o: Any) = o match {
    case t: Transition =>
      t.fromName == fromName &&
      t.fromIndex == fromIndex &&
      t.toName == toName &&
      t.toIndex == toIndex &&
      t.condition.toString == condition.toString
    case _ => false
  }

  override def hashCode = toString.hashCode
}
