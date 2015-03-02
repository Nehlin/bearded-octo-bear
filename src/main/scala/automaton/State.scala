package automaton

/**
 * A state is the same as a node. It is defined by its name and index. A graph cannot contain duplicate states, but a
 * graph may contain multiple states with the same name, but different indices.
 *
 * @param n     Name of the state.
 * @param ind   Optional. Index of the state.
 */
class State(n:String, ind:Option[Int]) {
  val nameString = n
  val index = ind

  def name: String = State.name(nameString, index)

  /**
   * Creates a copy of a state. If copyIndex is supplied the new state will have the same name as the old state, but
   * its index will be set to copyIndex. If copyIndex is None, both name and index will be copied from the old state.
   *
   * @param copyIndex   Optional. New index for the state.
   * @return            A copy of the state, possibly with a new index.
   */
  def copyName(copyIndex:Option[Int]): String = copyIndex match {
    case Some(i) => State.name(nameString, copyIndex)
    case _ => name
  }

  override def toString = {
    name
  }

  /**
   * Creates a perfect copy of the state, copying both name and index from the original.
   * @return A copy of the state.
   */
  def copy: State = {
    new State(nameString, index)
  }
}

object State {
  /**
   * Generates a unique string identifier for a state, based on its name and index.
   * @param nameString  Name of the state.
   * @param paramIndex  Optional. Index of the state.
   * @return            Unique string identifier for the state.
   */
  def name(nameString: String, paramIndex: Option[Int]): String = paramIndex match {
    case Some(i) => nameString + "_" + i
    case _ => nameString
  }
}
