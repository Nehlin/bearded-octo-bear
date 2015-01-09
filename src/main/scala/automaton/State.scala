package automaton

class State(n:String, ind:Option[Int]) {
  val nameString = n
  val index = ind

  def name: String = State.name(nameString, index)

  def copyName(copyIndex:Option[Int]): String = copyIndex match {
    case Some(i) => State.name(nameString, copyIndex)
    case _ => name
  }

  override def toString = {
    name
  }

  def copy: State = {
    new State(nameString, index)
  }
}

object State {
  def name(nameString: String, paramIndex: Option[Int]): String = paramIndex match {
    case Some(i) => nameString + "_" + i
    case _ => nameString
  }
}
