package automaton

import scala.xml.{Node, NodeSeq}
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

class Automaton(sMap: Map[String, State], t: Set[Transition], iName: Option[(String, Option[Int])], i: Option[Int]) {

  val initialName = iName
  val index = i
  val stateMap = sMap
  val transitions = t

  def initialIndex: Option[Int] = initialName match {
    case Some((_, Some(ind))) => Some(ind)
    case _ => None
  }

  def initialState: Option[State] = initialName match {
    case Some((name, initialInd)) => Some(stateMap(State.name(name, initialInd)))
    case _ => None
  }

  def addTransitions(addedTransitions: Set[Transition]): Automaton = {
    val newTransitions = transitions ++ addedTransitions
    val newStates = states.map(_.copy)
    val newStateMap = newStates.map(state => (state.name, state)).toMap

    new Automaton(newStateMap, newTransitions, initialName, index)
  }

  def from(transition: Transition): State = {
    stateMap(transition.from)
  }

  def to(transition: Transition): State = {
    stateMap(transition.to)
  }

  def transitions(state: State): Set[Transition] = {
    transitions.filter(t => t.from == state.name)
  }

  def sendTransitions: Set[Transition] = {
    transitions.filter(s_t => s_t.condition.isInstanceOf[Send])
  }

  def receiveTransitions: Set[Transition] = {
    transitions.filter(s_t => s_t.condition.isInstanceOf[Receive])
  }

  def transpose: Automaton = {
    val newMap = states.map(state => (state.name, state.copy)).toMap
    val newTransitions = transitions.map(transition => {
      new Transition(to(transition), from(transition), transition.condition)
    })
    val newIndex = index

    new Automaton(newMap, newTransitions, initialName, newIndex)
  }

  def dfs (states: Array[State],
           action: Option[((State, State)=>Unit)]): Map[State, (Int, Int)] = {

    var time = 0
    val seen = MSet[State]()
    val discovered = MMap[State, Int]()
    val finished = MMap[State, Int]()

    def visit(startState: State, u: State): Unit = {

      if (action.isDefined)
        action.get(startState, u)

      seen.add(u)
      time += 1
      discovered(u) = time
      for (t <- transitions(u)) {
        val v = to(t)
        if (!seen.contains(v)) {
          visit(startState, v)
        }
      }
      time += 1
      finished(u) = time
    }

    for (state <- states) {
      if (!seen(state))
        visit(state, state)
    }

    states.map(s => (s, (discovered(s), finished(s)))).toMap
  }

  def reachableStates: Set[State] = {
    if (initialState.isEmpty) {
      throw new RuntimeException("No initial state specified when searching for reachable states.")
    }

    val ackSet = MSet[State]()
    val initialArray = Array[State](initialState.get)

    def f (x:State, s:State) : Unit = ackSet.add(s)

    dfs(initialArray, Some(f))

    ackSet.toSet
  }

  def reachableCopy: Automaton = {
    val rStates = reachableStates
    val newStateMap = rStates.map(state => (state.name, state.copy)).toMap

    val newTransitions = transitions.filter(transition => {
      newStateMap.contains(transition.from) && newStateMap.contains(transition.to)
    }).map(_.copy)

    new Automaton(newStateMap, newTransitions, initialName, index)
  }

  def scc: Set[Set[State]] = {
    // runs dfs on states to get finish-time for all states
    val dfsResult = dfs(states.toArray.sortBy(_.name), None)

    // Converts to array of tuple (finish-time, state-name)
    val unorderedNameArr = dfsResult.keys.map(s => (dfsResult(s)._2, s.name)).toArray

    // Sort the array in descending order by finish-time,
    // remove finish time so array only contains states
    val orderedNameArr = unorderedNameArr.sortBy(_._1).map(_._2).reverse

    // Create array containing the states from the transposed automaton,
    // ordered as in orderedNameArr
    val transposedAutomaton = transpose
    val transposedMap = transposedAutomaton.stateMap
    val orderedStateArr = orderedNameArr.map(transposedMap(_))

    val resultMap = MMap[String, MSet[State]]()
    def f(startState:State, state:State) : Unit = {
      val startName = startState.name

      if (!resultMap.contains(startName))
        resultMap(startName) = MSet[State]()

      // Add state from the original state, not the transposed one
      val originalState = stateMap(state.name)
      resultMap(startName).add(originalState)
    }

    transposedAutomaton.dfs(orderedStateArr, Some(f))

    resultMap.values.map(_.toSet).toSet
  }

  def states: Set[State] = stateMap.values.toSet

  def sortedStates: Array[State] = states.toArray.sortBy(_.name)
  def sortedTransitions(state: State) : Array[Transition] = {
    transitions(state).toArray.sortBy(_.toString)
  }

  override def toString = {
    val states = sortedStates

    val nameString = initialState match {case Some(n) => "initial state: " + n + "\n" case _ => ""}
    val stateString = states.map(state => {
      state + ":\n" + transitions(state).map(t => "  " + t + "\n").mkString
    }).mkString

    nameString + stateString
  }

  def copy: Automaton = {
    copy(None)
  }

  def copy(newIndex: Int): Automaton = {
    copy(Some(newIndex))
  }

  // If newIndex is set, all states and the automaton will have index = newIndex
  // otherwise, they copy the (possibly empty) index from the previous automaton
  private def copy(newIndex: Option[Int]): Automaton = {

    val newStates = states.map(oldState => {
      newIndex match {
        case Some(ind) => new State(oldState.nameString, Some(ind))
        case _ => oldState.copy
      }
    })

    val newStateMap = newStates.map(state => (state.name, state)).toMap
    val newTransitions = newIndex match {
      case Some(nInd) => transitions.map(_.copy(nInd))
      case _ => transitions.map(_.copy)
    }

    val newInd = if (newIndex.isDefined) newIndex else index

    val newInitialName = (newIndex, initialName) match {
      case (Some(newI), Some((name, _))) => Some(name, Some(newI))
      case _ => initialName
    }

    new Automaton(newStateMap, newTransitions, newInitialName, newInd)
  }

  def transitionsInSet(states: Set[State]): Set[Transition] = {
    transitions.filter(t => states.contains(from(t)) && states.contains(to(t)))
  }

  def filterCopy (newIndex: Option[Int], filterFun: (Transition => Boolean)): Automaton = {

    val newInd = newIndex.orElse(index)
    val newStateMap = stateMap.values.map{state => {
      val ns = new State(state.nameString, newInd)
      (ns.name, ns)
    }}.toMap
    val newTransitions = transitions.filter(t => !filterFun(t))

    new Automaton(newStateMap, newTransitions, initialName, newInd)
  }

  def sendCopy: Automaton = filterCopy(None, t => t.condition.isInstanceOf[Receive])
  def receiveCopy: Automaton = filterCopy(None, t => t.condition.isInstanceOf[Send])
  def sendCopy(newIndex: Int): Automaton = filterCopy(Some(newIndex), t => t.condition.isInstanceOf[Receive])
  def receiveCopy(newIndex: Int): Automaton = filterCopy(Some(newIndex), t => t.condition.isInstanceOf[Send])
}

object Automaton {

  def makeStateMap(states: Set[State]): Map[String, State] = {
    states.map(state => (state.name, state)).toMap
  }

  private def getInitialStateName(states: NodeSeq): String = {
    if (states.isEmpty)
      throw new RuntimeException("No initial state found")
    else if ((states.head \\ "@type").text == "initial")
      states.head.child.text
    else
      getInitialStateName(states.tail)
  }

  private def getSpecificRule(path: String,
                              messageName: String)
                             (rule:Node): Option[(String, String)] = {

    val post = (rule \ path).head
    val messageNode = post \ messageName
    val channelNode = post \ "channel"
    if (messageNode.isEmpty || messageNode.isEmpty)
      None
    else {
      val message = messageNode.head.text
      val channel = channelNode.head.text
      Some(channel, message)
    }
  }

  private def getSend = getSpecificRule("post", "send_message")_
  private def getReceive = getSpecificRule("pre", "received_message")_

  private def getRule(rule: Node): TransitionCondition = {
    val send = getSend(rule)
    val receive = getReceive(rule)
    (send, receive) match {
      case (Some((channel, message)), _) => Send(channel, message)
      case (_, Some((channel, message))) => Receive(channel, message)
      case _ => Nop()
    }
  }

  def fromXml(process: Node): Automaton = {

    val states = process \ "states" \ "state"
    val stateMap = states.map(s => (s.child.text, new State(s.child.text, None))).toMap

    val rules = process \\ "rule"
    val transitions = rules.map(rule => {
      val fromStateName = (rule \\ "pre" \\ "current_state").head.child.text
      val toStateName = (rule \\ "post" \\ "next_state").head.child.text

      val fromState = stateMap(fromStateName)
      val toState = stateMap(toStateName)

      val tRule = getRule(rule)
      new Transition(fromState, toState, tRule)
    }).toSet

    val initialStateName = getInitialStateName(states)

    new Automaton(stateMap, transitions, Some(initialStateName, None), None)
  }


  /* NOTE: both automatons need to be indexed. Result automaton index = index of 2nd
   * NOTE: transition source for the transitions between automaton1 and automaton2 are
   * the states in automaton1 where (nameString, index) are the nameStrings from
   * 'transitions.State' and index is from automaton1.index */
  def combine(automaton1: Automaton,
              automaton2: Automaton,
              transitions: Set[Transition]): (Automaton, Set[Transition]) = {


    val states = automaton1.states ++ automaton2.states
    val stateMap = states.map(state => (state.name, state)).toMap

    val newTransitions = MSet[Transition]()

    if (automaton1.index.isEmpty || automaton2.index.isEmpty)
      throw new RuntimeException("combined automatons must be indexed.")

    val index1 = automaton1.index.get
    val index2 = automaton2.index.get

    for (transition <- transitions) {
      val fromName = transition.from(index1)
      val toName = transition.to(index2)

      val fromState = stateMap(fromName)
      val toState = stateMap(toName)
      val condition = transition.condition

      val newTransition = new Transition(fromState, toState, condition)
      newTransitions.add(newTransition)
    }

    val allTransitions = automaton1.transitions ++ automaton2.transitions ++ newTransitions
    val combined = new Automaton(stateMap, allTransitions, automaton1.initialName, automaton2.index)

    (combined, newTransitions.toSet)
  }
}