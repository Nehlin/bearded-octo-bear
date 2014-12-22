package automaton

import scala.xml.{Node, NodeSeq}
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

class Automaton(sMap: Map[String, State], i:Option[Int]) {

  var initialState: State = _
  val index: Option[Int] = i
  val stateMap = sMap

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
      for (t <- u.transitions) {
        val v = t.to
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

  def transitions: Set[(State, Transition)] = {
    val ack = MSet[(State, Transition)]()
    for (s <- stateMap.values) {
      for (t <- s.transitions) {
        ack.add((s, t))
      }
    }
    ack.toSet
  }

  def sendTransitions: Set[(State, Transition)] = {
    transitions.filter(s_t => s_t._2.condition.isInstanceOf[Send])
  }

  def receiveTransitions: Set[(State, Transition)] = {
    transitions.filter(s_t => s_t._2.condition.isInstanceOf[Receive])
  }

  def transpose: Automaton = {
    val automatonCopy = copy
    val oldTransitions = automatonCopy.transitions

    for (s <- automatonCopy.states)
      s.removeTransitions()

    for ((from, transition) <- oldTransitions) {
      val fromName = from.name
      val toName = transition.to.name
      val condition = transition.condition

      val newFrom = automatonCopy.stateMap(toName)
      val newTo = automatonCopy.stateMap(fromName)

      val newTransition = new Transition(newFrom, newTo, condition)
    }
    automatonCopy
  }

  def reachableStates: Set[State] = {
    val ackSet = MSet[State]()
    val initialArray = Array[State](initialState)

    def f (x:State, s:State) : Unit = ackSet.add(s)

    dfs(initialArray, Some(f))

    ackSet.toSet
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
    println(stateMap)
    val transposedMap = transpose.stateMap
    println(transposedMap)
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

    dfs(orderedStateArr, Some(f))
    resultMap.values.map(_.toSet).toSet
  }

  def states: Set[State] = stateMap.values.toSet

  def sortedStates: Array[State] = states.toArray.sortBy(_.name)

  override def toString = {
    val states = sortedStates

    val nameString = "initial state: " + initialState.name + "\n"
    val stateString = (for (state <- states) yield state.toString).mkString

    nameString + stateString
  }

  def copy: Automaton = {
    copy(None)
  }

  def copy(newIndex: Option[Int]): Automaton = {

    def nameFun(oldName: String): String = {
      newIndex match {
        case Some(newI) => Automaton.nameFromIndex(oldName, newI)
        case None => oldName
      }
    }

    val stateNames = states.map(_.name)
    val newStateMap = stateNames.map(name => {
      val newName = nameFun(name)
      (newName, new State(newName))
    }).toMap

    for (oldState <- states) {
      val newState = newStateMap(nameFun(oldState.name))

      for (oldTransition <- oldState.transitions) {
        val oldTransitionToName = oldTransition.to.name
        val oldTransitionCond = oldTransition.condition

        val newStateTo = newStateMap(nameFun(oldTransitionToName))

        new Transition(newState, newStateTo, oldTransitionCond)
      }
    }

    val copyIndex = if(newIndex.isDefined) newIndex else index
    val automatonCopy = new Automaton(newStateMap, copyIndex)

    automatonCopy.initialState = newStateMap(nameFun(initialState.name))
    automatonCopy
  }


  def filterCopy (newIndex: Option[Int], filterFun: (Transition => Boolean)): Automaton = {

    val automatonCopy = copy(newIndex)
    for (state <- automatonCopy.states) {

      for (transition <- state.transitions.filter(filterFun)) {
        state.removeTransition(transition)
      }
    }
    automatonCopy
  }

  def sendCopy: Automaton = filterCopy(None, t => t.condition.isInstanceOf[Receive])
  def receiveCopy: Automaton = filterCopy(None, t => t.condition.isInstanceOf[Send])
  def sendCopy(newIndex: Int): Automaton = filterCopy(Some(newIndex), t => t.condition.isInstanceOf[Receive])
  def receiveCopy(newIndex: Int): Automaton = filterCopy(Some(newIndex), t => t.condition.isInstanceOf[Send])
}

object Automaton {
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
    val stateMap = states.map(s => (s.child.text, new State(s.child.text))).toMap

    val rules = process \\ "rule"
    for (rule <- rules) {

      val fromStateName = (rule \\ "pre" \\ "current_state").head.child.text
      val toStateName = (rule \\ "post" \\ "next_state").head.child.text

      val fromState = stateMap(fromStateName)
      val toState = stateMap(toStateName)

      val tRule = getRule(rule)
      val transition = new Transition(fromState, toState, tRule)
    }

    val automaton = new Automaton(stateMap, None)

    val initialState = stateMap(getInitialStateName(states))
    automaton.initialState = initialState

    automaton
  }

  def transitionsInSet(states: Set[State]): Set[Transition] = {
    val transitions = MSet[Transition]()
    for (state <- states) {
      for (transition <- state.transitions) {
        if (states.contains(transition.to)) {
          transitions.add(transition)
        }
      }
    }
    transitions.toSet
  }

  def nameFromIndex(rawName: String, ind: Int): String = {
    rawName + "_" + ind
  }

  /* NOTE: both automatons need to be indexed. Result automaton index = index of 2nd */
  def combine(automaton1: Automaton,
              automaton2: Automaton,
              transitions: Set[(State, Transition)]): (Automaton, Set[Transition]) = {

    val automaton1copy = automaton1.copy
    val automaton2copy = automaton2.copy
    val states = automaton1copy.states ++ automaton2copy.states
    val stateMap = states.map(state => (state.name, state)).toMap

    val newTransitions = MSet[Transition]()

    if (automaton1copy.index.isEmpty || automaton2copy.index.isEmpty)
      throw new RuntimeException("combined automatons must be indexed.")

    for ((state, transition) <- transitions) {
      val fromName = nameFromIndex(state.name, automaton1copy.index.get)
      val toName = nameFromIndex(transition.to.name, automaton2copy.index.get)

      val fromState = stateMap(fromName)
      val toState = stateMap(toName)
      val condition = transition.condition

      val newTransition = new Transition(fromState, toState, condition)
      newTransitions.add(newTransition)
    }

    val combined = new Automaton(stateMap, automaton2copy.index)
    combined.initialState = automaton1copy.initialState
    (combined, newTransitions.toSet)
  }
}