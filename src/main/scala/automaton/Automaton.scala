package automaton

import scala.xml.{Node, NodeSeq}
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

/**
 * An automaton is the same as a graph. An automaton contains states, and transitions as well as an optional initial
 * state and an an optional index. The states and transitions contain only names, the automaton is responsible for
 * connecting names in transitions to states.
 *
 * @param sMap    A map where referencing each state in the automaton by its name.
 * @param t       The set of transitions in the automaton.
 * @param iName   Optional. The name (and possibly index) of the initial state.
 * @param i       Optional. The index of the automaton.
 */
class Automaton(sMap: Map[String, State], t: Set[Transition], iName: Option[(String, Option[Int])], i: Option[Int]) {

  val initialName = iName
  val index = i
  val stateMap = sMap
  val transitions = t

  /**
   * Returns the index of the initial state.
   *
   * @return Some(index) if there is an indexed initial state. None if not.
   */
  def initialIndex: Option[Int] = initialName match {
    case Some((_, Some(ind))) => Some(ind)
    case _ => None
  }

  /**
   * Returns the initial state.
   *
   * @return Some(state) if there is an initial state. None if not.
   */
  def initialState: Option[State] = initialName match {
    case Some((name, initialInd)) => Some(stateMap(State.name(name, initialInd)))
    case _ => None
  }

  /**
   * Generates a new automaton from the existing automaton with addedTransitions added to the set of transitions.
   *
   * @param addedTransitions  The transitions to add to the new automaton.
   * @return                  A new automaton that contains the states from the current automaton and the transitions
   *                          from the current automaton and addedTransitions.
   */
  def addTransitions(addedTransitions: Set[Transition]): Automaton = {
    val newTransitions = transitions ++ addedTransitions
    val newStates = states.map(_.copy)
    val newStateMap = newStates.map(state => (state.name, state)).toMap

    new Automaton(newStateMap, newTransitions, initialName, index)
  }

  /**
   * Returns the state in the automaton that corresponds to the from-state of a transition.
   *
   * @param transition  the transitions to fetch the from-state from.
   * @return            the state in the automaton matched by the from-state in the transition.
   */
  def from(transition: Transition): State = {
    stateMap(transition.from)
  }

  /**
   * Return the state in the automaton that corresponds to the to-state of a transition.
   *
   * @param transition  the transitions to fetch the from-state from.
   * @return            the state in the automaton matched by the from-state in the transition.
   */
  def to(transition: Transition): State = {
    stateMap(transition.to)
  }

  /**
   * Returns all transitions originating in a state.
   * @param state   The starting state.
   * @return        All transitions in the automaton starting in the state.
   */
  def transitions(state: State): Set[Transition] = {
    transitions.filter(t => t.from == state.name)
  }

  /**
   * Returns the transitions of the automaton starting in a state in an array that is sorted. The actual ordering is
   * not important, this is used mostly since the ordering of transitions and states affect the generated .dot-files
   * and sorting the transitions according to any ordering guarantees that a certain graph always generate the same
   * .dot-file.
   *
   * @param state   The start-state of the returned transitions
   * @return        All transitions starting in the state, in a sorted array.
   */
  def sortedTransitions(state: State) : Array[Transition] = {
    transitions(state).toArray.sortBy(_.toString)
  }

  /**
   * @return A set of all the transitions in the automaton.
   */
  def states: Set[State] = stateMap.values.toSet

  /**
   * @return An array containing all the states in the automaton, sorted by their names.
   */
  def sortedStates: Array[State] = states.toArray.sortBy(_.name)

  /**
   * @return All transitions in the automaton that are of the type Send.
   */
  def sendTransitions: Set[Transition] = {
    transitions.filter(s_t => s_t.condition.isInstanceOf[Send])
  }

  /**
   * @return All transitions in the automaton that are of the type Receive.
   */
  def receiveTransitions: Set[Transition] = {
    transitions.filter(s_t => s_t.condition.isInstanceOf[Receive])
  }

  /**
   * Generates an automaton that is the transpose of the original automaton. The transpose of a graph (which is how
   * automatons are generated) is a graph with the same state but where every edge from A to B is replaced by a similar
   * edge from B to A.
   *
   * @return A new automaton which is the transpose of the original.
   */
  def transpose: Automaton = {
    val newMap = states.map(state => (state.name, state.copy)).toMap
    val newTransitions = transitions.map(transition => {
      new Transition(to(transition), from(transition), transition.condition)
    })
    val newIndex = index

    new Automaton(newMap, newTransitions, initialName, newIndex)
  }

  /**
   * Depth first search in an automaton. The dfs traverses the graph visiting each state once. The states-array
   * represents the order in which to search the array. Searching will start in the first state of the array and
   * continue until no more states can be found. Once that is done, the next un-discovered state from the array will be
   * traversed. The search is depth first, which means that children are searched before siblings. A map where the
   * discovery- and finnish-time is saved for each state.
   *
   * Optionally, a function may be supplied which is then called for each state as it gets visited. This function gets
   * sent the state where the traversal started that led the dfs to find the state, as well as the current visited
   * state.
   *
   * @param states  The array that decides the order in which to traverse the graph.
   * @param action  Optional. The function that gets called when each state is visited.
   * @return        A map containing the discovery- and finnish-time for each state.
   */
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

  /**
   * @return All states that are reachable from the initial state of an automaton.
   */
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

  /**
   * Finds the strongly connected components (scc) of the automaton.
   *
   * @return a set containing the sets of strongly connected components.
   */
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

  /**
   * Creates a copy of the automaton where any unreachable states (states that are not reachable from the initial
   * state) are removed.
   *
   * @return A copy of the automaton containing only the reachable states.
   */
  def reachableCopy: Automaton = {
    val rStates = reachableStates
    val newStateMap = rStates.map(state => (state.name, state.copy)).toMap

    val newTransitions = transitions.filter(transition => {
      newStateMap.contains(transition.from) && newStateMap.contains(transition.to)
    }).map(_.copy)

    new Automaton(newStateMap, newTransitions, initialName, index)
  }

  /**
   * @return A string representation of the automaton
   */
  override def toString = {
    val states = sortedStates

    val nameString = initialState match {case Some(n) => "initial state: " + n + "\n" case _ => ""}
    val stateString = states.map(state => {
      state + ":\n" + transitions(state).map(t => "  " + t + "\n").mkString
    }).mkString

    nameString + stateString
  }

  /**
   * @return a perfect copy of the automaton.
   */
  def copy: Automaton = {
    copy(None)
  }

  /**
   * Creates a copy of the automaton where all states, transitions and the index of the actual automaton is set to a
   * new index.
   *
   * @param newIndex  The index of the copied automaton.
   * @return          A copy of the automaton with a new index.
   */
  def copy(newIndex: Int): Automaton = {
    copy(Some(newIndex))
  }

  /**
   * Creates a copy of the automaton, possibly with a new index. If newIndex is set, all indices of states and
   * transitions as well as the index of the new automaton is set to newIndex. If not, a perfect copy of the automaton
   * is created.
   *
   * @param newIndex  Optional. Possible new index of the automaton.
   * @return          A copy of the automaton, possibly with a new index.
   */
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

  /**
   * Returns all the internal transitions from a subset of the states in the automaton. The internal transitions of a
   * set of states means all transitions that both start and end inside the set.
   *
   * @param states  The set of states to extract the internal transitions from.
   * @return        All transitions of the automaton that both start and end inside the set of states.
   */
  def transitionsInSet(states: Set[State]): Set[Transition] = {
    transitions.filter(t => states.contains(from(t)) && states.contains(to(t)))
  }

  /**
   * Creates a copy of the automaton where all transitions are filtered by a function that removes any transitions that
   * causes the function to return true.
   *
   * @param filterFun   The function that filters the transitions. Should return true for any transition that
   *                    should be removed.
   * @return            The automaton with all unwanted transitions removed.
   */
  private def filterCopy (filterFun: (Transition => Boolean)): Automaton = {

    val newStateMap = stateMap.values.map{state => {
      val ns = new State(state.nameString, index)
      (ns.name, ns)
    }}.toMap
    val newTransitions = transitions.filter(t => !filterFun(t))

    new Automaton(newStateMap, newTransitions, initialName, index)
  }

  /**
   * @return a copy of the automaton containing only send- and nop-transitions
   */
  def sendCopy: Automaton = filterCopy(t => t.condition.isInstanceOf[Receive])

  /**
   * @return a copy of the automaton containing only receive- and nop-transitions
   */
  def receiveCopy: Automaton = filterCopy(t => t.condition.isInstanceOf[Send])

  /**
   * Creates a copy of the automaton where all states are renamed to names that ensures that no name conflicts occur.
   * Note that you should always normalise the names unless the automaton is used to generate example graphs or
   * .dot-files, since the kinds of bugs that could result from not normalising would probably be unpredictable and
   * hard to locate. For further discussion on this, see Automaton.makeIdentifier
   *
   * Note that this function will create an automaton without any indices. This function is meant to be called after
   * the automaton is read from the xml.
   *
   * The function will also return a map to translate the normalised names to the old names. As this is a 1 to 1-
   * conversion, the map can easily be reversed to translate old names to normalised.
   *
   * @return A tuple containing the normalised automaton and the conversion map.
   */
  // NOTE: creates an automaton without indices as it should only be used on the initial xml-automaton.
  def normaliseNames: (Automaton, Map[String, String]) = {
    // Create map with old state name => new state name
    val idMap = sortedStates.zipWithIndex.map {case (s, ind) => (s.name, Automaton.makeIdentifier(ind))}.toMap

    val newStates = states.map(oldState => {
      val newName = idMap(oldState.name)
      val ind = oldState.index
      new State(newName, ind)
    })

    val newStateMap = newStates.map(state => (state.name, state)).toMap
    val newTransitions = transitions.map{case (transition) =>
      val newFrom = idMap(transition.fromName)
      val newTo = idMap(transition.toName)
      new Transition(newFrom, newTo, transition.condition)
    }

    val newInitialName = initialName match {
      case Some((oldName, _)) => Some((idMap(oldName), None))
      case _ => None
    }

    val normalised = new Automaton(newStateMap, newTransitions, newInitialName, None)
    val reversedMap = idMap.map(_.swap)
    (normalised, reversedMap)
  }
}

object Automaton {

  /**
   * Generates a unique and safe state name from an index.
   *
   * States should be renamed to ensure proper behaviour when generating new states. An automaton requires state names
   * to be unique. Some new state names are automatically generated from existing ones and to ensure this does not
   * cause name collisions, the names must follow a certain format. In order not to impose this limitation on the input
   * format, the names are change.
   *
   * An example when this could otherwise cause a problem would be in the k-saturation. Consider a state called A in
   * the XML. While working with generating the final automaton, this becomes a state with index 1. The name of this
   * state would be A_1. Suppose that we k-saturating this state with k = 2, generating the states A0_1,
   * A1_1. If another state from the XML would already be called A_1, A0_1 or A1_1, this would cause duplicate states.
   *
   * @param i Natural number representing the index of a state. Each number creates a unique state name
   * @return  Unique and safe name for the index. Names are of the form A...Z, Aa...Az, Ba...Bz
   */
  def makeIdentifier(i: Int):String = {
    val range = 'a' to 'z'

    if (i < range.length) {
      range(i).toUpper.toString
    } else {
      makeIdentifier(i / range.length - 1) + range(i % range.length)
    }
  }

  /**
   * Extracts the name of the initial state from the XML.
   *
   * Since an initial state is required, this method will throw an error if none is found.
   *
   * @param states  The NodeSeq containing the states of the automaton.
   * @return        The name of the initial state.
   */
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
    if (messageNode.isEmpty || channelNode.isEmpty)
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

  /**
   * Creates an automaton from a role-node in the xml-document.
   *
   * @param process A role-node from the xml-document.
   * @return        An automaton matching the data contained in the node.
   */
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

  /**
   * Combines two automatons into a new automaton. A set of transitions is supplied to tie the two automatons together.
   * Both the automatons need to be indexed and the indices should not be equal for both automatons. The resulting
   * automaton will have its index set to the index of the second automaton.
   *
   * The transitions should not be indexed, or rather the indices will be ignored. A transition from state F to state T
   * will be treated as a transition from state F in automaton1 to state T in automaton2.
   *
   * @param automaton1  First automaton to combine.
   * @param automaton2  Second automaton to combine.
   * @param transitions Transitions linking the two automatons.
   * @return            An automaton that is automaton1 combined with automaton2 linked by the transitions.
   */
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