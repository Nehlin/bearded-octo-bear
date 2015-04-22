package automaton

import scala.io.Source
import scala.xml.{XML, Elem, Node, NodeSeq}
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
class Automaton(sMap: Map[String, State],
                t: Set[Transition],
                iName: Option[(String, Option[Int])],
                i: Option[Int],
                aName: String) {

  val initialName = iName
  val index = i
  val stateMap = sMap
  val transitions = t
  val automatonName = aName

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

    new Automaton(newStateMap, newTransitions, initialName, index, automatonName)
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
  def outgoingTransitions(state: State): Set[Transition] = {
    transitions.filter(t => t.from == state.name)
  }

  /**
   * Returns all transitions targeting a state.
   * @param state   The target state.
   * @return        All transitions in the automaton ending in the state.
   */
  def incomingTransitions(state: State): Set[Transition] = {
    transitions.filter(t => t.to == state.name)
  }

  /**
   * Returns all transitions starting in a certain state as a sorted list. The actual ordering is  not important, this
   * is used mostly since the ordering of transitions and states affect the generated .dot-files and sorting the
   * transitions according to any ordering guarantees that a certain graph always generate the same .dot-file.
   *
   * @param state   The start-state of the returned transitions
   * @return        All transitions starting in the state, in a sorted list.
   */
  def sortedTransitions(state: State): List[Transition] = {
    outgoingTransitions(state).toList.sortBy(_.toString)
  }

  /**
   * Returns all transitions as a sorted list. The actual ordering is  not important, this is used mostly since the
   * ordering of transitions and states affect the generated .dot-files and sorting the transitions according to any
   * ordering guarantees that a certain graph always generate the same .dot-file.
   *
   * @return All transitions starting in the state, in a sorted list.
   */
  def sortedTransitions: List[Transition] = {
    transitions.toList.sortBy(_.toString)
  }

  /**
   * @return A set of all the transitions in the automaton.
   */
  def states: Set[State] = stateMap.values.toSet

  /**
   * @return A list containing all the states in the automaton, sorted by their names.
   */
  def sortedStates: List[State] = states.toList.sortBy(_.name)

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

    new Automaton(newMap, newTransitions, initialName, newIndex, automatonName)
  }

  /**
   * Depth first search in an automaton. The dfs traverses the graph visiting each state once. The states-list
   * represents the order in which to search the list. Searching will start in the first state of the list and
   * continue until no more states can be found. Once that is done, the next un-discovered state from the list will be
   * traversed. The search is depth first, which means that children are searched before siblings. A map where the
   * discovery- and finnish-time is saved for each state.
   *
   * Optionally, a function may be supplied which is then called for each state as it gets visited. This function gets
   * sent the state where the traversal started that led the dfs to find the state, as well as the current visited
   * state.
   *
   * @param states  The list that decides the order in which to traverse the graph.
   * @param action  Optional. The function that gets called when each state is visited.
   * @return        A map containing the discovery- and finnish-time for each state.
   */
  def dfs (states: List[State],
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
      for (t <- outgoingTransitions(u)) {
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

    reachableStatesFrom(initialState.get)
  }

  /**
   * @param startState the state to use as start-state when finding reachable states.
   * @return All states that are reachable from startState.
   */
  def reachableStatesFrom(startState: State): Set[State] = {
    val ackSet = MSet[State]()
    val initialList = List[State](startState)

    def f (x:State, s:State) : Unit = ackSet.add(s)

    dfs(initialList, Some(f))

    ackSet.toSet
  }

  /**
   * Finds the strongly connected components (scc) of the automaton.
   *
   * @return a set containing the sets of strongly connected components.
   */
  def scc: Set[Set[State]] = {
    // runs dfs on states to get finish-time for all states
    val dfsResult = dfs(states.toList.sortBy(_.name), None)

    // Converts to list of tuple (finish-time, state-name)
    val unorderedNameList = dfsResult.keys.map(s => (dfsResult(s)._2, s.name)).toList

    // Sort the list in descending order by finish-time,
    // remove finish time so list only contains states
    val orderedNameList = unorderedNameList.sortBy(_._1).map(_._2).reverse

    // Create list containing the states from the transposed automaton,
    // ordered as in orderedNameList
    val transposedAutomaton = transpose
    val transposedMap = transposedAutomaton.stateMap
    val orderedStateArr = orderedNameList.map(transposedMap(_))

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

    new Automaton(newStateMap, newTransitions, initialName, index, automatonName)
  }

  /**
   * Removes any unreachable/meaningless states. States are unreachable if they are not reachable from the initial
   * state. States are meaningless if they cannot be a part of any path leading from the initial state to any end
   * state. If the initial state or all end states are removed the automaton is meaningless and None will be returned
   *
   * @param endStates A set of states representing the valid end states.
   * @return An option that contains the trimmed automaton, or None if trimming renders the automaton meaningless.
   */
  def trimmedCopy(endStates: Set[State]): Option[Automaton] = {

    val reachableEndNames = MSet[String]()
    reachableEndNames ++= endStates.map(s => s.name)

    def trimNormal(automaton: Automaton): Option[Automaton] = {
      val trimmedAutomaton = automaton.reachableCopy
      val trimmedStateNames = trimmedAutomaton.states.map(s => s.name)
      reachableEndNames.retain(name => trimmedStateNames.contains(name))
      if (reachableEndNames.nonEmpty) {
        Some(trimmedAutomaton)
      } else {
        None
      }
    }

    def trimTransposed(automaton: Automaton): Option[Automaton] = {
      val startState = automaton.initialState.get
      val transposed = automaton.transpose
      val endStates = reachableEndNames.map(en => transposed.stateMap(en))
      val reachableStates = endStates.map(es => transposed.reachableStatesFrom(es)).flatten

      val newStateMap = reachableStates.map(state => (state.name, state.copy)).toMap

      val newTransitions = transposed.transitions.filter(transition => {
        newStateMap.contains(transition.from) && newStateMap.contains(transition.to)
      }).map(_.copy)

      if (reachableStates.contains(startState)) {
        val trimmedAutomaton = new Automaton(newStateMap, newTransitions, initialName, index, automatonName)
        Some(trimmedAutomaton.transpose)
      } else {
        None
      }
    }

    val forwardTrimmed = trimNormal(this)
    if (forwardTrimmed.isDefined) {
      val doubleTrimmed = trimTransposed(forwardTrimmed.get)
      doubleTrimmed
    } else {
      None
    }
  }

  /**
   * @return A string representation of the automaton
   */
  override def toString = {
    val states = sortedStates

    val headerString = automatonName + (if (automatonName == "") "" else "\n")
    val nameString = initialState match {case Some(n) => "initial state: " + n + "\n" case _ => ""}
    val stateString = states.map(state => {
      state + ":\n" + outgoingTransitions(state).map(t => "  " + t + "\n").mkString
    }).mkString

    headerString + nameString + stateString
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

    new Automaton(newStateMap, newTransitions, newInitialName, newInd, automatonName)
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

    new Automaton(newStateMap, newTransitions, initialName, index, automatonName)
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
   * hard to locate. For further discussion on this, see Util.makeIdentifier
   *
   * State names are basically enumerated and given an index. This index is translated to a string which will be its
   * name. In order to create names that are unique in the entire system (not just unique in the present automaton, but
   * unique among all states in all automatons) an offset to this index can be supplied, and the number of states will
   * also be returned so that this index can be updated for normalising the next automaton.
   *
   * Note that this function will create an automaton without any indices. This function is meant to be called after
   * the automaton is read from the xml.
   *
   * The function will also return a map to translate the normalised names to the old names. As this is a 1 to 1-
   * conversion, the map can easily be reversed to translate old names to normalised.
   *
   * @param startIndex  The offset for the state enumeration.
   * @return            A tuple containing the normalised automaton, the conversion map and the number of states.
   */
  // NOTE: creates an automaton without indices as it should only be used on the initial xml-automaton.
  def normaliseNames(startIndex: Int): (Automaton, Map[String, String], Int) = {
    // Create map with old state name => new state name

    val oldStates = sortedStates
    val range = Stream from startIndex
    val idMap = oldStates.zip(range).map {case (s, ind) => (s.name, Util.makeIdentifier(ind))}.toMap

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

    val normalised = new Automaton(newStateMap, newTransitions, newInitialName, None, automatonName)
    val reversedMap = idMap.map(_.swap)
    (normalised, reversedMap, newStates.size)
  }

  def renameTransitions(channelMap: Map[String, String],
                        messageMap: Map[String, String],
                        stackMap: Map[String, String]): Automaton = {

    val newTransitions = transitions.map(transition => {
      val fromName = transition.fromName
      val fromIndex = transition.fromIndex
      val toName = transition.toName
      val toIndex = transition.toIndex
      val condition = transition.condition match {
        case Send(chn, msg) => Send(channelMap(chn), messageMap(msg))
        case Receive(chn, msg) => Receive(channelMap(chn), messageMap(msg))
        case Push(msg) => Push(stackMap(msg))
        case Pop(msg) => Pop(stackMap(msg))
        case Nop() => Nop()
      }
      new Transition(fromName, fromIndex, toName, toIndex, condition)
    })

    val newStateMap = Automaton.makeStateMap(states.map(_.copy))

    new Automaton(newStateMap, newTransitions, initialName, index, automatonName)
  }
}

object Automaton {

  def makeStateMap(states: Set[State]): Map[String, State] = {
    states.map(state => (state.name, state)).toMap
  }

  def readAutomatonFile(fileName: String): List[(Automaton, List[String], Boolean)] = {

    val allLines = Source.fromFile(fileName).getLines().toList.map(_.trim).filter(l => !l.startsWith("%"))
    // TODO: extract meta data from head
    val lines = allLines.tail

    def splitAutomatons (l: List[String]): List[List[String]] = {
      if (l.contains(",")) {
        val pos = l.indexOf(",")
        val head = l.take(pos)
        val tail = l.slice(pos + 1, l.length)
        head :: splitAutomatons(tail)
      } else {
        List(l)
      }
    }

    val automatonLines = splitAutomatons(lines)
    val automatons = automatonLines.map(lines => {
      val name = lines.head
      val transitionStrings = lines.tail
      val normalTransitions = transitionStrings.filter(_.split(" ").length == 3)
      val startName = transitionStrings.filter(_.startsWith(">")).head.stripPrefix(">").trim
      val endNames = transitionStrings.filter(_.endsWith(">")).map(_.stripSuffix(">").trim)

      val stateNames = transitionStrings.map(_.split(" ")).flatten.filter(!_.contains(">")).distinct.sorted
      val transitions = normalTransitions.map(t => {
        val split = t.split(" ")
        val fromName = split(0)
        val toName = split(2)
        val condStr = split(1).stripSuffix(">")
        val condition = if (condStr.contains('!')) {
          val cSplit = condStr.split('!')
          val chn = cSplit(0)
          val msg = if (cSplit.length > 1) { cSplit(1) } else { "" }
          Send(chn, msg)
        } else if (condStr.contains('?')) {
          val cSplit = condStr.split('?')
          val chn = cSplit(0)
          val msg = if (cSplit.length > 1) { cSplit(1) } else { "" }
          Receive(chn, msg)
        } else if (condStr.contains('+')) {
          val msg = condStr.stripPrefix("+")
          Push(msg)
        } else if (condStr.contains('-')) {
          val msg = condStr.stripPrefix("-")
          Pop(msg)
        } else {
          Nop()
        }
        new Transition(fromName, toName, condition)
      }).toSet

      val (endStateNames, hasEndStates) = if (endNames.nonEmpty) {
        (endNames, true)
      } else {
        (stateNames, false)
      }

      val stateMap = stateNames.map(sName => (sName, new State(sName, None))).toMap
      (new Automaton(stateMap, transitions, Some(startName, None), None, name), endStateNames, hasEndStates)
    })
    automatons
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
    val combined = new Automaton(stateMap, allTransitions,
      automaton1.initialName, automaton2.index, automaton1.automatonName)

    (combined, newTransitions.toSet)
  }

  /**
   * Creates an automaton from a xml-document.
   *
   * @param filename The file name of the xml-document.
   * @return         An automaton matching the data contained in the xml-document.
   */
  def fromXml(filename: String): Automaton = {

    val xml = XML.loadFile(filename)
    val process = (xml \\ "protocol" \\ "role").head
    /**
     * Extracts the name of the initial state from the XML.
     *
     * Since an initial state is required, this method will throw an error if none is found.
     *
     * @param states  The NodeSeq containing the states of the automaton.
     * @return        The name of the initial state.
     */
    def getInitialStateName(states: NodeSeq): String = {
      if (states.isEmpty)
        throw new RuntimeException("No initial state found")
      else if ((states.head \\ "@type").text == "initial")
        states.head.child.text
      else
        getInitialStateName(states.tail)
    }

    def getSpecificRule(path: String,
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

    def getSend = getSpecificRule("post", "send_message")_
    def getReceive = getSpecificRule("pre", "received_message")_

    def getRule(rule: Node): TransitionCondition = {
      val send = getSend(rule)
      val receive = getReceive(rule)
      (send, receive) match {
        case (Some((channel, message)), _) => Send(channel, message)
        case (_, Some((channel, message))) => Receive(channel, message)
        case _ => Nop()
      }
    }

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

    val automatonName = (process \ "@name").toString()
    new Automaton(stateMap, transitions, Some(initialStateName, None), None, automatonName)
  }
}