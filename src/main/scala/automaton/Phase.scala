package automaton

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}


object Phase {

  def makeCondensed(automaton: Automaton, p:Int, saturationValues: List[Int]): Automaton = {
    val (combined, _) = makeCondensedPrint(automaton, p, saturationValues)
    combined
  }

  def makeCondensedPrint(automaton: Automaton,
                         p:Int,
                         saturationValues: List[Int]): (Automaton, Set[Transition]) = {

    if (p > saturationValues.length * 2) {
      throw new RuntimeException("List of saturation values is too short.\n" +
        "The list needs to contain an integer for every send-phase.")
    }

    val receivePart = automaton.receiveCopy
    val (sendPart, translationMap) = condenseSendAutomaton(automaton.sendCopy)

    // Used for transitions between phases, updated to target condensed automaton.
    val sendTransitions = automaton.sendTransitions.map(st => {
      val toName = translationMap(st.to)

      new Transition(st.from, toName, st.condition)
    })
    val receiveTransitions = automaton.receiveTransitions.map(rt => {
      val fromName = translationMap(rt.from)

      new Transition(fromName, rt.to, rt.condition)
    })

    val (saturatedSend, saturationEndStates) = kSaturate(saturationValues.head, sendPart.copy(0))
    addReceiveCondensed(saturatedSend, Set(), sendPart, saturationEndStates,
      receivePart, sendTransitions, receiveTransitions, 1, p, saturationValues.tail)
  }

  def addReceiveCondensed(combined: Automaton,
                          combinedTransitions: Set[Transition],
                          condensedSendPart: Automaton,
                          saturationEndStates: Map[String, String],
                          receivePart: Automaton,
                          sendTransitions: Set[Transition],
                          receiveTransitions: Set[Transition],
                          phase: Int,
                          numPhases: Int,
                          saturationValues: List[Int]): (Automaton, Set[Transition]) = {

    if (phase > numPhases) {
      return (combined, combinedTransitions)
    }

    val translatedCombineTransitions = receiveTransitions.map(transition => {
      val fromName = saturationEndStates.getOrElse(transition.fromName, transition.fromName)
      val toName = transition.toName
      new Transition(fromName, toName, transition.condition)
    })

    val (newCombined, newTransitions) = Automaton.combine(combined,
      receivePart.copy(phase), translatedCombineTransitions)

    val allTransitions = combinedTransitions ++ newTransitions
    addSendCondensed(newCombined, allTransitions, condensedSendPart, receivePart, sendTransitions,
      receiveTransitions, phase + 1, numPhases, saturationValues)
  }

  def addSendCondensed(combined: Automaton,
                       combinedTransitions: Set[Transition],
                       condensedSendPart: Automaton,
                       receivePart: Automaton,
                       sendTransitions: Set[Transition],
                       receiveTransitions: Set[Transition],
                       phase: Int,
                       numPhases: Int,
                       saturationValues: List[Int]): (Automaton, Set[Transition]) = {

    if (phase >= numPhases) {
      return (combined, combinedTransitions)
    }

    val (saturatedSend, saturationEndStates) =
      kSaturate(saturationValues.head, condensedSendPart.copy(phase))

    val (newCombined, newTransitions) = Automaton.combine(combined, saturatedSend, sendTransitions)
    val allTransitions = combinedTransitions ++ newTransitions

    addReceiveCondensed(newCombined, allTransitions, condensedSendPart, saturationEndStates, receivePart,
      sendTransitions, receiveTransitions, phase + 1, numPhases, saturationValues.tail)
  }

  /**
   * TODO: Figure out if transitions between condensed states are unique. Consider transitions A-x>C and B-x>D when
   * combining (AB) and (CD). verify that there is one, not two transitions AB-x>CD
   *
   * Condenses a send automaton by combining its strongly connected components to single states. For more information
   * on condensing, refer to the external documentation
   * 
   * @param automaton the send part of an automaton
   * @return (condensed, map)
   *         condensed  automaton where every strongly connected component has been replaced by a single state with
   *                    self-loops
   *         map        a map translating names in 'automaton' to names in 'condensed'
   */
  def condenseSendAutomaton(automaton: Automaton): (Automaton,
    Map[String, String]) = {

    val index = automaton.index

    val translationMap = MMap[String, String]()
    val combinedMap = MMap[String, State]()
    val combinedTransitions = MSet[Transition]()
    val existingTransitions = MSet[(String, String, TransitionCondition)]()
    var initialStateName: Option[(String, Option[Int])] = None

    def constructNewComponent(component:Set[State]): (Set[Transition]) = {
      val stateNames = component.map(_.name)
      val combinedName = stateNames.toArray.sorted.mkString
      val combinedState = new State(combinedName, index)

      for (oldName <- stateNames) {
        translationMap(oldName) = combinedState.nameString
      }
      combinedMap(combinedName) = combinedState

      val oldTransitions = component.map(state => automaton.transitions(state)).flatten

      if (automaton.initialState.isDefined && component.contains(automaton.initialState.get)) {
        initialStateName = Some(combinedState.name, index)
      }
      oldTransitions
    }

    // NOTE: must be called after all calls of constructNewComponent
    def translateTransitions(transitions: (Set[Transition])): Unit = {

      for (transition <- transitions) {
        val newFrom = translationMap(transition.from)
        val newTo = translationMap(transition.to)
        val newCondition = transition.condition

        val translatedTransition = new Transition(newFrom, newTo, newCondition)
        val compareId = translatedTransition.compareId

        if (!existingTransitions.contains(compareId) &&
          !(newFrom == newTo && newCondition == Nop())) {
          existingTransitions += compareId
          combinedTransitions += translatedTransition
        }
      }
    }

    val transitions = automaton.scc.map(constructNewComponent)
    transitions.foreach(translateTransitions)

    val combinedAutomaton = new Automaton(combinedMap.toMap, combinedTransitions.toSet, initialStateName, index)

    (combinedAutomaton, translationMap.toMap)
  }

  /**
   * K-saturates an entire automaton. This assumes that the automaton is already condensed. For an explanation of
   * k-saturation, refer to the external documentation
   * 
   * @param k number of added transition steps
   * @param automaton automaton to k-saturate
   * @return (saturated, map)
   *         saturated  a k-saturated version of 'automaton'
   *         map        a map that translates the states in 'automaton' to the end states in 'saturated' for every
   *                    state in 'automaton' that has been k-saturated
   */
  def kSaturate(k: Int, automaton: Automaton): (Automaton, Map[String, String]) = {

    val saturationStates = MSet[State]()
    val saturationTransitions = MSet[Transition]()
    val newEndStates = MMap[String, String]()

    // k-saturate all states with loops
    for (state <- automaton.states) {
      val transitions = automaton.transitions(state)
      val internalConditions = transitions.filter(_.to == state.name).map(_.condition)
      val externalTransitions = transitions.filter(_.to != state.name)

      // This state has no loops, no k-saturation needed
      if (internalConditions.isEmpty) {
        for (transition <- externalTransitions) {
          saturationTransitions += transition
        }
        // This state has loops and out-going transitions,
        // k-saturate each transition, do not include original transition
      } else if (externalTransitions.nonEmpty) {
        val (newStates, newTransitions, endName) =
          kSaturateState(k, automaton.index, externalTransitions, internalConditions)

        newEndStates(state.nameString) = endName
        saturationStates ++= newStates
        saturationTransitions ++= newTransitions
      }
    }

    val oldStateCopy = automaton.states.map(_.copy)
    val newStates = oldStateCopy ++ saturationStates
    (new Automaton(Automaton.makeStateMap(newStates),
      saturationTransitions.toSet, automaton.initialName, automaton.index), newEndStates.toMap)
  }

  /**
   * Creates all new states and transitions necessary to k-saturate a single state. After creating the new transition
   * all previous transitions for the state should be disregarded. For an explanation of k-saturation, refer to 
   * the external documentation
   *
   * @param k number of added transition steps
   * @param automatonIndex index of the state to saturate
   * @param transitions all outgoing external (non self-looping) transitions from the state
   * @param conditions all conditions that exists in self loops in the state
   * @return (states, transitions, endState)
   *         states       the states forming the k-saturation chain
   *         transitions  all transition for linking to, inside, and from the chain
   *         endState     the name of the end state in the chain
   */
  def kSaturateState(k: Int,
                     automatonIndex: Option[Int],
                     transitions: Set[Transition],
                     conditions: Set[TransitionCondition]) : (Set[State], Set[Transition], String) = {

    // Sets enforce uniqueness, so this is safe even if Nop is already a part of conditions
    val conditionsPlusNop = conditions ++ Set(new Nop())

    // Since all transitions target the same state, just pick any state for from/to-values
    val anyTransition = transitions.toArray.head
    val fromName = anyTransition.fromName
    val fromIndex = anyTransition.fromIndex

    // Removes duplicate conditions since sets enforce uniqueness
    val transitionConditions = transitions.map(transition =>
      (transition.condition, transition.toName, transition.toIndex)).toSet

    // Naming function for the intermediate states
    def tmpName(index: Int) = fromName + index

    // Create k intermediate states
    val tmpStates = (0 to k).map(i => new State(tmpName(i), automatonIndex))

    // Add a transition from the original state to the first intermediate state
    val newTransitions = MSet[Transition]()
    val firstTemp = tmpStates.head
    newTransitions += new Transition(fromName, fromIndex, firstTemp.nameString, firstTemp.index, Nop())

    // For every condition c and every intermediate state s[n], s[n+1],
    // add a transition with condition c from s[n] to s[n+1]
    newTransitions ++= (0 to k-1).map(i => {
      conditionsPlusNop.map(condition => {
        val fromState = tmpStates(i)
        val toState = tmpStates(i+1)
        new Transition(fromState, toState, condition)
      })
    }).flatten

    // Add outgoing transitions to the final intermediate state matching
    // the outgoing transitions from the original state
    val finalState = tmpStates(k)
    for ((condition, toName, toIndex) <- transitionConditions) {
      newTransitions += new Transition(finalState.nameString, finalState.index, toName, toIndex, condition)
    }

    (tmpStates.toSet, newTransitions.toSet, tmpName(k))
  }





  // === Phases without condensing or k-saturation. For illustrative purposes only ===

  def makePhases(automaton: Automaton, p: Int): Automaton = {
    val (combined, _) = makePhasesForPrinting(automaton, p)

    combined
  }

  def makePhasesForPrinting(automaton: Automaton, p:Int): (Automaton, Set[Transition]) = {
    val sendPart = automaton.sendCopy
    val receivePart = automaton.receiveCopy
    val sendTransitions = automaton.sendTransitions
    val receiveTransitions = automaton.receiveTransitions

    addReceive(sendPart.copy(0), Set(), sendPart, receivePart, sendTransitions, receiveTransitions, 1, p)
  }

  def addSend(combined: Automaton,
              addedTransitions: Set[Transition],
              sendPart: Automaton,
              receivePart: Automaton,
              sendTransitions: Set[Transition],
              receiveTransitions: Set[Transition],
              phase: Int,
              numPhases: Int): (Automaton, Set[Transition]) = {

    if (phase > numPhases) {
      (combined, addedTransitions)
    } else {
      val (newCombined, newTransitions) = Automaton.combine(combined, sendPart.copy(phase), sendTransitions)
      val newAddedTransitions = addedTransitions ++ newTransitions

      addReceive(newCombined, newAddedTransitions, sendPart, receivePart, 
        sendTransitions, receiveTransitions, phase + 1, numPhases)
    }
  }

  def addReceive(combined: Automaton,
                 addedTransitions: Set[Transition],
                 sendPart: Automaton,
                 receivePart: Automaton,
                 sendTransitions: Set[Transition],
                 receiveTransitions: Set[Transition],
                 phase: Int,
                 numPhases: Int): (Automaton, Set[Transition]) = {

    if (phase > numPhases) {
      (combined, addedTransitions)
    } else {
      val (newCombined, newTransitions) = Automaton.combine(combined, receivePart.copy(phase), receiveTransitions)
      val newAddedTransitions = addedTransitions ++ newTransitions

      addSend(newCombined, newAddedTransitions, sendPart, receivePart, 
        sendTransitions, receiveTransitions, phase + 1, numPhases)
    }
  }
}