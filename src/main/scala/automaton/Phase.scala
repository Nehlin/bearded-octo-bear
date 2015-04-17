package automaton

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

/**
 * p is (state + 1)
 */


object Phase {

  // TODO: make private
  def noTransform(automaton: Automaton) = {
    automaton
  }

  /**
   * Creates an automaton that enforces send- and receive-phases. The created automaton will have its send-phases
   * condensed and will be saturated. Too understand condensing and saturation, please refer to the paper.
   *
   * @param automaton         The original automaton.
   * @param p                 Number of phases.
   * @return                  A condensed, saturated automaton with send- and receive-phases as well as the map for
   *                          converting condensed send names to un-condensed and the map for finding the end states of
   *                          the saturation chains.
   */
  def makeCondensed(automaton: Automaton, p:Int, saturationValues: List[Int]):
    (Automaton, Map[String, String], List[(Int, Map[String, String])]) = {

    val (combined, _, sendMap, endMap) = makeCondensedPrint(automaton, p, saturationValues)
    (combined, sendMap, endMap)
  }

  def makeCondensedPrintWithTransforms(automaton: Automaton,
                                       p: Int,
                                       saturationValues: List[Int],
                                       fullConverter: Automaton => Automaton,
                                       sendConverter: Automaton => Automaton,
                                       receiveConverter: Automaton => Automaton):
  (Automaton, Set[Transition], Map[String, String], List[(Int, Map[String, String])]) = {

    val convertedAutomaton = fullConverter(automaton)

    val rawSend = convertedAutomaton.sendCopy
    val rawReceive = convertedAutomaton.receiveCopy

    val convertedSend = sendConverter(rawSend)
    val convertedReceive = receiveConverter(rawReceive)

    val receivePart = convertedReceive
    val (sendPart, translationMap) = condenseSendAutomaton(convertedSend)

    // Used for transitions between phases, updated to target condensed automaton.
    val createdTransitions = MSet[(String, String, TransitionCondition)]()
    val sendTransitions = automaton.sendTransitions.map(st => {
      val fromName = st.from
      val toName = translationMap(st.to)
      val condition = st.condition

      if (createdTransitions.contains((fromName, toName, condition))) {
        None
      } else {
        createdTransitions.add((fromName, toName, condition))
        Some(new Transition(st.from, toName, st.condition))
      }
    }).flatten
    val receiveTransitions = automaton.receiveTransitions.map(rt => {
      val fromName = translationMap(rt.from)

      new Transition(fromName, rt.to, rt.condition)
    })

    val saturationValue = saturationValues.head

    val (saturatedSend, saturationEndStates) = kSaturate(saturationValue, sendPart.copy(0), receiveTransitions)
    addReceiveCondensed(saturatedSend, Set(), sendPart, saturationEndStates,
      receivePart, sendTransitions, receiveTransitions, 1, p, saturationValues.tail, translationMap, List((0, saturationEndStates)))
  }

  /**
   * Creates an automaton that enforces send- and receive-phases. The created automaton will have its send-phases
   * condensed and will be saturated. Too understand condensing and saturation, please refer to the paper. Also returns
   * the set of created transitions between phases for easy highlighting when printing to .dot-format.
   *
   * @param automaton         The original automaton.
   * @param p                 Number of phases.
   * @return                  A tuple containing a condensed, saturated automaton with send- and receive-phases and the
   *                          set of transitions between phases, as well as the translation map for the phases and a
   *                          map for the end states from saturation chains.
   */
  def makeCondensedPrint(automaton: Automaton,
                         p:Int,
                         saturationValues: List[Int]): (Automaton, Set[Transition], Map[String, String], List[(Int, Map[String, String])]) = {

    makeCondensedPrintWithTransforms(automaton, p, saturationValues, noTransform, noTransform, noTransform)
  }

  def getSaturationValuesWithTransforms(automatons: List[Automaton],
                                        endStateNames: List[String],
                                        p: Int,
                                        fullConverter: Automaton => Automaton,
                                        sendConverter: Automaton => Automaton,
                                        receiveConverter: Automaton => Automaton): List[Int] = {

    val l = List.fill(p)(0)
    val res = automatons.map(auto =>
      makeCondensedPrintWithTransforms(auto, p, l, fullConverter, sendConverter, receiveConverter)
    )

    val endStates = res.map{case (auto, _, sendMap, endStateMaps) =>
      Files.write(Paths.get(auto.automatonName + "_original.dot"),
        Dot.make(auto).getBytes(StandardCharsets.UTF_8))
      endStatesForPhased(endStateNames, auto, sendMap, endStateMaps)
    }

    val condensedAutomatons = res.map{case (auto, _, _, _) => auto}
    val trimmedAutomatons = (condensedAutomatons, endStates).zipped.map((cAuto, eStates) =>
      cAuto.trimmedCopy(eStates)
    ).flatten

    val states = trimmedAutomatons.foldRight[Set[State]](Set[State]())((auto, ack) => ack ++ auto.states)

    // Counts number or states in each receive-phase
    val numStatesPerReceivePhase = states.groupBy(state => state.index.get).toList.
      filter{case (ind, _) => ind % 2 == 1}.sortBy{case (ind, _) => ind}.
      map{case (_, state) => state.size}

    val saturationValues = numStatesPerReceivePhase.tails.toList.init.map(a => a.sum)

    saturationValues
  }

  private def addReceiveCondensed(combined: Automaton,
                          combinedTransitions: Set[Transition],
                          condensedSendPart: Automaton,
                          saturationEndStates: Map[String, String],
                          receivePart: Automaton,
                          sendTransitions: Set[Transition],
                          receiveTransitions: Set[Transition],
                          phase: Int,
                          numPhases: Int,
                          saturationValues: List[Int],
                          sendMap: Map[String, String],
                          endStatesCombined: List[(Int, Map[String, String])])
  : (Automaton, Set[Transition], Map[String, String], List[(Int, Map[String, String])]) = {

    if (phase > numPhases) {
      return (combined, combinedTransitions, sendMap, endStatesCombined)
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
      receiveTransitions, phase + 1, numPhases, saturationValues, sendMap, endStatesCombined ++ List((phase, Map[String, String]())))
  }

  private def addSendCondensed(combined: Automaton,
                       combinedTransitions: Set[Transition],
                       condensedSendPart: Automaton,
                       receivePart: Automaton,
                       sendTransitions: Set[Transition],
                       receiveTransitions: Set[Transition],
                       phase: Int,
                       numPhases: Int,
                       saturationValues: List[Int],
                       sendMap: Map[String, String],
                       endStatesCombined: List[(Int, Map[String, String])])
  : (Automaton, Set[Transition], Map[String, String], List[(Int, Map[String, String])]) = {

    if (phase >= numPhases) {
      return (combined, combinedTransitions, sendMap, endStatesCombined)
    }

    val saturationValue = saturationValues.head

    val (saturatedSend, saturationEndStates) =
      kSaturate(saturationValue, condensedSendPart.copy(phase), receiveTransitions)

    val (newCombined, newTransitions) = Automaton.combine(combined, saturatedSend, sendTransitions)
    val allTransitions = combinedTransitions ++ newTransitions

    addReceiveCondensed(newCombined, allTransitions, condensedSendPart, saturationEndStates, receivePart,
      sendTransitions, receiveTransitions, phase + 1, numPhases, saturationValues.tail,
      sendMap, endStatesCombined ++ List((phase, saturationEndStates)))
  }

  /**
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
  // TODO: make private.
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

      val oldTransitions = component.map(state => automaton.outgoingTransitions(state)).flatten

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

    val combinedAutomaton = new Automaton(combinedMap.toMap,
      combinedTransitions.toSet, initialStateName, index, automaton.automatonName)

    (combinedAutomaton, translationMap.toMap)
  }

  /**
   * K-saturates an entire automaton. This assumes that the automaton is already condensed. For an explanation of
   * k-saturation, refer to the external documentation
   *
   * @param k number of added transition steps.
   * @param automaton automaton to k-saturate.
   * @param receiveTransitions transitions between phases.
   * @return (saturated, map)
   *         saturated  a k-saturated version of 'automaton'
   *         map        a map that translates the states in 'automaton' to the end states in 'saturated' for every
   *                    state in 'automaton' that has been k-saturated
   */
  private def kSaturate(k: Int,
                        automaton: Automaton,
                        receiveTransitions:Set[Transition]): (Automaton, Map[String, String]) = {

    val saturationStates = MSet[State]()
    val saturationTransitions = MSet[Transition]()
    val newEndStates = MMap[String, String]()

    val receiveFrom = receiveTransitions.map(a => a.fromName).toSet

    // k-saturate all states with loops
    for (state <- automaton.states) {

      val transitions = automaton.outgoingTransitions(state)
      val internalConditions = transitions.filter(_.to == state.name).map(_.condition)
      val externalTransitions = transitions.filter(_.to != state.name)

      // This state has no loops, no k-saturation needed
      if (internalConditions.isEmpty) {
        for (transition <- externalTransitions) {
          saturationTransitions += transition
        }
        // This state has loops and out-going transitions (either external sends or receives between phases),
        // k-saturate each transition, do not include original transition
      } else if (externalTransitions.nonEmpty || receiveFrom.contains(state.nameString)) {
        val (newStates, newTransitions, endName) =
          kSaturateState(k, automaton.index, state.nameString, state.index, externalTransitions, internalConditions)

        newEndStates(state.nameString) = endName
        saturationStates ++= newStates
        saturationTransitions ++= newTransitions
      }
    }

    val oldStateCopy = automaton.states.map(_.copy)
    val newStates = oldStateCopy ++ saturationStates
    (new Automaton(Automaton.makeStateMap(newStates),
      saturationTransitions.toSet, automaton.initialName,
      automaton.index, automaton.automatonName), newEndStates.toMap)
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
  private def kSaturateState(k: Int,
                     automatonIndex: Option[Int],
                     fromName: String,
                     fromIndex: Option[Int],
                     transitions: Set[Transition],
                     conditions: Set[TransitionCondition]) : (Set[State], Set[Transition], String) = {

    val conditionsMinusNop = conditions.filter{case Nop() => false case _ => true}

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
      conditionsMinusNop.map(condition => {
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





  /* === Phases without condensing or k-saturation. For illustrative purposes only === */

  /**
   * Creates an automaton with send- and receive-phases.
   *
   * @param automaton The original automaton.
   * @param p         Number of phases.
   * @return          An automaton with send- and receive-phases.
   */
  def makePhases(automaton: Automaton, p: Int): Automaton = {
    val (combined, _) = makePhasesForPrinting(automaton, p)

    combined
  }

  /**
   * Creates an automaton with send- and receive-phases. Also returns the created transitions between phases for
   * printing purposes.
   *
   * @param automaton The original automaton.
   * @param p         Number of phases.
   * @return          A tuple containing an automaton with send- and receive-phases and the set of created transitions.
   */
  def makePhasesForPrinting(automaton: Automaton, p:Int): (Automaton, Set[Transition]) = {
    val sendPart = automaton.sendCopy
    val receivePart = automaton.receiveCopy
    val sendTransitions = automaton.sendTransitions
    val receiveTransitions = automaton.receiveTransitions

    addReceive(sendPart.copy(0), Set(), sendPart, receivePart, sendTransitions, receiveTransitions, 1, p)
  }

  private def addSend(combined: Automaton,
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

  private def addReceive(combined: Automaton,
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

  def endStatesForPhased(endNames: List[String],
                         automaton: Automaton,
                         sendMap: Map[String, String],
                         endStateMaps: List[(Int, Map[String, String])]): Set[State] = {

    val phaseEndStateNames = endNames.map(name => {
      val condName = sendMap.getOrElse(name, name)
      endStateMaps.map{case (phase, em) =>
        (phase, em.getOrElse(condName, condName))
      }
    }).flatten

    phaseEndStateNames.map{case (phase, name) =>
      automaton.states.filter(state => {
        state.nameString == name && state.index.get == phase
      })
    }.flatten.toSet
  }
}