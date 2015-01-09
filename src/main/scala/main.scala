// TODO: rename states to ensure collision free behaviour from _(index), _tmp, etc

import automaton._

import scala.xml._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

object Y {

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
      val (newCombined, newTransitions) = Automaton.combine(combined, sendPart.copy(Some(phase)), sendTransitions)
      val newAddedTransitions = addedTransitions ++ newTransitions

      addReceive(newCombined, newAddedTransitions, sendPart, receivePart, sendTransitions, receiveTransitions, phase + 1, numPhases)
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
      val (newCombined, newTransitions) = Automaton.combine(combined, receivePart.copy(Some(phase)), receiveTransitions)
      val newAddedTransitions = addedTransitions ++ newTransitions

      addSend(newCombined, newAddedTransitions, sendPart, receivePart, sendTransitions, receiveTransitions, phase + 1, numPhases)
    }
  }

  def combineScc(automaton: Automaton): (Automaton,
    Map[String, String]) = {

    val index = automaton.index

    val translationMap = MMap[String, String]()
    val combinedMap = MMap[String, State]()
    val combinedTransitions = MSet[Transition]()
    var initialStateName: Option[String] = None

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
        initialStateName = Some(combinedState.name)
      }
      oldTransitions
    }

    // NOTE: must be called after all calls of constructNewComponent
    def translateTransitions(transitions: (Set[Transition])): Unit = {

      for (transition <- transitions)
        combinedTransitions +=
          new Transition(translationMap(transition.from), translationMap(transition.to), transition.condition)
    }

    val transitions = automaton.scc.map(constructNewComponent)
    transitions.foreach(translateTransitions)

    val combinedAutomaton = new Automaton(combinedMap.toMap, combinedTransitions.toSet, initialStateName, index)

    (combinedAutomaton, translationMap.toMap)
  }

  // TODO: temp states need index and better name to avoid name collisions
  // NOTE: transitions must all have the same from- and to-state
  def kSaturate(k: Int,
                transitions: Set[Transition],
                conditions: Set[TransitionCondition]) : (Set[State], Set[Transition]) = {

    // Since all transitions target the same state, just pick any state for from/to-values
    val anyTransition = transitions.toArray.head
    val fromName = anyTransition.from
    val toName = anyTransition.to

    // Removes duplicate conditions since sets enforce uniqueness
    val transitionConditions = transitions.map(_.condition).toSet

    def tmpName(index: Int) = fromName + "_" + toName + index
    val tmpStates = (0 to k).map(i => new State(tmpName(i), None))

    val newTransitions = MSet[Transition]()
    newTransitions += new Transition(fromName, tmpStates.head.nameString, Nop())

    newTransitions ++= (0 to k-1).map(i => {
      conditions.map(condition => {
        val fromState = tmpStates(i)
        val toState = tmpStates(i+1)
        new Transition(fromState, toState, condition)
      })
    }).flatten
    for (condition <- transitionConditions) {
      newTransitions += new Transition(tmpStates(k).name, toName, condition)
    }

    (tmpStates.toSet, newTransitions.toSet)
  }

  def makeCondensedPhases(automaton: Automaton, p:Int): Automaton = {
    val sendPart = automaton.sendCopy
    val receivePart = automaton.receiveCopy
    val receiveTransitions = automaton.receiveTransitions
    val sendTransitions = automaton.sendTransitions

    val (condensedSend, translationMap) = combineScc(sendPart)

    // Only used for transitions between phases, update to target new automaton.
    val condensedSendTransitions = sendTransitions.map(st => {
      val toName = translationMap(st.to)

      new Transition(st.from, toName, st.condition)
    })
    val condensedReceiveTransitions = receiveTransitions.map(rt => {
      val fromName = translationMap(rt.from)

      new Transition(fromName, rt.to, rt.condition)
    })

    val saturationStates = MSet[State]()
    val saturationTransitions = MSet[Transition]()

    // k-saturate all states with loops
    for (state <- condensedSend.states) {
      val transitions = condensedSend.transitions(state)
      val internalConditions = transitions.filter(_.to == state.name).map(_.condition)
      val externalTransitions = transitions.filter(_.to != state.name)
      val externalGrouped = externalTransitions.groupBy(_.to).values.toSet
      println(externalGrouped)

      // This state has no loops, no k-saturation needed
      if (internalConditions.isEmpty) {
        for (transition <- externalTransitions) {
          saturationTransitions += transition
        }
      // This state has loops, k-saturate each transition, do not include original transition
      } else {
        for (transitionsGrouped <- externalGrouped) {
          val (newStates, newTransitions) = kSaturate(1, transitionsGrouped, internalConditions)
          saturationStates ++= newStates
          saturationTransitions ++= newTransitions
        }
      }
    }

    val newStates = condensedSend.states ++ saturationStates
    condensedSend
    new Automaton(Automaton.makeStateMap(newStates),
        saturationTransitions.toSet, condensedSend.initialName, condensedSend.index)
  }

  def makePhasesForPrinting(automaton: Automaton, p:Int): (Automaton, Set[Transition]) = {
    val sendPart = automaton.sendCopy
    val receivePart = automaton.receiveCopy
    val sendTransitions = automaton.sendTransitions
    val receiveTransitions = automaton.receiveTransitions

    addReceive(sendPart.copy(Some(1)), Set(), sendPart, receivePart, sendTransitions, receiveTransitions, 2, p)
  }

  def makePhases(automaton: Automaton, p: Int): Automaton = {
    val (combined, _) = makePhasesForPrinting(automaton, p)

    combined
  }



  def main(args:Array[String]) {

    val filename = if (args.length > 0) {
      args(0)
    } else {
      "sample_xml/simpler_scc.xml"
    }

    val xml = XML.loadFile(filename)
    val process = (xml \\ "protocol" \\ "role").head

    val original = Automaton.fromXml(process)

    /*
    val sendPart = original.sendCopy(1)
    val receivePart = original.receiveCopy(2)
    val sendPart2 = original.sendCopy(3)
    val receivePart2 = original.receiveCopy(4)

    val receiveTransitions = original.receiveTransitions
    val sendTransitions = original.sendTransitions
    val (combined, transitions1)  = Automaton.combine(sendPart, receivePart, receiveTransitions)
    val (combined2, transitions2) = Automaton.combine(combined, sendPart2, sendTransitions)
    val (combined3, transitions3) = Automaton.combine(combined2, receivePart2, receiveTransitions)
    val reachable3Names = combined3.reachableStates.map(_.name)

    //println(reachable3Names.toArray.sortBy(_.reverse).map(println(_)))
    val transitions = transitions1 ++ transitions2 ++ transitions3
    */


    val condensed = makeCondensedPhases(original, 2)

    //original.sendCopy
    //val (scc, _, _) = combineScc(original.sendCopy)

    val (combined, transitions) = makePhasesForPrinting(original, 3)
    val reachableNames = Some(combined.reachableStates.map(_.name))

    Files.write(Paths.get("dot/graph_original.dot"), Dot.make(original).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/graph_send.dot"), Dot.make(original.sendCopy).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/condensed.dot"), Dot.make(condensed).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/graph_copy(1).dot"), Dot.make(original.copy(Some(1))).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/graph_copy(1).copy.dot"), Dot.make(original.copy(Some(1)).copy).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/transpose.dot"), Dot.make(original.transpose).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/graph_combined.dot"), Dot.makeHighlighted(combined, transitions, reachableNames).getBytes(StandardCharsets.UTF_8))

    //Files.write(Paths.get("dot/graph_scc.dot"), Dot.make(scc).getBytes(StandardCharsets.UTF_8))


    //Files.write(Paths.get("graph_combined.dot"), Dot.makeHighlighted(combined3, transitions, Some(reachable3Names)).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/scc.dot"), Dot.highlightScc(original.sendCopy).getBytes(StandardCharsets.UTF_8))
  }
}