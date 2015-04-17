package automaton

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}

object Stack {
  // NOTE: Should not be called on indexed automatons.
  def reachability(automaton: Automaton): Set[(String, String)] = {

    def addIfNew(rSet: Set[(String, String)],
                 mSet: MSet[(String, String)],
                 from: String,
                 to: String,
                 startsIn: MMap[String, MSet[String]],
                 endsIn: MMap[String, MSet[String]]): Unit = {

      val key = (from, to)
      if (!rSet.contains(key) && !mSet.contains(key)) {

        mSet.add(key)

        if (!startsIn.contains(from)) {
          startsIn(from) = MSet[String]()
        }
        if (!endsIn.contains(to)) {
          endsIn(to) = MSet[String]()
        }
        startsIn(from).add(to)
        endsIn(to).add(from)


        if (endsIn.contains(from)) {
          for (beforeFrom <- endsIn(from)) {
            addIfNew(rSet, mSet, beforeFrom, to, startsIn, endsIn)
          }
        }

        if (startsIn.contains(to)) {
          for (afterTo <- startsIn(to)) {
            addIfNew(rSet, mSet, from, afterTo, startsIn, endsIn)
          }
        }

      }
    }

    def expandReachable(discovered: MSet[(String, String)],
                        horizon: MSet[(String, String)],
                        startsIn: MMap[String, MSet[String]],
                        endsIn: MMap[String, MSet[String]]): Unit = {

      val newlyDiscovered = MSet[(String, String)]()

      for ((s1, s2) <- horizon) {

        val s1Incoming = automaton.incomingTransitions(automaton.stateMap(s1))
        val s2Outgoing = automaton.outgoingTransitions(automaton.stateMap(s2))

        val s1IncomingPush = s1Incoming.filter(t => t.condition.isInstanceOf[Push])
        val s2OutgoingPop = s2Outgoing.filter(t => t.condition.isInstanceOf[Pop])

        for (pushTransition <- s1IncomingPush) {
          val pushMsg = pushTransition.condition match {
            case Push(m) => m
          }

          for (popTransition <- s2OutgoingPop) {
            val popMsg = popTransition.condition match {
              case Pop(m) => m
            }

            if (pushMsg == popMsg) {
              addIfNew(discovered.toSet, newlyDiscovered, pushTransition.from, popTransition.to, startsIn, endsIn)
            }
          }
        }

        val s1IncomingNop = s1Incoming.filter(t => t.condition.isInstanceOf[Nop])
        val s2OutgoingNop = s2Outgoing.filter(t => t.condition.isInstanceOf[Nop])

        for (s1IncNop <- s1IncomingNop) {
          addIfNew(discovered.toSet, newlyDiscovered, s1IncNop.from, s2, startsIn, endsIn)
        }

        for (s2OutNop <- s2OutgoingNop) {
          addIfNew(discovered.toSet, newlyDiscovered, s1, s2OutNop.to, startsIn, endsIn)
        }
      }

      discovered ++= newlyDiscovered
      if (newlyDiscovered.nonEmpty) {
        expandReachable(discovered, newlyDiscovered, startsIn, endsIn)
      }
    }

    val startsIn = MMap[String, MSet[String]]()
    val endsIn = MMap[String, MSet[String]]()

    val rSet = MSet[(String, String)]()
    for (state <- automaton.states) {
      rSet.add((state.name, state.name))
    }

    expandReachable(rSet, rSet, startsIn, endsIn)
    rSet.filter{case (s1, s2) => s1 != s2}.toSet
  }

  def convertAutomaton(automaton: Automaton): Automaton = {
    val (res, _) = convertAutomatonPrint(automaton)
    res
  }

  def convertAutomatonPrint(automaton: Automaton): (Automaton, Set[Transition]) = {
    val stateNamePairs = reachability(automaton)
    val newTransitions = stateNamePairs.map{ case (s1, s2) => new Transition(s1, s2, Nop())}

    val remainingTransitions = automaton.transitions.filter(t =>
      !(t.condition.isInstanceOf[Push] || t.condition.isInstanceOf[Pop])
    )
    val allTransitions = newTransitions ++ remainingTransitions

    val newStateMap = automaton.states.map(state => (state.name, state.copy)).toMap
    val newAutomaton = new Automaton(newStateMap, allTransitions,
      automaton.initialName, automaton.index, automaton.automatonName)

    (newAutomaton, newTransitions)
  }

  def expandAutomatonPrint(automaton: Automaton): (Automaton, Set[Transition]) = {
    val stateNamePairs = reachability(automaton)
    val newTransitions = stateNamePairs.map{ case (s1, s2) => new Transition(s1, s2, Nop())}

    (automaton.addTransitions(newTransitions), newTransitions)
  }

  def addNopForSend(automaton: Automaton): Automaton = {
    val addedNops = automaton.sendTransitions.map(sendTransition => sendTransition.nopCopy)
    automaton.addTransitions(addedNops)
  }

  def temp(automaton: Automaton): (Automaton, Set[Transition]) = {
    val addedNops = automaton.sendTransitions.map(sendTransition => sendTransition.nopCopy)
    val newAuto = automaton.addTransitions(addedNops)

    val statePairs = Stack.reachability(newAuto)
    val newNops = statePairs.map{ case (s1, s2) => new Transition(s1, s2, Nop())}

    (newAuto.addTransitions(newNops), newNops)
  }

  

  def hoppo(automaton: Automaton, m: Int): (Automaton, Set[Transition]) = {

    def stateName(stateName: String, currentPos: List[(String, Int)]): String = {
      val suffix = currentPos.map{case (chn, index) => index.toString + chn}.mkString("")
      stateName + suffix
    }

    def keyFromPos(pos: List[(String, Int)]) = {
      pos.mkString("")
    }

    def makeStep(pos: List[(String, Int)],
                 prevPos: Option[List[(String, Int)]],
                 numSteps: Int,
                 addedKeys: MSet[String]): Option[(Set[String], Set[Transition], Set[Transition])] = {

      val maxStep = pos.map{ case (_, index) => index}.max
      val key = keyFromPos(pos)

      // This state is out of the bounds
      if (maxStep > numSteps) {
        println(maxStep)
        return None
      }

      val internalTransitions = automaton.transitions.filter(p => !p.condition.isInstanceOf[Send])
      val externalTransitions = automaton.transitions.filter(p => p.condition.isInstanceOf[Send])

      val intermediateTransitions = if (prevPos.isDefined) {
        // The channel between this component and the previous is the channel where prevPos and pos differ
        val prevP = prevPos.get
        val (matchChn, _) = prevP.reduce((curr, ack) => if (!pos.contains(curr)) curr else ack)
        val channelTransitions = externalTransitions.filter(p => {
          p.condition match {
            case Send(chn, msg) => chn == matchChn
          }
        })

        channelTransitions.map(ct => {
          val fromName = stateName(ct.from, prevP)
          val toName = stateName(ct.to, pos)
          val cond = ct.condition

          new Transition(fromName, toName, cond)
        })
      } else {
        Set[Transition]()
      }

      // This component has been created, but the intermediate transitions must be added
      if (addedKeys.contains(key)) {
        return Some(Set[String](), intermediateTransitions, intermediateTransitions)
      }

      // This component has not been created, create it and add intermediate transitions
      addedKeys.add(key)

      val newNameMap = automaton.states.map(state => {
        (state.name, stateName(state.name, pos))
      }).toMap
      val newNames = newNameMap.values.toSet

      //val newTransitions = internalTransitions.map(it => {
      val nt1 = internalTransitions.map(it => {
        val fromName = newNameMap(it.from)
        val toName = newNameMap(it.to)
        val cond = it.condition
        new Transition(fromName, toName, cond)
      })

      val nt2 = externalTransitions.map(et => {
        val fromName = newNameMap(et.from)
        val toName = newNameMap(et.to)
        val cond = Nop()
        new Transition(fromName, toName, cond)
      })

      val newTransitions = nt1 ++ nt2

      val (childStates, childTransitions, childIntermediate) = pos.map{case(chnName, currentIndex) =>
        val nextPos = pos.map{case (cName, ind) => if (cName == chnName) { (cName, ind + 1) } else { (cName, ind) } }
        makeStep(nextPos, Some(pos), numSteps, addedKeys)
      }.flatten.unzip3

      val allNames = newNames ++ childStates.flatten
      val allTransitions = newTransitions ++ childTransitions.flatten ++ intermediateTransitions
      val allIntermediate = intermediateTransitions ++ childIntermediate.flatten

      Some(allNames, allTransitions, allIntermediate)
    }

    val sendConditions = automaton.transitions.filter(t => t.condition.isInstanceOf[Send]).map(t => t.condition)
    val channels = sendConditions.map{case Send(chn, _) => chn}.toList.sorted

    println(channels)

    val startPos = channels.map(chn => (chn, 0))
    val (stateNames, allTransitions, intermediateTransitions) =
      makeStep(startPos, None, m, MSet[String]()).get

    val stateMap = stateNames.map(name => (name, new State(name, None))).toMap
    val initialName = automaton.initialName match {
      case Some((str, _)) => Some((stateName(str, startPos), None))
      case _ => None
    }

    val newAuto = new Automaton(stateMap, allTransitions, initialName, automaton.index, automaton.automatonName)

    (newAuto, intermediateTransitions)
  }
}
