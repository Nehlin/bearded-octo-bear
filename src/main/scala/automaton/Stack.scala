package automaton

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{MultiMap => MMultiMap, HashMap => MHashMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{MutableList => MList}

object Stack {

  def name(s1: String, s2: String): String = {
    s1.toUpperCase + s2.toLowerCase
  }

  def messageName(chn: String, msg: String): String = {
    chn + "!" + msg
  }

  abstract class GrammarComponent(n: String) {
    val name = n
  }

  case class Epsilon() extends GrammarComponent("Epsilon") {
    override def toString = "Epsilon"
  }

  case class Terminal(n: String) extends GrammarComponent(n) {
    override def toString = "Terminal(" + name + ")"
  }

  case class NonTerminal(n: String) extends GrammarComponent(n) {
    override def toString = "NonTerminal(" + name + ")"
  }

  class Rule(f: List[NonTerminal], t: List[GrammarComponent]) {
    val from = f
    val to = t

    override def toString = from.map(f => f.name).mkString(" ") + " -> " + to.map(t => t.name).mkString(" ")
    def compareString = from.map(f => f.toString).mkString(" ") + " -> " + to.map(t => t.toString).mkString(" ")

    override def equals(o: Any) = o match {
      case rule: Rule => rule.compareString == compareString
      case _ => false
    }

    override def hashCode = compareString.hashCode
  }

  object Rule {

    private def nonTerminalFromPair(s1: String, s2: String) = {
      NonTerminal(name(s1, s2))
    }

    private def singleTerminal(t: String) = {
      List(Terminal(t))
    }

    private def singleEpsilon = {
      List(Epsilon())
    }

    def transitionRule(from1: String, from2: String, to1: String, to2: String): Rule = {
      new Rule(List(nonTerminalFromPair(from1, from2)), List(nonTerminalFromPair(to1, to2)))
    }

    def triRule(from: String, intermediate: String, to: String): Rule = {
      new Rule(List(nonTerminalFromPair(from, to)),
        List(nonTerminalFromPair(from, intermediate), nonTerminalFromPair(intermediate, to)))
    }

    def epsilonRule(from: String, to: String): Rule = {
      new Rule(List(nonTerminalFromPair(from, to)), singleEpsilon)
    }

    def sendRule(from: String, to: String, message: String): Rule = {
      new Rule(List(nonTerminalFromPair(from, to)), singleTerminal(message))
    }
  }

  class IntermediateAutomaton(n: String,
                              iNop: Boolean,
                              iTransitions: Set[String],
                              fSigma: Set[String],
                              sSigma: Set[String],
                              eSingle: Set[String],
                              eDouble: Set[(String, String)]) {

    val name = n
    val internalNop = iNop
    val internalTransitions = iTransitions
    val firstSigma = fSigma
    val secondSigma = sSigma
    val externalSingle = eSingle
    val externalDouble = eDouble

  }


  // NOTE: Should not be called on indexed automatons.
  def reachability(automaton: Automaton): Set[(String, String)] = {
      reachabilityWithAddFun(automaton, None)
  }

  def reachabilityWithAddFun(automaton: Automaton,
                             addFun: Option[(String, String, Option[String], Option[(String, String)])=>Unit]): Set[(String, String)] = {

    def addIfNew(rSet: Set[(String, String)],
                 mSet: MSet[(String, String)],
                 from: String,
                 to: String,
                 via: Option[String],
                 fromPair: Option[(String, String)],
                 startsIn: MMap[String, MSet[String]],
                 endsIn: MMap[String, MSet[String]]): Unit = {

      val key = (from, to)

      if (addFun.isDefined) {
        val fun = addFun.get
        fun(from, to, via, fromPair)
      }

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
            addIfNew(rSet, mSet, beforeFrom, to, Some(from), None, startsIn, endsIn)
          }
        }

        if (startsIn.contains(to)) {
          for (afterTo <- startsIn(to)) {
            addIfNew(rSet, mSet, from, afterTo, Some(to), None, startsIn, endsIn)
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
              addIfNew(discovered.toSet, newlyDiscovered, pushTransition.from, popTransition.to, None, Some((s1, s2)), startsIn, endsIn)
            }
          }
        }

        val s1IncomingNop = s1Incoming.filter(t => t.condition.isInstanceOf[Nop])
        val s2OutgoingNop = s2Outgoing.filter(t => t.condition.isInstanceOf[Nop])

        for (s1IncNop <- s1IncomingNop) {
          addIfNew(discovered.toSet, newlyDiscovered, s1IncNop.from, s2, Some(s1), None, startsIn, endsIn)
        }

        for (s2OutNop <- s2OutgoingNop) {
          addIfNew(discovered.toSet, newlyDiscovered, s1, s2OutNop.to, Some(s2), None, startsIn, endsIn)
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

  def dependencyGraph(automaton: Automaton) = {
    val rules = new MHashMap[String, MSet[String]] with MMultiMap[String, String]

    def fun(from: String, to: String, via: Option[String], pair: Option[(String, String)]): Unit = {
      if (via.isDefined) {
        val fromName = name(from, to)
        rules.addBinding(fromName, name(from, via.get))
        rules.addBinding(fromName, name(via.get, to))
      }
      if (pair.isDefined) {
        val (pairFrom, pairTo) = pair.get
        rules.addBinding(name(from, to), name(pairFrom, pairTo))
      }
    }

    reachabilityWithAddFun(addNopForSend(automaton), Some(fun))

    val stateNames = rules.foldLeft(Set[String]()) { case (ack, (key, values)) => (ack ++ values) + key }

    val stateMap = stateNames.map(sn => (sn, new State(sn, None))).toMap
    val nopTransitions = rules.keys.map(from => {
      rules(from).map(to => new Transition(from, to, Nop())).toSet
    }).toSet.flatten

    val sendTransitions = automaton.sendTransitions
    val convertedSends = sendTransitions.map(st => {
      val sName = name(st.from, st.to)
      new Transition(sName, sName, st.condition)
    })

    val depAuto = new Automaton(stateMap, nopTransitions ++ convertedSends, None, None, automaton.automatonName)

    val (condensedAuto, translationMap) = Phase.condenseSendAutomaton(depAuto)

    def addSendFromState(automaton: Automaton,
                         stateName: String,
                         condition: TransitionCondition,
                         addedTransitions: Set[Transition]): Set[Transition] = {

      val state = automaton.stateMap(stateName)

      val selfLoop = new Transition(state, state, condition)
      if (!addedTransitions.contains(selfLoop)) {
        val incomingNops = automaton.incomingTransitions(state).filter(p => p.condition.isInstanceOf[Nop] && p.from != state.name)

        incomingNops.foldLeft[Set[Transition]](addedTransitions + selfLoop) { (ack, t) =>
          ack ++ addSendFromState(automaton, t.from, condition, ack)
        }
      } else {
        addedTransitions
      }
    }

    val depTransitions = condensedAuto.sendTransitions.foldLeft(Set[Transition]()) { (ack, sendT) =>
      val name = sendT.fromName
      val condition = sendT.condition
      addSendFromState(condensedAuto, name, condition, ack)
    }
    condensedAuto.addTransitions(depTransitions)

    // NOTE: these are all useful for illustrative purposes
    //(depAuto, condensedAuto, condensedAuto.addTransitions(depTransitions))

    (condensedAuto.addTransitions(depTransitions), translationMap)
  }

  def addNopForSend(automaton: Automaton): Automaton = {
    val addedNops = automaton.sendTransitions.map(sendTransition => sendTransition.nopCopy)
    automaton.addTransitions(addedNops)
  }

  def makeGrammar(automaton: Automaton): List[Rule] = {

    val rules = MList[Rule]()

    val states = automaton.sortedStates
    val transitions = automaton.sortedTransitions
    val sendTransitions = transitions.filter(p => p.condition.isInstanceOf[Send])
    val pushTransitions = transitions.filter(p => p.condition.isInstanceOf[Push])
    val popTransitions = transitions.filter(p => p.condition.isInstanceOf[Pop])
    val nopTransitions = transitions.filter(p => p.condition.isInstanceOf[Nop])

    for (s <- states) {
      rules += Rule.epsilonRule(s.name, s.name)
    }

    for (t <- nopTransitions ++ sendTransitions) {
      val from = t.from
      val to = t.to
      if (from != to) {
        rules += Rule.epsilonRule(from, to)
      }
    }

    for (st <- sendTransitions) {
      val from = st.from
      val to = st.to
      val Send(chn, msg) = st.condition
      val message = messageName(chn, msg)
      rules += Rule.sendRule(from, to, message)
    }

    for (pushT <- pushTransitions) {
      val Push(pushMessage) = pushT.condition
      val matchingPops = popTransitions.filter(popT => {
        val Pop(popMessage) = popT.condition
        pushMessage == popMessage
      })
      for (popT <- matchingPops) {
        rules += Rule.transitionRule(pushT.from, popT.to, pushT.to, popT.from)
      }
    }

    def fun(from: String, to: String, via: Option[String], pair: Option[(String, String)]): Unit = {
      if (via.isDefined) {
        rules += Rule.triRule(from, via.get, to)
      }
    }

    reachabilityWithAddFun(addNopForSend(automaton), Some(fun))

    rules.toList
  }

  def pushdownToNfa(automaton: Automaton) = {
    val rules = makeGrammar(automaton)
    val (condensedDependencyGraph, translationMap) = dependencyGraph(automaton)

    val sigma = condensedDependencyGraph.states.map(state => {
      val sendConditions = condensedDependencyGraph.outgoingTransitions(state).map(t =>
        t.condition).filter(cond => cond.isInstanceOf[Send])
      (state.name, sendConditions)
    }).toMap

    println(sigma)

    // Theses are only self-looping, by construction
    /*val additionalSendRules = condensedDependencyGraph.sendTransitions.map(st => {
      val Send(chn, msg) = st.condition
      Rule.sendRule(st.from, st.to, messageName(chn, msg))
    })*/

    val convertedRules = rules.map(rule => {
      val from = rule.from.map{case NonTerminal(name) => NonTerminal(translationMap.getOrElse(name, name))}
      val to = rule.to.map{
        case NonTerminal(name) => NonTerminal(translationMap.getOrElse(name, name))
        case nonTerm => nonTerm
      }
      new Rule(from, to)
    })

    val allRules = convertedRules.distinct

    val ruleMap = allRules.map(rule => {
      val NonTerminal(fromName)::_ = rule.from
      val to = rule.to
      (fromName, to)
    }).foldLeft(new MHashMap[String, MSet[List[GrammarComponent]]] with MMultiMap[String, List[GrammarComponent]]){
      case (ack, (from, to)) => ack.addBinding(from, to)
    }

    for (r <- allRules.sortBy(_.toString)) {
      println(r)
    }

    val leaves = condensedDependencyGraph.states.filter(state => {
      val outgoing = condensedDependencyGraph.outgoingTransitions(state)
      val trueOutgoing = outgoing.filter(t => t.to != state.name)
      trueOutgoing.size == 0
    })
    println(leaves)

    val leerbok = MMap[String, Automaton]()
    for (leaf <- leaves) {

      val stateName = leaf.name
      var hasEpsilon = false
      val internalTransitions = MSet[String]()
      val firstSigma = MSet[String]()
      val secondSigma = MSet[String]()
      val singleExternal = MSet[String]()
      val doubleExternal = MSet[(String, String)]()

      val rules = ruleMap(stateName)
      val ap = rules.foreach {

        case List(Epsilon()) => hasEpsilon = true
        case List(Terminal(msg)) => internalTransitions += msg
        case List(NonTerminal(n1), NonTerminal(n2)) =>
          if (n1 == stateName) {
            // Fetch sigma of n2 and add to secondSigma
            //secondSigma
          }
          if (n2 == stateName) {

          }
          if (n1 != stateName && n2 != stateName) {
            doubleExternal += ((n1, n2))
          }
        case List(NonTerminal(n)) => singleExternal += n
      }

      new IntermediateAutomaton(
        leaf.name,
        hasEpsilon,
        internalTransitions.toSet,
        firstSigma.toSet,
        secondSigma.toSet,
        singleExternal.toSet,
        doubleExternal.toSet
      )
      println(ap)
    }

    allRules
  }
/*


  def temp(automaton: Automaton): (Automaton, Set[Transition]) = {
    val addedNops = automaton.sendTransitions.map(sendTransition => sendTransition.nopCopy)
    val newAuto = automaton.addTransitions(addedNops)

    val statePairs = Stack.reachability(newAuto)
    val newNops = statePairs.map{ case (s1, s2) => new Transition(s1, s2, Nop())}

    (newAuto.addTransitions(newNops), newNops)
  }

  def expandAutomatonPrint(automaton: Automaton): (Automaton, Set[Transition]) = {
    val stateNamePairs = reachability(automaton)
    val newTransitions = stateNamePairs.map{ case (s1, s2) => new Transition(s1, s2, Nop())}

    (automaton.addTransitions(newTransitions), newTransitions)
  }

  def indexToCoordinates(index: Int, dimensions: List[Int]): List[Int] = {
    var prevDims = 1
    dimensions.map(currentDim => {
      val res = (index / prevDims) % currentDim
      prevDims = prevDims * currentDim
      res
    })
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
  }*/
}
