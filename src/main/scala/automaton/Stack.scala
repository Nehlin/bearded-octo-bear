package automaton

import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{MultiMap => MMultiMap, HashMap => MHashMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{MutableList => MList}

object Stack {

  def name(s1: String, s2: String): String = {
    s1.toUpperCase + s2.toLowerCase
  }

  def sendMessageName(chn: String, msg: String): String = {
    chn + "!" + msg
  }

  def receiveMessageName(chn: String, msg: String): String = {
    chn + "?" + msg
  }

  def nameFromSend(send: TransitionCondition): String = {
    val Send(chn, msg) = send
    sendMessageName(chn, msg)
  }
  
  def transitionFromName(name: String): TransitionCondition = {
    if (name.contains("!")) {
      val sp = name.split("!")
      Send(sp(0), sp(1))
    } else {
      val sp = name.split("?")
      Receive(sp(0), sp(1))
    }
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
                              iConditions: Set[String],
                              fSigma: Set[String],
                              sSigma: Set[String],
                              eSingle: Set[String],
                              eDouble: Set[(String, String)]) {

    val name = n
    val internalNop = iNop
    val internalConditions = iConditions
    val firstSigma = fSigma
    val secondSigma = sSigma
    val externalSingle = eSingle
    val externalDouble = eDouble
    
    def stateName(first: Boolean)(index: Int) = {
      IntermediateAutomaton.stateName(first)(name, index)
    }
    
    val inState = stateName(true)_
    val outState = stateName(false)_
    
    def makeAutomaton(index: Int): (State, State, Set[Transition]) = {
      val in = new State(inState(index), None)
      val out = new State(outState(index), None)
      
      val transitions = MSet[Transition]()
      if (internalNop) {
        transitions += new Transition(in, out, Nop())
      }

      transitions ++= internalConditions.map(msg => {
        new Transition(in, out, transitionFromName(msg))
      })

      transitions ++= firstSigma.map(msg => {
        new Transition(in, in, transitionFromName(msg))
      })

      transitions ++= secondSigma.map(msg => {
        new Transition(out, out, transitionFromName(msg))
      })

      (in, out, transitions.toSet)
    }

    override def toString =
      "name: " + name + "\n" +
      "internalNop: " + internalNop.toString + "\n" +
      "internalTransitions: " + internalConditions.toList.sorted.mkString(", ") + "\n" +
      "firstSigma: " + firstSigma.toList.sorted.mkString(", ") + "\n" +
      "secondSigma: " + secondSigma.toList.sorted.mkString(", ")+ "\n" +
      "externalSingle: " + externalSingle.toList.sorted.mkString(", ") + "\n" +
      "externalDouble: " + externalDouble.toList.sorted.mkString(", ") + "\n"

  }

  object IntermediateAutomaton {
    def stateName(first: Boolean)(name: String, index: Int) = {
      val firstChar = if (first) "in" else "out"
      firstChar + index + name
    }

    val inState = stateName(true)_
    val outState = stateName(false)_
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

  def addNopForReceive(automaton: Automaton): Automaton = {
    val addedNops = automaton.receiveTransitions.map(receiveTransition => receiveTransition.nopCopy)
    automaton.addTransitions(addedNops)
  }

  def makeGrammar(automaton: Automaton, forSend: Boolean): List[Rule] = {

    val rules = MList[Rule]()

    val states = automaton.sortedStates
    val transitions = automaton.sortedTransitions
    val sendTransitions = transitions.filter(p => p.condition.isInstanceOf[Send])
    val receiveTransitions = transitions.filter(p => p.condition.isInstanceOf[Receive])
    val pushTransitions = transitions.filter(p => p.condition.isInstanceOf[Push])
    val popTransitions = transitions.filter(p => p.condition.isInstanceOf[Pop])
    val nopTransitions = transitions.filter(p => p.condition.isInstanceOf[Nop])

    if (forSend) {
      for (s <- states) {
        rules += Rule.epsilonRule(s.name, s.name)
      }
    }

    val epsilonTransitions = if (forSend) nopTransitions ++ sendTransitions else nopTransitions
    for (t <- epsilonTransitions) {
      val from = t.from
      val to = t.to
      if (from != to) {
        rules += Rule.epsilonRule(from, to)
      }
    }

    val terminalTransitions = if (forSend) sendTransitions else receiveTransitions
    for (tt <- terminalTransitions) {
      val from = tt.from
      val to = tt.to
      val message = if (forSend) {
        val Send(c, m) = tt.condition
        sendMessageName(c, m)
      } else {
        val Receive(c, m) = tt.condition
        receiveMessageName(c, m)
      }
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

    val reachabilityAutomaton = if (forSend) addNopForSend(automaton) else addNopForReceive(automaton)
    reachabilityWithAddFun(reachabilityAutomaton, Some(fun))

    rules.toList
  }

  def makeReceiveBetweenPair(from: String, to: String, wordsBetween: Map[(String, String), Set[List[TransitionCondition]]]):
    Option[(MSet[State], MSet[Transition])] = {

    def makeStatesAndTransitions(fromName: String, toName: String, words: Set[List[TransitionCondition]]):
    (MSet[State], MSet[Transition]) = {

      def stateName(intermediateIndex: Int): String = {
        fromName + toName + intermediateIndex
      }

      if (words.isEmpty) {
        return (MSet[State](), MSet[Transition]())
      }

      var newStates = MSet[State]()
      var newTransitions = MSet[Transition]()

      val startState = new State(stateName(0), None)
      val endState = new State(stateName(1), None)
      newStates += startState
      newStates += endState

      var intermediateIndex = 2
      for (word <- words) {
        var prevState = startState
        for (cond <- word.dropRight(1)) {
          val currentState = new State(stateName(intermediateIndex), None)
          newStates += currentState
          intermediateIndex += 1
          newTransitions += new Transition(prevState, currentState, cond)
          prevState = currentState
        }
        newTransitions += new Transition(prevState, endState, word.last)
      }

      (newStates, newTransitions)
    }

    if (wordsBetween.contains((from, to))) {
      Some(makeStatesAndTransitions(from, to, wordsBetween((from, to))))
    } else {
      None
    }
  }

  def pushdownToNfaCreateHelpDataReceive(automaton: Automaton):
    Map[(String, String), Set[List[TransitionCondition]]] = {

    def isSubword(a: List[TransitionCondition], b: List[TransitionCondition]): Boolean = {
      def isSubwordNopFree(a: List[TransitionCondition], b: List[TransitionCondition]): Boolean = {
        if (a == Nil) {
          true
        } else {
          val firstA = a.head
          if (b.contains(firstA)) {
            isSubword(a.tail, b.slice(b.indexOf(firstA) + 1, b.size))
          } else {
            false
          }
        }
      }

      // TODO: investigate this filtering
      isSubwordNopFree(a.filter(p => !isInstanceOf[Nop]), b.filter(p => !isInstanceOf[Nop]))
    }

    val w = new MHashMap[(String, String, Int), MSet[List[TransitionCondition]]] with MMultiMap[(String, String, Int), List[TransitionCondition]]
    val states = automaton.states
    val transitions = automaton.transitions

    val outgoing = new MHashMap[String, MSet[Transition]] with MMultiMap[String, Transition]
    val incoming = new MHashMap[String, MSet[Transition]] with MMultiMap[String, Transition]
    for (t <- transitions) {
      outgoing.addBinding(t.from, t)
      incoming.addBinding(t.to, t)
    }

    //val n = states.size
    val n = 3

    // Fill bottom layer with terminals
    for (a <- states; b <- states) {
      val an = a.name
      val bn = b.name

      val linkingTransitions = transitions.filter(t => t.links(a, b) && (t.isNop || t.isReceive))
      val (linkingNops, linkingReceives) = linkingTransitions.partition(t => t.isNop)

      if (linkingNops.size > 0) {
        w.addBinding((an, bn, 0), List(Nop()))
      }

      for (rec <- linkingReceives) {
        w.addBinding((an, bn, 0), List(rec.condition))
      }
    }

    var breakFromLayerEquality = false
    var finalIndex = 0
    for (i <- 1 to n * n - 1; if !breakFromLayerEquality) {

      finalIndex = i

      for (a <- states; b <- states) {
        val an = a.name
        val bn = b.name

        // Copy previous layer
        if (w.contains((an, bn, i - 1))) {
          for (existingBinding <- w(an, bn, i - 1)) {
            w.addBinding((an, bn, i), existingBinding)
          }
        }

        // Handle matching push/pop
        val aOutPush = outgoing(an).filter(t => t.isPush)
        val bInPop = incoming(bn).filter(t => t.isPop)

        for (aPush <- aOutPush; bPop <- bInPop) {
          val aTo = aPush.to
          val bFrom = bPop.from
          val Push(aSymbol) = aPush.condition
          val Pop(bSymbol) = bPop.condition

          if (aSymbol == bSymbol) {

            val yc = w.getOrElse((aTo, bFrom, i - 1), Set[List[TransitionCondition]]())
            for (y <- yc) {
              var alreadyExists = false

              val xc = w.getOrElse((an, bn, i), Set[List[TransitionCondition]]())
              for (x <- xc) {
                if (isSubword(x, y)) {
                  alreadyExists = true
                }
              }

              if (!alreadyExists) {
                w.addBinding((an, bn, i), y)
              }
            }

          }
        }

        // Handle intermediate states
        for (c <- states) {
          val cn = c.name

          val yc = w.getOrElse((an, cn, i - 1), Set[List[TransitionCondition]]())
          val zc = w.getOrElse((cn, bn, i - 1), Set[List[TransitionCondition]]())
          for (y <- yc; z <- zc) {
            val yz = y ++ z
            var alreadyExists = false

            val xc = w.getOrElse((an, bn, i), Set[List[TransitionCondition]]())
            for (x <- xc) {
              if (isSubword(x, yz)) {
                alreadyExists = true
              }
            }

            if (!alreadyExists) {
              w.addBinding((an, bn, i), yz)
            }
          }
        }

      }


      val wPreviousLayer = w.filter{ case ((_, _, layer), _) => layer == i - 1}.map{ case ((_, _, _), set) => set}.toSet
      val wCurrentLayer = w.filter{ case ((_, _, layer), _) => layer == i}.map{ case ((_, _, _), set) => set}.toSet
      breakFromLayerEquality = wPreviousLayer == wCurrentLayer

    }

    w.filter{case ((_, _, i), _) => i == finalIndex}.map{case ((f, t, _), words) => ((f, t), words.toSet)}.toMap
  }

  def pushdownToNfaCreateHelpDataSend(automaton: Automaton):
  (List[Rule],
    Automaton,
    Map[String, String],
    MHashMap[String, MSet[List[GrammarComponent]]] with MMultiMap[String, List[GrammarComponent]],
    Map[String, IntermediateAutomaton]
    ) = {

    val rules = Stack.makeGrammar(automaton, true)
    val (condensedDependencyGraph, translationMap) = Stack.dependencyGraph(automaton)

    val sigma = condensedDependencyGraph.states.map(state => {
      val sendConditions = condensedDependencyGraph.outgoingTransitions(state).map(t =>
        t.condition).filter(cond => cond.isInstanceOf[Send])
      (state.name, sendConditions)
    }).toMap

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

    val intermediateAutomatons = condensedDependencyGraph.states.map(state => {

      val stateName = state.name
      var hasEpsilon = false
      val internalTransitions = MSet[String]()
      val firstSigma = MSet[String]()
      val secondSigma = MSet[String]()
      val singleExternal = MSet[String]()
      val doubleExternal = MSet[(String, String)]()

      val rules = ruleMap(stateName)
      rules.foreach {

        case List(Epsilon()) => hasEpsilon = true
        case List(Terminal(msg)) => internalTransitions += msg
        case List(NonTerminal(n1), NonTerminal(n2)) =>
          if (n1 == stateName) {
            secondSigma ++= sigma(n2).map(nameFromSend)
          }
          if (n2 == stateName) {
            firstSigma ++= sigma(n1).map(nameFromSend)
          }
          if (n1 != stateName && n2 != stateName) {
            doubleExternal += ((n1, n2))
          }
        case List(NonTerminal(n)) => singleExternal += n
      }

      val intermediateAutomaton = new IntermediateAutomaton(
        state.name,
        hasEpsilon,
        internalTransitions.toSet,
        firstSigma.toSet,
        secondSigma.toSet,
        singleExternal.toSet,
        doubleExternal.toSet
      )
      (stateName, intermediateAutomaton)
    }).toMap



    (rules, condensedDependencyGraph, translationMap, ruleMap, intermediateAutomatons)
  }

  def makeSubAutomatonIndexMap(condensedDependencyGraph: Automaton): MMap[String, Int] = {
    val subAutomatonIndex = MMap[String, Int]()
    for (state <- condensedDependencyGraph.states) {
      subAutomatonIndex(state.name) = 0
    }

    subAutomatonIndex
  }

  def realiseIntermediateAutomaton(intermediateName: String,
                                   automatonMap: Map[String, IntermediateAutomaton],
                                   subAutomatonIndex: MMap[String, Int],
                                   depth: Option[Int]): (Set[State], Set[Transition], State, State) = {

    val intermediateAutomaton = automatonMap(intermediateName)
    val currentIndex = subAutomatonIndex(intermediateAutomaton.name)
    subAutomatonIndex(intermediateAutomaton.name) = currentIndex + 1
    val (inState, outState, internalTransitions) = intermediateAutomaton.makeAutomaton(currentIndex)

    val newDepth = if (depth.isDefined) Some(depth.get - 1) else None


    val (singleChildStates, singleChildTransitions) = if (depth.isEmpty || depth.get > 0) {
      intermediateAutomaton.externalSingle.map(externalName => {
        if (externalName != intermediateName) {

          val (childStates, childTransitions, childStart, childEnd) =
            realiseIntermediateAutomaton(externalName, automatonMap, subAutomatonIndex, newDepth)

          val startTransition = new Transition(inState.name, childStart.name)
          val endTransition = new Transition(childEnd.name, outState.name)

          Some((childStates, childTransitions + startTransition + endTransition))
        } else {
          None
        }
      }).flatten.unzip
    } else {
      (Set[Set[State]](), Set[Set[Transition]]())
    }

    val (doubleChildStates, doubleChildTransitions) = if (depth.isEmpty || depth.get > 0) {
      intermediateAutomaton.externalDouble.map { case (e1Name, e2Name) =>
        val (e1ChildStates, e1ChildTransitions, e1ChildIn, e1ChildOut) =
          realiseIntermediateAutomaton(e1Name, automatonMap, subAutomatonIndex, newDepth)
        val (e2ChildStates, e2ChildTransitions, e2ChildIn, e2ChildOut) =
          realiseIntermediateAutomaton(e2Name, automatonMap, subAutomatonIndex, newDepth)

        val startTransition = new Transition(inState.name, e1ChildIn.name)
        val intermediateTransition = new Transition(e1ChildOut.name, e2ChildIn.name)
        val endTransition = new Transition(e2ChildOut.name, outState.name)

        val allStates = e1ChildStates ++ e2ChildStates
        val allTransitions = e1ChildTransitions ++ e2ChildTransitions +
          startTransition + intermediateTransition + endTransition

        (allStates, allTransitions)
      }.unzip
    } else {
      (Set[Set[State]](), Set[Set[Transition]]())
    }

    val allStates = singleChildStates.flatten ++ doubleChildStates.flatten + inState + outState
    val allTransitions = singleChildTransitions.flatten ++ doubleChildTransitions.flatten ++ internalTransitions

    (allStates, allTransitions, inState, outState)
  }

  def pushdownToNfaStatesSend(condensedDependencyGraph: Automaton,
                              translationMap: Map[String, String],
                              rules: List[Rule],
                              subAutomatonIndex: MMap[String, Int],
                              intermediateAutomatons: Map[String, IntermediateAutomaton],
                              fromName: String,
                              toName: String): (Set[State], Set[Transition], State, State) = {

    val rawStartStateName = name(fromName, toName)
    val startStateName = translationMap.getOrElse(rawStartStateName, rawStartStateName)
    val (nfaStates, nfaTransitions, inState, outState) =
      realiseIntermediateAutomaton(startStateName, intermediateAutomatons, subAutomatonIndex, None)

    (nfaStates, nfaTransitions, inState, outState)
  }

  def makeInitialSend(condensedDependencyGraph: Automaton,
                      translationMap: Map[String, String],
                      rules: List[Rule],
                      intermediateAutomatons: Map[String, IntermediateAutomaton],
                      initialName: String,
                      outNames: Set[String]): (Automaton, Set[State]) = {

    val (auto, inStates, outStates) = makeSend(condensedDependencyGraph, translationMap, rules,
      intermediateAutomatons, Set(initialName), outNames)

    val initialState = new State("start", None)
    val startTransitions = inStates.map(is => new Transition(initialState.name, is.name))

    (new Automaton(auto.states + initialState, auto.transitions ++ startTransitions, Some(initialState), "initial_send").copy(0), outStates)
  }
  
  def makeSend(condensedDependencyGraph: Automaton,
               translationMap: Map[String, String],
               rules: List[Rule],
               intermediateAutomatons: Map[String, IntermediateAutomaton],
               inNames: Set[String],
               outNames: Set[String]): (Automaton, Set[State], Set[State]) = {

    val subAutomatonIndex = makeSubAutomatonIndexMap(condensedDependencyGraph)

    val (states, transitions, inOutStates) = inNames.map(inName => {
      outNames.map(outName => {

        val rawName = name(inName, outName)
        val stateName = translationMap.getOrElse(rawName, rawName)
        if (condensedDependencyGraph.stateMap.contains(stateName)) {
          val (states, transitions, inState, outState) =
            pushdownToNfaStatesSend(condensedDependencyGraph, translationMap, rules, subAutomatonIndex, intermediateAutomatons, inName, outName)

          val (nopFreeStates, nopFreeTransitions) = removeUselessNops(states, transitions, inState.name, outState.name)
          val (combinedStates, combinedTransitions) = combineDuplicatePaths(nopFreeStates, nopFreeTransitions, inState.name, outState.name)
          val (doubleLoopFreeStates, doubleLoopFreeTransitions) = removeUselessSelfLoops(inState, combinedStates, combinedTransitions)

          Some(doubleLoopFreeStates, doubleLoopFreeTransitions, (inState, outState))
        } else {
          None
        }
      }).flatten.unzip3
    }).unzip3

    val (inStates, outStates) = inOutStates.flatten.unzip

    (new Automaton(states.flatten.flatten, transitions.flatten.flatten, None, ""), inStates, outStates)
  }

  private def trueChildStates(state: State, stateMap: Map[String, State], transitions: MSet[Transition]): Set[State] = {

    val trueOutgoing = transitions.filter(t => t.from == state.name && t.to != state.name)
    trueOutgoing.map(t => stateMap(t.to)).toSet
  }

  private def trueParentStates(state: State, stateMap: Map[String, State], transitions: MSet[Transition]): Set[State] = {
    val trueIncoming = transitions.filter(t => t.to == state.name && t.from != state.name)
    trueIncoming.map(t => stateMap(t.from)).toSet
  }

  private def transitionsForState(stateName: String,
                                  allTransitions: MSet[Transition]): (Set[Transition], Set[Transition]) = {

    val incoming = allTransitions.filter(t => t.to == stateName)
    val outgoing = allTransitions.filter(t => t.from == stateName)
    (incoming.toSet, outgoing.toSet)
  }

  def removeUselessNops(states: Set[State],
                        transitions: Set[Transition],
                        startStateName: String,
                        endStateName: String): (MSet[State], MSet[Transition]) = {

    def outgoingTransitions(state: State): Set[Transition] = {
      transitions.filter(t => t.from == state.name)
    }

    val stateMap = states.map(st => (st.name, st)).toMap

    val sendTransitions = transitions.filter(t => t.condition.isInstanceOf[Send])
    val sendPairs = sendTransitions.map(t => (t.from, t.to))
    val allStates = MSet() ++ states
    val nopTransitions = transitions.filter(t => t.condition.isInstanceOf[Nop])
    val nonNopTransitions = MSet() ++ transitions.filter(t => !t.condition.isInstanceOf[Nop])
    val nopWithoutMatchingSend = MSet() ++ nopTransitions.filter(nt => {
      !sendPairs.contains((nt.from, nt.to))
    })

    val allTransitions = nonNopTransitions ++ nopWithoutMatchingSend

    def isSingleNop(transitionSet: Set[Transition]): Boolean = {
      transitionSet.size == 1 && transitionSet.head.condition.isInstanceOf[Nop]
    }

    def cullByIncoming(currentState: State, states: MSet[State], allTransitions: MSet[Transition]): Unit = {
      val (incoming, outgoing) = transitionsForState(currentState.name, allTransitions)

      if (isSingleNop(incoming) && outgoing.size > 0) {
        val it = incoming.head
        val from = it.from
        states -= currentState
        allTransitions -= it
        for (ot <- outgoing) {
          allTransitions -= ot
          allTransitions += new Transition(from, ot.to, ot.condition)
        }
      }

      for (ot <- outgoing) {
        val nextState = stateMap(ot.to)
        if (nextState != currentState) {
          cullByIncoming(nextState, states, allTransitions)
        }
      }
    }

    def cullByOutgoing(currentState: State, states: MSet[State], allTransitions: MSet[Transition]): Unit = {
      val (incoming, outgoing) = transitionsForState(currentState.name, allTransitions)

      if (isSingleNop(outgoing) && incoming.size > 0) {
        val ot = outgoing.head
        val to = ot.to
        states -= currentState
        allTransitions -= ot
        for (it <- incoming) {
          allTransitions -= it
          allTransitions += new Transition(it.from, to, it.condition)
        }
      }

      for (it <- incoming) {
        val prevState = stateMap(it.from)
        if (prevState != currentState) {
          cullByOutgoing(prevState, states, allTransitions)
        }
      }
    }

    val iState = stateMap(startStateName)
    val firstChildren = outgoingTransitions(iState).map(t => stateMap(t.to))
    val oState = stateMap(endStateName)



    for (child <- firstChildren) {
      cullByIncoming(child, allStates, allTransitions)
    }

    cullByOutgoing(oState, allStates, allTransitions)

    (allStates, allTransitions)
  }

  def combineDuplicatePaths(states: MSet[State],
                            transitions: MSet[Transition],
                            startStateName: String,
                            endStateName: String): (MSet[State], MSet[Transition]) = {

    val stateMap = states.map(st => (st.name, st)).toMap
    val allStates = states
    val allTransitions = transitions

    def trueTransitionsForState(stateName: String,
                                transitions: MSet[Transition]):
    (Set[Transition], Set[Transition], Set[TransitionCondition]) = {

      val (incoming, outgoing) = transitionsForState(stateName, transitions)
      val (trueIncoming, internal) = incoming.partition(t => t.from != stateName)
      val trueOutgoing = outgoing.filter(t => t.to != stateName)
      val internalConditions = internal.map(t => t.condition)
      (trueIncoming, trueOutgoing, internalConditions)
    }

    /**
     * A pair of states (S1, S2) can be merged into a single state if there is a state S, the only incoming
     * transitions (not counting self-loops) for S1 and S2 is S -c1> S1, S -c1> S2 where c1 == c2 and the conditions
     * for the (possibly empty) self loops S1 and S2 are the same.
     *
     */
    def combineForward(currentState: State, states: MSet[State], transitions: MSet[Transition]): Unit = {

      val candidateChildStates = trueChildStates(currentState, stateMap, transitions).map(state => {
        val stateName = state.name
        val (in, out, cond) = trueTransitionsForState(stateName, transitions)
        (stateName, in, out, cond)
      }).filter { case (_, in, _, _) => in.size == 1}

      val matchGroups = candidateChildStates.foldLeft(Set[(TransitionCondition, Set[TransitionCondition])]())
      { case (ack, (_, in, _, internalConditions)) => ack + ((in.head.condition, internalConditions)) }

      for ((inCondition, internalConditions) <- matchGroups) {
        val matchingStates = candidateChildStates.filter { case (_, in, _, internal) =>
          in.head.condition == inCondition && internal == internalConditions
        }

        if (matchingStates.size > 1) {
          val retainState = matchingStates.head
          val (retainName, _, _, _) = retainState
          for ((name, in, out, _) <- matchingStates - retainState) {
            states -= stateMap(name)
            transitions --= in
            for (ot <- out) {
              transitions -= ot
              transitions += new Transition(retainName, ot.to, ot.condition)
            }
          }
        }
      }

      for (child <- trueChildStates(currentState, stateMap, transitions)) {
        combineForward(child, states, transitions)
      }

    }

    def combineBackward(currentState: State, states: MSet[State], transitions: MSet[Transition]): Unit = {

      val candidateParentStates = trueParentStates(currentState, stateMap, transitions).map(state => {
        val stateName = state.name
        val (in, out, cond) = trueTransitionsForState(stateName, transitions)
        (stateName, in, out, cond)
      }).filter { case (_, _, out, _) => out.size == 1}

      val matchGroups = candidateParentStates.foldLeft(Set[(TransitionCondition, Set[TransitionCondition])]())
      { case (ack, (_, _, out, internalConditions)) => ack + ((out.head.condition, internalConditions)) }

      for ((outCondition, internalConditions) <- matchGroups) {
        val matchingStates = candidateParentStates.filter { case (_, _, out, internal) =>
          out.head.condition == outCondition && internal == internalConditions
        }

        if (matchingStates.size > 1) {

          val retainState = matchingStates.head
          val (retainName, _, _, _) = retainState
          for ((name, in, out, _) <- matchingStates - retainState) {
            states -= stateMap(name)
            transitions --= out
            for (it <- in) {
              transitions -= it
              transitions += new Transition(it.from, retainName, it.condition)
            }
          }

        }
      }

      for (parent <- trueParentStates(currentState, stateMap, transitions)) {
        combineBackward(parent, states, transitions)
      }

    }

    combineForward(stateMap(startStateName), allStates, allTransitions)
    combineBackward(stateMap(endStateName), allStates, allTransitions)
    (allStates, allTransitions)
  }

  def removeUselessSelfLoops(initialState: State, states: MSet[State], transitions: MSet[Transition]): (Set[State], Set[Transition]) = {

    val stateMap = states.map(st => (st.name, st)).toMap

    def internalTransitions(stateName: String, transitions: MSet[Transition]): Set[Transition] = {
      transitions.filter(t => t.from == stateName && t.to == stateName).toSet
    }

    def internalConditions(stateName: String, transitions: MSet[Transition]): Set[TransitionCondition] = {
      internalTransitions(stateName, transitions).map(t => t.condition)
    }

    def conditionsBetweenPair(s1Name: String, s2Name: String, transitions: MSet[Transition]): Set[TransitionCondition] = {
      transitions.filter(t => t.from == s1Name && t.to == s2Name && !t.condition.isInstanceOf[Nop]).map(t => t.condition).toSet
    }

    def hasIntermediateStates(s1Name: String, s2Name: String, transitions: MSet[Transition]): Boolean = {
      val s1OutsidePair = transitions.filter(t => t.from == s1Name && t.to != s1Name && t.to != s2Name)
      val s2OutsidePair = transitions.filter(t => t.from != s1Name && t.from != s2Name && t.to == s2Name)
      s1OutsidePair.nonEmpty || s2OutsidePair.nonEmpty
    }

    def searchPairs(currentState: State, transitions: MSet[Transition]): Unit = {
      val name = currentState.name
      val internalCond = internalConditions(name, transitions)
      val children = trueChildStates(currentState, stateMap, transitions)

      for (child <- children) {
        val childName = child.name
        if (internalCond.nonEmpty) {
          if (!hasIntermediateStates(name, childName, transitions)) {
            val childConditions = internalConditions(childName, transitions)
            val intermediateConditions = conditionsBetweenPair(name, childName, transitions)

            if (childConditions == internalCond && (intermediateConditions.isEmpty || internalCond == intermediateConditions)) {
              transitions --= internalTransitions(name, transitions)
            }
          }
        }
      }

      for (child <- children) {
        searchPairs(child, transitions)
      }
    }

    val allStates = MSet[State]() ++ states
    val allTransitions = MSet[Transition]() ++ transitions

    searchPairs(initialState, allTransitions)

    (allStates.toSet, allTransitions.toSet)
  }

  /*

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
      }.pn.unzip3

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
