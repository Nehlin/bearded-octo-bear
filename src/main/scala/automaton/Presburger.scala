package automaton

import com.sun.org.apache.xpath.internal.operations.Bool

import scala.collection.mutable.{MutableList => MList}

object Presburger {
  def makePresburger(sys: Sys): String = {
    val automatons = sys.automatons

    val allStates = automatons.map(_.states).flatten
    val allTransitions = automatons.map(_.transitions).flatten

    val allNop = allTransitions.filter(transition => transition.condition.isInstanceOf[Nop])
    val allSend = allTransitions.filter(transition => transition.condition.isInstanceOf[Send])
    val allReceive = allTransitions.filter(transition => transition.condition.isInstanceOf[Receive])


    val a = automatons.head
    val startState = a.initialState.get
    val endState = a.stateMap("D")
    val states = a.states - startState - endState
    val transitions = a.sortedTransitions


    def incomingVar(state: State, transitions: Array[Transition]): Array[Transition] = {
      transitions.filter(t => t.to == state.name)
    }

    def outgoingVar(state: State, transitions: Array[Transition]): Array[Transition] = {
      transitions.filter(t => t.from == state.name)
    }


    def makeAddition(vars: Array[Transition]): String = {
      if (vars.tail.isEmpty) {
        occ(vars.head)
      } else {
        "(or " + occ(vars.head) + " " + makeAddition(vars.tail) + ")"
      }
    }

    def varName(prefix: String)(transition: Transition) = {
      prefix + "_" + sys.transitionVarName(transition)
    }

    def occ = varName("occ")_
    def seq = varName("seq")_

    def makeFlowRuleSingle(transitions: Array[Transition], value:Boolean):String = {
      val valueString = if (value) "true" else "false"
      "(assert (= " + valueString + " " + makeAddition(transitions) + "))"
    }

    def makeFlowRule(incoming: Array[Transition], outgoing: Array[Transition]): String = {
      "(assert (= " + makeAddition(incoming) + " " + makeAddition(outgoing) + "))"
    }

    def makeUniqueRule(transition1: Transition, transition2: Transition): String = {
      "(assert (not (= seq_" + sys.transitionVarName(transition1) + " seq_" + sys.transitionVarName(transition2) + ")))"
    }

    def makeSequenceRule(transition1: Transition, transition2: Transition): String = {
      "(assert (=> (and "
    }

    val rules = MList[String]()

    for (transition <- transitions) {
      rules += "(declare-const " + occ(transition) + " Bool)"
    }

    for (transition <- transitions) {
      rules += "(declare-const " + seq(transition) + " Int)"
    }

    val transitionPairs = (for (t1 <- transitions; t2 <- transitions; if t1 != t2) yield (t1, t2)).toSet
    val sortedTransitionPairs = transitionPairs.toList.sortBy {case (t1, t2) => t1 + " " + t2}
    for ((t1, t2) <- sortedTransitionPairs) {
      rules += "(assert (not (= " + seq(t1) + " " + seq(t2) + ")))"
    }
    for (t1 <- transitions) {
      for (t2 <- transitions
        if t2.from == t1.to) {
          rules += "(assert (=> (and " + occ(t1) + " " + occ(t2) + ") (< " + seq(t1) + " " + seq(t2) + ")))"
        }
    }

    rules += makeFlowRuleSingle(outgoingVar(startState, transitions), true)
    rules += makeFlowRuleSingle(incomingVar(endState, transitions), true)

    for (state <- states) {
      val incoming = incomingVar(state, transitions)
      val outgoing = outgoingVar(state, transitions)

      if (incoming.nonEmpty && outgoing.nonEmpty) {
        rules += makeFlowRule(incoming, outgoing)
      } else if (incoming.nonEmpty) {
        rules += makeFlowRuleSingle(incoming, false)
      } else if (outgoing.nonEmpty) {
        rules += makeFlowRuleSingle(outgoing, false)
      }

    }

    rules += "(check-sat)"
    rules += "(get-model)"

    rules.mkString("\n")
  }
}
