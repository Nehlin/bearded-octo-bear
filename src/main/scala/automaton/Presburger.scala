package automaton

import com.sun.org.apache.xpath.internal.operations.Bool

import scala.collection.mutable.{MutableList => MList}

object Presburger {
  def makePresburger(sys: Sys, phases: Int): String = {

    val automatons = sys.automatons

    val allTransitions = automatons.map(_.transitions).flatten.sortBy(t => t.toString).toList
    val allStates = automatons.map(_.states).flatten.sortBy(s => s.name).toList
    val allInitialStates = automatons.map(_.initialState).flatten
    val endState = allStates.filter(s => s.name == "H").head

    val allSend = allTransitions.filter(transition => transition.condition.isInstanceOf[Send])
    val allReceive = allTransitions.filter(transition => transition.condition.isInstanceOf[Receive])

    def varName(prefix: String)(transition: Transition) = {
      prefix + "_" + sys.transitionVarName(transition)
    }

    def occ = varName("occ")_
    def seq = varName("seq")_
    def mat = varName("match")_

    def declare(name: String, varType: String): String = {
      "(declare-const " + name + " " + varType + ")"
    }

    def assert(condition: String): String = {
      "(assert " + condition + ")"
    }

    def comment(string: String): String = {
      ";\n" +
      "; " + string
    }

    def unary(operator: String, operand: String): String = {
      "(" + operator + " " + operand + ")"
    }

    def binary(operator: String, op1: String, op2: String): String = {
      "(" + operator + " " + op1 + " " + op2 + ")"
    }

    def chain(operator: String, operands: List[String]): String = {
      if (operands.tail.isEmpty) {
        operands.head
      } else {
        binary(operator, operands.head, chain(operator, operands.tail))
      }
    }

    def occurs(transition: Transition): String = {
      binary("=", occ(transition), "1")
    }

    val rules = MList[String]()

    rules += comment("Define occurrence and constrain it to 1/0")
    for (transition <- allTransitions) {
      rules += declare(occ(transition), "Int")
    }

    for (transition <- allTransitions) {
      rules += assert(
        binary("or",
          binary("=", occ(transition), "0"),
          binary("=", occ(transition), "1")
        )
      )
    }



    rules += comment("Define sequence and enforce uniqueness")
    for (transition <- allTransitions) {
      rules += declare(seq(transition), "Int")
    }

    val transitionPairs = allTransitions.combinations(2).toList.map{case List(t1, t2) => (t1, t2)}
    for ((t1, t2) <- transitionPairs) {
      rules += assert(
        unary("not",
          binary("=", seq(t1), seq(t2))
        )
      )
    }



    rules += comment("Incoming flow = outgoing flow for all intermediate states")
    for (state <- allStates.diff(allInitialStates).diff(List(endState))) {
      val incomingOcc = allTransitions.filter(p => p.to == state.name).toList.map(occ)
      val outgoingOcc = allTransitions.filter(p => p.from == state.name).toList.map(occ)
      if (incomingOcc.nonEmpty && outgoingOcc.nonEmpty) {
        rules += assert(
          binary("=",
            chain("+", incomingOcc),
            chain("+", outgoingOcc)
          )
        )
      } else if (incomingOcc.nonEmpty) {
        rules += assert(
          binary("=",
            chain("+", incomingOcc),
            "0"
          )
        )
      } else if (outgoingOcc.nonEmpty) {
        rules += assert(
          binary("=",
            chain("+", outgoingOcc),
            "0"
          )
        )
      }
    }



    rules += comment("Outgoing flow from initial states is at most 1")
    for (state <- allInitialStates) {
      val outgoingOcc = allTransitions.filter(p => p.from == state.name).toList.map(occ)
      rules += assert(
        binary("<=",
          chain("+", outgoingOcc),
          "1"
        )
      )
    }



    rules += comment("Incoming flow to target state is 1")
    val incomingOcc = allTransitions.filter(p => p.to == endState.name).toList.map(occ)
    rules += assert(
      binary("=",
        chain("+", incomingOcc),
        "1"
      )
    )



    rules += comment("Increasing sequence number along a path")
    for (t1 <- allTransitions) {
      for (t2 <- allTransitions.filter(t2 => t1.to == t2.from)) {
        rules += assert(
          binary("=>",
            binary("and", occurs(t1), occurs(t2)),
            binary("<", seq(t1), seq(t2))
          )
        )
      }
    }



    rules += comment("Each receive needs a matching send")
    val (declareMatch, assertMatch) = allReceive.map(rt => {
      val (rChn, rMsg) = rt.condition match {case Receive(chn, msg) => (chn, msg)}
      val matchingSends = allSend.filter(st => {
        st.condition match {case Send(chn, msg) => chn == rChn && msg == rMsg}
      })

      val matchRules = matchingSends.map(st => {
        binary("and",
          binary("=", mat(rt), seq(st)),
          occurs(st)
        )
      })

      (declare(mat(rt), "Int"),
      assert(
        binary("=>",
          occurs(rt),
          chain("or", matchRules)
        )
      ))
    }).unzip
    rules ++= declareMatch
    rules ++= assertMatch

    rules += "(check-sat)"
    rules += "(get-model)"
    rules.mkString("\n")
    /*
    val automatons = sys.automatonsWithPhases(phases)
    val automatons = sys.automatons

    val allTransitions = automatons.map(_.transitions).flatten.sortBy(t => t.toString)

    val allSend = allTransitions.filter(transition => transition.condition.isInstanceOf[Send])
    val allReceive = allTransitions.filter(transition => transition.condition.isInstanceOf[Receive])


    val a = automatons.head
    val startState = a.initialState.get
    val endState = a.stateMap("E")
    val states = a.states - startState - endState
    val transitions = a.sortedTransitions



    def incomingVar(state: State, transitions: Array[Transition]): Array[Transition] = {
      transitions.filter(t => t.to == state.name)
    }

    def outgoingVar(state: State, transitions: Array[Transition]): Array[Transition] = {
      transitions.filter(t => t.from == state.name)
    }

    def makeComposition(vars: Array[String], operator: String): String = {
      if (vars.tail.isEmpty) {
        vars.head
      } else {
        "(" + operator + " " + vars.head + " " + makeComposition(vars.tail, operator) + ")"
      }
    }

    def makeDisjunction(vars: Array[String]): String = {
      makeComposition(vars, "or")
    }

    def makeAddition(vars: Array[Transition]): String = {
      makeComposition(vars.map(occ), "+")
    }

    def varName(prefix: String)(transition: Transition) = {
      prefix + "_" + sys.transitionVarName(transition)
    }

    def occ = varName("occ")_
    def seq = varName("seq")_
    def mat = varName("match")_

    def doesOccur(transition: Transition): String = {
      "(= " + occ(transition) + " 1)"
    }

    def makeFlowRuleSingle(transitions: Array[Transition], value:Boolean):String = {
      val valueString = if (value) "1" else "0"
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
      rules += "(declare-const " + occ(transition) + " Int)"
    }

    for (transition <- transitions) {
      rules += "(declare-const " + seq(transition) + " Int)"
    }

    val transitionPairs = transitions.combinations(2).toList.map(a => (a(0), a(1)))
    for ((t1, t2) <- transitionPairs) {
      rules += "(assert (not (= " + seq(t1) + " " + seq(t2) + ")))"
    }

    for (t1 <- transitions) {
      for (t2 <- transitions
        if t2.from == t1.to) {
          rules += "(assert " +
            "(=> (and " +
            doesOccur(t1) + " " +
            doesOccur(t2) +
            ") " +
            "(< " + seq(t1) + " " + seq(t2) + ")))"
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

    def makeMatchRule(receive: Transition): Option[(String, String)] = {
      val (rChn, rMsg) = receive.condition match {case Receive(chn, msg) => (chn, msg)}
      val matchingSends = allSend.filter(send => {
        val (sChn, sMsg) = send.condition match {case Send(chn, msg) => (chn, msg)}
        rChn == sChn && rMsg == sMsg
      })

      if (matchingSends.nonEmpty) {
        val defRule = "(declare-const " + mat(receive) + " Int)"
        val sendRules = matchingSends.map(send => {
          "(and " +
          "(= " + mat(receive) + " " + seq(send) + ") " +
          doesOccur(send) +
          ")"
        })
        val sendRule = "(assert " + makeDisjunction(sendRules.toArray) + ")"
        Some((defRule, sendRule))
      } else {
        None
      }
    }

    val (matchDefRules, matchRules) = allReceive.toArray.map(makeMatchRule).flatten.unzip
    rules ++= matchDefRules
    rules ++= matchRules

    val receivePairs = allReceive.combinations(2).toList.map(a => (a(0), a(1)))
    for ((r1, r2) <- receivePairs) {
      val r1Channel = r1.condition match { case Receive(chn, _) => chn }
      val r2Channel = r2.condition match { case Receive(chn, _) => chn }

      if (r1Channel == r2Channel) {
        rules += "(assert (=> " +
          "(and " +
          doesOccur(r1) + " " +
          doesOccur(r2) +
          ") " +
          "(= (< " + seq(r1) + " " + seq(r2) + ") " +
          "(< " + mat(r1) + " " + mat(r2) + "))))"
      }
    }

    for (receive <- allReceive) {
      // TODO: figure this one out
      //rules += "(assert (=> " + occ(receive) + " " + mat(receive) + "))"
    }

    rules += "(check-sat)"
    rules += "(get-model)"

    rules.mkString("\n")
    */
  }
}
