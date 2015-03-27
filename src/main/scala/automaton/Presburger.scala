package automaton

import scala.collection.mutable.{MutableList => MList}

object Presburger {
  def makePresburger(sys: Sys, phases: Int): String = {

    val automatons = sys.automatons

    val allTransitions = automatons.map(_.transitions).flatten.sortBy(t => t.toString).toList
    val allStates = automatons.map(_.states).flatten.sortBy(s => s.name).toList
    val allInitialStates = automatons.map(_.initialState).flatten
    val allEndStates = sys.endStatesNormalised

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

    def getChannelMessage(transition: Transition): (String, String) = {
      transition.condition match {
        case Send(chn, msg) => (chn, msg)
        case Receive(chn, msg) => (chn, msg)
      }
    }

    def getChannel(transition: Transition): String = {
      getChannelMessage(transition) match { case (chn, _) => chn}
    }

    def getMessage(transition: Transition): String = {
      getChannelMessage(transition) match { case (_, msg) => msg}
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
    for (state <- allStates.diff(allInitialStates).diff(allEndStates.flatten)) {
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



    rules += comment("Incoming flow to at least one target state is 1")
    for (endStatesForSingleAutomaton <- allEndStates) {
      for (endState <- endStatesForSingleAutomaton) {
        val incomingOcc = allTransitions.filter(t => t.to == endState.name).toList.map(occ)
        val outgoingOcc = allTransitions.filter(t => t.from == endState.name).toList.map(occ)
        if (incomingOcc.nonEmpty && outgoingOcc.nonEmpty) {
          rules += assert(
            binary("<=",
              chain("+", outgoingOcc),
              chain("+", incomingOcc)
            )
          )
        }
      }
      val nonStartEndStates = endStatesForSingleAutomaton.map(endState => {
        val incomingOcc = allTransitions.filter(t => t.to == endState.name).toList.map(occ)
        val outgoingOcc = allTransitions.filter(t => t.from == endState.name).toList.map(occ)
        if (incomingOcc.nonEmpty && outgoingOcc.nonEmpty) {
          Some(binary("and",
            binary("=", chain("+", incomingOcc), "1"),
            binary("=", chain("+", outgoingOcc), "0")
          ))
        } else if (incomingOcc.nonEmpty) {
          Some(binary("=", chain("+", incomingOcc), "1"))
        } else {
          None
        }
      }).flatten

      if (nonStartEndStates.nonEmpty) {
        rules += assert(chain("or", nonStartEndStates))
      }
    }



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



    rules += comment("Each receive needs a matching send, send needs to occur before receive")
    val (declareMatch, assertMatch) = allReceive.map(rt => {
      val chnMsg = getChannelMessage(rt)
      val matchingSends = allSend.filter(st => {
        getChannelMessage(st) == chnMsg
      })

      if (matchingSends.nonEmpty) {

        val matchRules = matchingSends.map(st => {
          binary("and",
            binary("=", mat(rt), seq(st)),
            occurs(st)
          )
        })

        val declareMatch = declare(mat(rt), "Int")
        val assertMatch = assert(
          binary("and",
            binary("=>",
              occurs(rt),
              chain("or", matchRules)
            ),
            binary("<", mat(rt), seq(rt))
          )
        )
        Some((declareMatch, assertMatch))
      } else {
        None
      }
    }).flatten.unzip
    rules ++= declareMatch
    rules ++= assertMatch



    rules += comment("Respect FIFO for channels")
    val receivePairs = allReceive.combinations(2).toList.map{case List(t1, t2) => (t1, t2)}
    for ((rt1, rt2) <- receivePairs) {
      if (getChannel(rt1) == getChannel(rt2)) {
        rules += assert(
          binary("=>",
            binary("and",
              occurs(rt1),
              occurs(rt2)
            ),
            binary("=",
              binary("<", seq(rt1), seq(rt2)),
              binary("<", mat(rt1), mat(rt2))
            )
          )
        )
      }
    }



    rules += "(check-sat)"
    rules += "(get-model)"
    rules.mkString("\n")
  }
}
