package automaton

object Dot {
  def highlightScc(automaton: Automaton) : String = {
    val scc = automaton.scc
    val highlighted = scc.map(Automaton.transitionsInSet).flatten

    makeHighlighted(automaton, highlighted)
  }

  def makeHighlighted(automaton: Automaton, transitions: Set[Transition]) : String = {
    val transitionStrings = transitions.map(_.toString)

    def transitionFun(state: State, transition: Transition): String = {
      if (transitionStrings.contains(transition.toString)) {
        normalTransition(state, transition)
      } else {
        if (transition.from.name.split('_')(1).toInt % 2 == 0)
          makeTransition(state, transition, None, Some("red"))
        else
          makeTransition(state, transition, None, Some("green"))
      }
    }

    def stateFun(state: State, automaton: Automaton): String = {
      val stateIndex = state.name.toString.split('_')(1).toInt
      if (stateIndex % 2 == 0)
        makeState(state, automaton, None, Some("red"))
      else
        makeState(state, automaton, None, Some("green"))
    }

    make(automaton, transitionFun, stateFun)
  }

  def make(automaton: Automaton) : String = {
    make(automaton, normalTransition, normalState)
  }

  def make(automaton: Automaton,
                    transitionFunction: ((State, Transition) => String),
                    stateFunction: ((State, Automaton) => String)) : String = {

    var s = "digraph Automaton {\n"
    s = s + "  rankdir = LR;\n"
    val states = automaton.sortedStates

    for (state <- states) {
      s = s + stateFunction(state, automaton)

      val transitions = state.sortedTransitions

      for (transition <- transitions) {
        s = s + transitionFunction(state, transition)
      }
    }
    s + "}\n"
  }

  private def normalState(state: State, automaton: Automaton): String = {
    makeState(state, automaton, None, None)
  }

  private def makeState(state: State,
                        automaton: Automaton,
                        style: Option[String],
                        colour: Option[String]): String = {

    var s = "  " + state.name +
      " [shape=circle"

    if (style.isDefined)
      s+= ",style=" + style.get
    if (colour.isDefined)
      s+= ",color=" + colour.get

    s += ",label=\"" +
      state.name +
      "\"];\n"


    if (state == automaton.initialState) {
      s += "  initial [shape=plaintext,label=\"\"];\n"
      s += "  initial -> " + state.name + "\n"
    }
    s
  }

  private def dottedTransition(fromState: State, transition: Transition) : String = {
    makeTransition(fromState, transition, Some("dashed"), None)
  }

  private def fatTransition(fromState: State, transition: Transition) : String = {
    makeTransition(fromState, transition, Some("bold"), None)
  }

  private def normalTransition(fromState: State, transition: Transition) : String = {
    makeTransition(fromState, transition, None, None)
  }

  private def makeTransition(fromState: State,
                             transition: Transition,
                             style: Option[String],
                             colour: Option[String]) : String  = {
    val from = fromState.name
    val to = transition.to.name
    val label = transition.condition

    (style, colour) match {
      case (Some(styleString), Some(colourString)) =>
        "  " + from + " -> " + to + " [label=\"" + label + "\"" +
          ",style=\"" + styleString + "\"" +
          ",color=\"" + colourString + "\"" +
          "]\n"
      case (Some(styleString), None) =>
        "  " + from + " -> " + to + " [label=\"" + label + "\"" +
          ",style=\"" + styleString + "\"" +
          "]\n"
      case (None, Some(colourString)) =>
        "  " + from + " -> " + to + " [label=\"" + label + "\"" +
          ",color=\"" + colourString + "\"" +
          "]\n"
      case (None, None) =>
        "  " + from + " -> " + to + " [label=\"" + label + "\"]\n"
    }
  }
}