package automaton

object Dot {
  def highlightScc(automaton: Automaton) : String = {
    val scc = automaton.scc
    val highlighted = scc.map(automaton.transitionsInSet).flatten

    def transitionFun(transition: Transition): String = {
      if (highlighted.contains(transition)) {
        fatTransition(transition)
      } else {
        dottedTransition(transition)
      }
    }

    make(automaton, transitionFun, normalState)
  }

  def makeHl2(automaton: Automaton, transitions: Set[Transition]): String = {
    val transitionStrings = transitions.map(_.toString)

    def transitionFun(transition: Transition): String = {
      if (transitionStrings.contains(transition.toString)) {
        redTransition(transition)
      } else {
        normalTransition(transition)
      }
    }

    make (automaton, transitionFun, normalState)
  }

  def makeHighlighted(automaton: Automaton, transitions: Set[Transition], reachableStates: Option[Set[String]]) : String = {
    val transitionStrings = transitions.map(_.toString)

    def transitionFun(transition: Transition): String = {
      val state = automaton.from(transition)

      if (state.index.isEmpty || transitionStrings.contains(transition.toString)) {
        normalTransition(transition)
      } else {
        if (state.index.get % 2 == 0)
          makeTransition(transition, None, Some("red"))
        else
          makeTransition(transition, None, Some("green"))
      }
    }

    def stateFun(state: State, automaton: Automaton): String = {
      val nameSplit = state.name.toString.split('_')
      if (nameSplit.length > 1) {
        val style = if (reachableStates.isEmpty || reachableStates.get.contains(state.name)) {
          Some("bold")
        } else {
          Some("dotted")
        }
        val stateIndex = nameSplit(1).toInt
        if (stateIndex % 2 == 0)
          makeState(state, automaton, style, Some("red"))
        else
          makeState(state, automaton, style, Some("green"))
      } else
        normalState(state, automaton)
    }

    make(automaton, transitionFun, stateFun)
  }

  def make(automaton: Automaton) : String = {
    make(automaton, normalTransition, normalState)
  }

  def make(automaton: Automaton,
           transitionFunction: ((Transition) => String),
           stateFunction: ((State, Automaton) => String)) : String = {

    var s = "digraph Automaton {\n"
    s = s + "  rankdir = LR;\n"
    val states = automaton.sortedStates

    for (state <- states) {
      s = s + stateFunction(state, automaton)

      val transitions = automaton.sortedTransitions(state)

      for (transition <- transitions) {
        s = s + transitionFunction(transition)
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


    if (automaton.initialState.isDefined && state == automaton.initialState.get) {
      s += "  initial [shape=plaintext,label=\"\"];\n"
      s += "  initial -> " + state.name + "\n"
    }
    s
  }

  private def dottedTransition(transition: Transition) : String = {
    makeTransition(transition, Some("dashed"), None)
  }

  private def redTransition(transition: Transition): String = {
    makeTransition(transition, None, Some("orange"))
  }

  private def fatTransition(transition: Transition) : String = {
    makeTransition(transition, Some("bold"), None)
  }

  private def normalTransition(transition: Transition) : String = {
    makeTransition(transition, None, None)
  }

  private def makeTransition(transition: Transition,
                             style: Option[String],
                             colour: Option[String]) : String  = {

    val from = transition.from
    val to = transition.to
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