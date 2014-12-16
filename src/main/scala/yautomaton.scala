package yautomaton.automaton

import dk.brics.automaton.Automaton
import yautomaton.state._
import yautomaton.transition._

import scala.xml.{Node, NodeSeq}

class YAutomaton extends Automaton {

  override def toString = {
    val initialState = getInitialState.asInstanceOf[YState]
    val states = getStates.toArray.sortBy(_.asInstanceOf[YState].name)

    val str = "initial state: " + initialState.name + "\n"
    val stateString = (for (state <- states) yield state.toString).mkString

    str + stateString
  }

  override def toDot = {
    var b = "digraph Automaton {\n"
    b = b + "  rankdir = LR;\n"
    val states = getStates.toArray.sortBy(_.asInstanceOf[YState].name)

    for (s <- states) {
      val state = s.asInstanceOf[YState]
      b = b + "  " + state.name
      if (state.isAccept)
        b = b + " [shape=doublecircle"
      else
        b = b + " [shape=circle"
      b = b + ",label=\"" + state.name + "\"];\n"
      if (state == getInitialState) {
        b = b + "  initial [shape=plaintext,label=\"\"];\n"
        b = b + "  initial -> " + state.name + "\n"
      }

      val transitions = state.getTransitions.toArray.sortBy(_.asInstanceOf[YTransition].toString)

      for (t <- transitions) {
        val transition = t.asInstanceOf[YTransition]
        b = b +
          "  " +
          state.name +
          " -> " +
          transition.getDest.asInstanceOf[YState].name +
          " [label=\"" +
          transition.condition +
          "\"]\n"
      }
    }
    b + "}\n"
  }
}

object YAutomaton {
  private def getInitialStateName(states:NodeSeq) : String = {
    if (states.isEmpty)
      throw new RuntimeException("No initial state found")
    else if ((states.head \\ "@type").text == "initial")
      states.head.child.text
    else
      getInitialStateName(states.tail)
  }

  private def getSpecificRule(path:String, messageName:String)(rule:Node) : Option[(String, String)] = {
    val post = (rule \ path).head
    val messageNode = (post \ messageName)
    val channelNode = (post \ "channel")
    if (messageNode.isEmpty || messageNode.isEmpty)
      None
    else {
      val message = messageNode.head.text
      val channel = channelNode.head.text
      Some(channel, message)
    }
  }

  private def getSend = getSpecificRule("post", "send_message")_
  private def getReceive = getSpecificRule("pre", "received_message")_

  private def getRule(rule:Node) : TransitionCondition = {
    val send = getSend(rule)
    val receive = getReceive(rule)
    if (!send.isEmpty) {
      Send(send.get._1, send.get._2)
    } else if (!receive.isEmpty) {
      Receive(receive.get._1, receive.get._2)
    } else {
      Nop()
    }
  }

  def fromXml(process:Node) : YAutomaton = {

    val states = process \ "states" \ "state"
    val stateMap = states.map(s => (s.child.text, new YState(s.child.text))).toMap

    val rules = process \\ "rule"
    for (rule <- rules) {

      val fromStateName = (rule \\ "pre" \\ "current_state").head.child.text
      val toStateName = (rule \\ "post" \\ "next_state").head.child.text

      val fromState = stateMap(fromStateName)
      val toState = stateMap(toStateName)

      val tRule = getRule(rule)
      val transition = new YTransition(toState, tRule)

      fromState.addTransition(transition)
    }

    val automaton = new YAutomaton()
    val initialState = stateMap(getInitialStateName(states))
    automaton.setInitialState(initialState)
    automaton
  }
}