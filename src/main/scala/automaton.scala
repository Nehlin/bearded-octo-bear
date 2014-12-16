package automaton.automaton

import scala.collection.mutable.Set
import scala.xml.{Node, NodeSeq}

import automaton.state.State
import automaton.transition._

class Automaton {

  var initialState : State = _

  def traverse (ackSet:scala.collection.mutable.Set[State], currentState:State):Unit = {

    if (!ackSet.contains(currentState)) {

      ackSet.add(currentState)
      println("adding " + currentState)

      for (t <- currentState.transitions) {
        traverse(ackSet, t.to)
      }
    }
  }

  def states : Set[State] = {
    val ackSet = scala.collection.mutable.Set[State]()
    traverse(ackSet, initialState)
    ackSet
  }

  def sortedStates : Array[State] = states.toArray.sortBy(_.name)

  override def toString = {
    val states = sortedStates

    val str = "initial state: " + initialState.name + "\n"
    println(states)
    val stateString = (for (state <- states) yield state.toString).mkString

    str + stateString
  }

  def toDot = {
    var b = "digraph Automaton {\n"
    b = b + "  rankdir = LR;\n"
    val states = sortedStates

    for (s <- states) {
      b = b + "  " + s.name
      b = b + " [shape=circle"
      b = b + ",label=\"" + s.name + "\"];\n"
      if (s == initialState) {
        b = b + "  initial [shape=plaintext,label=\"\"];\n"
        b = b + "  initial -> " + s.name + "\n"
      }

      val transitions = s.sortedTransitions

      for (t <- transitions) {
        b = b +
          "  " +
          s.name +
          " -> " +
          t.to.name +
          " [label=\"" +
          t.condition +
          "\"]\n"
      }
    }
    b + "}\n"
  }

}

object Automaton {
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

  def fromXml(process:Node) : Automaton = {

    val states = process \ "states" \ "state"
    val stateMap = states.map(s => (s.child.text, new State(s.child.text))).toMap


    val rules = process \\ "rule"
    for (rule <- rules) {

      val fromStateName = (rule \\ "pre" \\ "current_state").head.child.text
      val toStateName = (rule \\ "post" \\ "next_state").head.child.text

      val fromState = stateMap(fromStateName)
      val toState = stateMap(toStateName)

      val tRule = getRule(rule)
      val transition = new Transition(toState, tRule)

      fromState.addTransition(transition)
    }

    val automaton = new Automaton()
    val initialState = stateMap(getInitialStateName(states))
    automaton.initialState = initialState
    automaton
  }
}