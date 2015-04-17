package automaton

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

import scala.xml._
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{MutableList => MList}

object ConvertXml {
  def xmlToFile(inputFile: String, outputFile: String): Unit = {
    val xml = XML.loadFile(inputFile)
    val protocol = xml \\ "protocol"
    val protocolName = (protocol \ "@name").head.text
    val capacity = (protocol \ "@capacity").head.text
    val roles = protocol \ "role"

    def containsMessage(node: Node): Boolean = {
      (node \ "send_message").nonEmpty || (node \ "received_message").nonEmpty
    }

    def getSendOrReceive(path: String, messageName:String)(node: Node): (String, String) = {
      val channelNode = node \ path \ "channel"
      val channel = if (channelNode.nonEmpty) { channelNode.head.text } else { "" }
      val message = (node \ path \ messageName).head.text
      (channel, message)
    }

    def getSend = getSendOrReceive("post", "send_message")_
    def getReceive = getSendOrReceive("pre", "received_message")_

    def getNames(node: Node): (String, String) = {
      val from = (node \ "pre" \ "current_state").head.text
      val to = (node \ "post" \ "next_state").head.text
      (from, to)
    }

    def makeIntermediateName(states: Set[String], intermediateStates: Set[String]): String = {
      val intermediatePrefix = "intermediate_"
      def name(index: Integer): String = {
        intermediatePrefix + index.toString
      }
      var i = 0
      while (states.contains(name(i)) || intermediateStates.contains(name(i))) {
        i += 1
      }
      name(i)
    }

    def makeTransitionString(from: String, to:String, condition:String): String = {
      from + " " + condition + " " + to
    }

    def sendString(chn: String, msg: String): String = chn + "!" + msg + ">"
    def receiveString(chn: String, msg: String): String = chn + "?" + msg + ">"
    def nopString = ">"
    def initialStateString(state: String) = "> " + state
    def endStateString(state: String) = state + " >"

    val allAutomatonStrings = roles.map(role => {
      val intermediateStates = MSet[String]()
      val transitionStrings = MList[String]()
      val automatonStrings = MList[String]()

      val name = (role \ "@name").head.toString()
      val initialName = (role \ "states" \ "state").filter(node => (node \ "@type").text == "initial").head.text
      val rules = role \ "rule"
      val states = rules.map(rule => {
        val (fromName, toName) = getNames(rule)
        List(fromName, toName)
      }).flatten.toSet

      for (rule <- rules) {
        val pre = (rule \ "pre").head
        val post = (rule \ "post").head
        val (from, to) = getNames(rule)
        if (containsMessage(pre) && containsMessage(post)) {
          val (sndChn, sndMsg) = getSend(rule)
          val (rcvChn, rcvMsg) = getReceive(rule)
          val intermediate = makeIntermediateName(states, intermediateStates.toSet)

          transitionStrings += makeTransitionString(from, intermediate, receiveString(rcvChn, rcvMsg))
          transitionStrings += makeTransitionString(intermediate, to, sendString(sndChn, sndMsg))

          intermediateStates += intermediate
        } else if (containsMessage(post)) {
          val (chn, msg) = getSend(rule)
          transitionStrings += makeTransitionString(from, to, sendString(chn, msg))
        } else if (containsMessage(pre)) {
          val (chn, msg) = getReceive(rule)
          transitionStrings += makeTransitionString(from, to, receiveString(chn, msg))
        } else {
          transitionStrings += makeTransitionString(from, to, nopString)
        }
      }
      automatonStrings += name
      automatonStrings += initialStateString(initialName)
      automatonStrings ++= transitionStrings
      automatonStrings.mkString("\n")
    })
    val completeString = protocolName + ":" + capacity + "\n" +
      allAutomatonStrings.mkString("\n,\n")
    Files.write(Paths.get(outputFile), completeString.getBytes(StandardCharsets.UTF_8))
  }

  /*
  private def getSpecificRule(path: String,
                              messageName: String)
                             (rule:Node): Option[(String, String)] = {

    val post = (rule \ path).head
    val messageNode = post \ messageName
    val channelNode = post \ "channel"
    if (messageNode.isEmpty || channelNode.isEmpty)
      None
    else {
      val message = messageNode.head.text
      val channel = channelNode.head.text
      Some(channel, message)
    }
  }

  private def getSend = getSpecificRule("post", "send_message")_
  private def getReceive = getSpecificRule("pre", "received_message")_

  private def getRule(rule: Node): TransitionCondition = {
    val send = getSend(rule)
    val receive = getReceive(rule)
    (send, receive) match {
      case (Some((channel, message)), _) => Send(channel, message)
      case (_, Some((channel, message))) => Receive(channel, message)
      case _ => Nop()
    }
  }

  /**
   * Creates an automaton from a role-node in the xml-document.
   *
   * @param process A role-node from the xml-document.
   * @return        An automaton matching the data contained in the node.
   */
  def fromXml(process: Node): Automaton = {

    val states = process \ "states" \ "state"
    val stateMap = states.map(s => (s.child.text, new State(s.child.text, None))).toMap

    val rules = process \\ "rule"
    val transitions = rules.map(rule => {
      val fromStateName = (rule \\ "pre" \\ "current_state").head.child.text
      val toStateName = (rule \\ "post" \\ "next_state").head.child.text

      val fromState = stateMap(fromStateName)
      val toState = stateMap(toStateName)

      val tRule = getRule(rule)
      new Transition(fromState, toState, tRule)
    }).toSet

    val initialStateName = getInitialStateName(states)

    val automatonName = (process \ "@name").toString()
    new Automaton(stateMap, transitions, Some(initialStateName, None), None, automatonName)
  }*/
}
