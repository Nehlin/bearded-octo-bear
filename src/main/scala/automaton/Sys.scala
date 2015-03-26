package automaton

import scala.xml._

class Sys(xmlRoot:Node) {

  private val roles = xmlRoot \\ "protocol" \\ "role"
  private val rawAutomatons = roles.map(Automaton.fromXml).sortBy(automaton => automaton.automatonName)

  private var index = 0
  val (automatonsNormalisedNames, translationMaps) = rawAutomatons.map(rawAutomaton => {
    val (normalised, translationMap, numStates) = rawAutomaton.normaliseNames(index)
    index += numStates
    (normalised, translationMap)
  }).unzip


  private def makeTransitionTranslationMaps(automatons: Seq[Automaton]): (Map[String, String], Map[String, String]) = {
    val allTransitions = automatons.foldLeft(Set[Transition]())((ack, automaton) => {
      ack ++ automaton.transitions
    })
    val allConditions = allTransitions.filter(transition => !transition.condition.isInstanceOf[Nop]).map(_.condition)

    val (channelSet, messageSet) = allConditions.foldLeft(Set[(String, String)]())((ack, condition) => {
      condition match {
        case Send(c, m) => ack + ((c, m))
        case Receive(c, m) => ack + ((c, m))
      }
    }).unzip

    val channels = channelSet.toArray.sorted
    val messages = messageSet.toArray.sorted

    val channelMap = channels.zipWithIndex.map { case (name, ind) => (name, Util.makeIdentifier(ind))}.toMap
    val messageMap = messages.zipWithIndex.map { case (name, ind) => (name, Util.makeIdentifier(ind))}.toMap
    (channelMap, messageMap)
  }
  val (channelMap, messageMap) = makeTransitionTranslationMaps(automatonsNormalisedNames)

  val automatons = automatonsNormalisedNames.map(_.renameTransitions(channelMap, messageMap))

  def transitionVarName(transition: Transition): String = {

    val tString = transition.condition match {
      case Send(channel, message) => channel + "_Snd_" + message
      case Receive(channel, message) => channel + "_Rcv_" + message
      case _ => "Nop"
    }

    transition.from + "_" + tString + "_" + transition.to
  }

  def transitionFromVarName(varName: String): Transition = {

    def getStateComponents(arr: Array[String]): (String, Option[Int]) = {
      if (arr.length == 1) {
        (arr(0), None)
      } else {
        (arr(0), Some(arr(1).toInt))
      }
    }

    val split = varName.split('_')
    if (split.contains("Nop")) {
      val nopPos = split.indexOf("Nop")

      val fromArr = split.slice(0, nopPos)
      val toArr = split.slice(nopPos + 1, split.length)
      val (fromName, fromIndex) = getStateComponents(fromArr)
      val (toName, toIndex) = getStateComponents(toArr)

      new Transition(fromName, fromIndex, toName, toIndex, Nop())
    }
    else {
      val pos = if (split.contains("Rcv")) {
        split.indexOf("Rcv")
      } else {
        split.indexOf("Snd")
      }

      val fromArr = split.slice(0, pos - 1)
      val toArr = split.slice(pos + 2, split.length)
      val (fromName, fromIndex) = getStateComponents(fromArr)
      val (toName, toIndex) = getStateComponents(toArr)
      val channel = split(pos - 1)
      val message = split(pos + 1)

      val transitionCondition = if (split.contains("Snd")) {
        Send(channel, message)
      } else {
        Receive(channel, message)
      }
      new Transition(fromName, fromIndex, toName, toIndex, transitionCondition)
    }
  }

  def automatonsWithPhases(p: Int): List[Automaton] = {
    //List(Phase.makeCondensed(automatons.head, p))
    automatons.map(automaton => Phase.makeCondensed(automaton, p)).toList
  }
}
