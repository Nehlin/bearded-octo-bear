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
    val split = varName.split('_')
    if (split.length == 3) {
      new Transition(split(0), split(2), new Nop())
    } else {
      val fromName = split(0)
      val toName = split(4)
      val channel = split(1)
      val message = split(3)
      val transitionCondition = if (split(2) == "Snd") {
        new Send(channel, message)
      } else {
        new Receive(channel, message)
      }
      new Transition(fromName, toName, transitionCondition)
    }
  }

  def automatonsWithPhases(p: Int): List[Automaton] = {
    List(Phase.makeCondensed(automatons.head, p))
    //automatons.map(automaton => Phase.makeCondensed(automaton, p)).toList
  }
}
