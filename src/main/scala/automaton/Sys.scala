package automaton

import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}

import scala.xml._

class Sys(automatonsWithEnd: List[(Automaton, List[String], Boolean)]) {

  private val (rawAutomatons, endStates, hasEndStates) = automatonsWithEnd.unzip3

  private var index = 0
  val (automatonsNormalisedNames, translationMaps) = rawAutomatons.map(rawAutomaton => {
    val (normalised, translationMap, numStates) = rawAutomaton.normaliseNames(index)
    index += numStates
    (normalised, translationMap)
  }).unzip

  private def makeTransitionTranslationMaps(automatons: Seq[Automaton]):
    (Map[String, String], Map[String, String], Map[String, String]) = {

    val allTransitions = automatons.foldLeft(Set[Transition]())((ack, automaton) => {
      ack ++ automaton.transitions
    })

    val allConditions = allTransitions.filter(transition =>
      transition.condition.isInstanceOf[Send] || transition.condition.isInstanceOf[Receive]
    ).map(_.condition)

    val allStackConditions = allTransitions.map(t => t.condition).filter(c => c.isInstanceOf[Push] || c.isInstanceOf[Pop])
    val stackSet = allStackConditions.foldLeft(Set[String]())((ack, condition) => {
      condition match {
        case Push(msg) => ack + msg
        case Pop(msg) => ack + msg
      }
    })

    val (channelSet, messageSet) = allConditions.foldLeft(Set[(String, String)]())((ack, condition) => {
      condition match {
        case Send(c, m) => ack + ((c, m))
        case Receive(c, m) => ack + ((c, m))
      }
    }).unzip

    val channels = channelSet.toArray.sorted
    val messages = messageSet.toArray.sorted
    val stackMessages = stackSet.toArray.sorted

    val channelMap = channels.zipWithIndex.map { case (name, ind) => (name, Util.makeIdentifier(ind))}.toMap
    val messageMap = messages.zipWithIndex.map { case (name, ind) => (name, Util.makeIdentifier(ind))}.toMap
    val stackMap = stackMessages.zipWithIndex.map {case (name, ind) => (name, Util.makeIdentifier(ind))}.toMap
    (channelMap, messageMap, stackMap)
  }
  val (channelMap, messageMap, stackMap) = makeTransitionTranslationMaps(automatonsNormalisedNames)

  val automatons = automatonsNormalisedNames.map(_.renameTransitions(channelMap, messageMap, stackMap))
  val endStatesNormalised: List[List[String]] = {
    (endStates, translationMaps, automatons).zipped.toList.map{ case (es, tm, a) =>
      // Map is 1-to-1, so this is legit
      val tmFlipped = tm.keys.map(key => (tm(key), key)).toMap
      es.map(s => {
        tmFlipped(s)
      })
    }
  }

  def endStatesForAutomaton(automaton: Automaton, endStateNames: List[String]): List[State] = {
    endStateNames.map(name => {
      automaton.stateMap(name)
    })
  }

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

  def automatonsWithPhases(p: Int): (Boolean, List[Automaton], List[List[State]]) = {

    val endStateNames = endStatesNormalised.flatten
    val saturationValues = Phase.getSaturationValuesWithTransforms(automatons, endStateNames, p,
      Phase.noTransform, Phase.noTransform, Phase.noTransform)
    val phaseAutomatons = automatons.map(automaton => Phase.makeCondensed(automaton, p, saturationValues))

    // TODO: use function in Phase. investigate the above flatten.
    val phaseEndStates = phaseAutomatons.map{case (auto, condenseMap, endStateMaps) =>

      val phaseEndStateNames = endStateNames.map(name => {
        val condName = condenseMap.getOrElse(name, name)
        endStateMaps.map{case (phase, em) =>
          (phase, em.getOrElse(condName, condName))
        }
      }).flatten

      phaseEndStateNames.map{case (phase, name) =>
        auto.states.filter(state => {
          state.nameString == name && state.index.get == phase
        })
      }.flatten
    }

    val (trimmedAutomatons, trimmedEndStates, unreachable) =
      (phaseAutomatons, phaseEndStates, hasEndStates).zipped.map{case ((auto, _, _), eStates, hasEnd) =>
        val trimmedAutoOption = auto.trimmedCopy(eStates.toSet)
        trimmedAutoOption match {
          case Some(trimmedAuto) =>
            val trimmedStates = trimmedAuto.states
            val trimmedEnd = eStates.filter(es => trimmedStates.contains(es))
            (Some(trimmedAuto), Some(trimmedEnd), false)
          case None =>
            Files.write(Paths.get(auto.automatonName + "_path.dot"),
              Dot.make(auto).getBytes(StandardCharsets.UTF_8))
            (None, None, hasEnd)
        }
    }.unzip3

    val isUnreachable = unreachable.fold(false) {
      (ack, curr) => if (curr) { true } else { ack }
    }

    //(phaseAutomatons.map{case (auto, _, _) => auto}, phaseEndStates)
    (isUnreachable, trimmedAutomatons.flatten, trimmedEndStates.flatten)
  }
}
