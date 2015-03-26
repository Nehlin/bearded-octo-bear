// TODO: rename states to ensure collision free behaviour from _(index), _tmp, etc

import automaton._

import scala.xml._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._
import scala.sys.process._
import scala.io.Source

object Y {

  def readConfig(): Map[String, String] = {
    val configLines = Files.readAllLines(Paths.get("config.txt"), StandardCharsets.UTF_8).toList
    configLines.map(line => {
      val keyValue = line.split("=")
      (keyValue(0), keyValue(1))
    }).toMap
  }

  def readArgs(args:Array[String]): Map[String, String] = {
    // Special case, the last input argument is the input file.
    if (args.length == 0) {
      Map[String, String]()
    } else {
      // TODO: fix this
      Map[String, String]()
    }
  }

  def getOption(args:Array[String]): String => Option[String] = {
    val argsConfig = readArgs(args)
    val fileConfig = readConfig()

    def getOpt(optionName: String): Option[String] = {
      if (argsConfig.contains(optionName)) {
        Some(argsConfig(optionName))
      } else if (fileConfig.contains(optionName)) {
        Some(fileConfig(optionName))
      } else {
        None
      }
    }

    getOpt
  }

  def parenInterval(string: String): (Int, Int) = {
    val startPos = string.indexOf('(')
    val afterFirstParen = string.substring(startPos + 1)
    var numParen = 1
    var length = 0

    while(numParen > 0 && length < afterFirstParen.length) {
      val currentChar = afterFirstParen.charAt(length)
      if (currentChar == '(') {
        numParen += 1
      } else if (currentChar == ')') {
        numParen -= 1
      }
      length += 1
    }

    (startPos, startPos + length + 1)
  }

  def getModelStrings(string: String):List[String] = {
    if (!string.contains('(')) {
      List[String]()
    } else {
      val (from, to) = parenInterval(string)
      val currentModel = string.substring(from + 1, to - 1)
      getModelStrings(string.substring(to)) :+ currentModel
    }
  }

  def extractValue(modelString: String): Option[(String, Int)] = {
    val split = modelString.split(' ')
    if (split(0) == "define-fun") {
      val name = split(1)
      val value = if (split(3) == "Bool") {
        if (split(4) == "false") {
          0
        } else {
          1
        }
      } else if (split.length == 5) {
        // Single number
        split(4).toInt
      } else {
        // Negative number of form (- num)
        (split(4) + split(5)).stripPrefix("(").stripSuffix(")").toInt
      }
      Some(name, value)
    } else {
      None
    }
  }

  def parseModel(resultString: String) = {
    val cleanedResult = resultString.replace('\t', ' ').replace('\r', ' ').replace('\n', ' ').replaceAll(" +", " ")

    val (from, to) = parenInterval(cleanedResult)
    val modelString = cleanedResult.substring(from + 1, to - 1)
    getModelStrings(modelString).map(extractValue).flatten.toMap
  }

  def parseResult(resultString: String): Option[Map[String, Int]] = {
    if (resultString.startsWith("sat")) {
      Some(parseModel(resultString))
    } else {
      None
    }
  }

  def readAutomatonFile(fileName: String): (String,
    String,
    Int,
    List[String],
    List[String],
    List[(String, List[String], List[(String, String, String, Option[String], Option[String])], String)]) = {

    val lines = Source.fromFile(fileName).getLines().toList.map(_.trim).filter(l => !l.startsWith("%"))
    val protocol = lines.head
    val protoSplit = protocol.split(':')
    val protoName = protoSplit(0)
    val protoType = protoSplit(1)
    val protoCapacity = protoSplit(2).toInt

    def splitAutomatonLines (l: List[String]): List[List[String]] = {
      if (l.contains(",")) {
        val pos = l.indexOf(",")
        val head = l.take(pos)
        val tail = l.slice(pos + 1, l.length)
        head :: splitAutomatonLines(tail)
      } else {
        List(l)
      }
    }

    def parseLine(line: String): (String, String, String, Option[String], Option[String], Option[String]) = {
      val components = line.split(" ")
      val from = components(0)
      val to = components(2)
      val transition = components(1).stripSuffix(">")

      val (tType, chn, msg) = if (transition.contains("!")) {
        val split = transition.split('!')
        ("s", Some(split(0)), Some(split(1)))
      } else if (transition.contains("?")) {
        val split = transition.split('?')
        ("r", Some(split(0)), Some(split(1)))
      } else {
        ("n", None, None)
      }

      if (components.contains("^")) {
        (from, to, tType, chn, msg, Some(from))
      } else {
        (from, to, tType, chn, msg, None)
      }
    }

    val automatonLines = splitAutomatonLines(lines.tail)
    val automatons = automatonLines.map(aLines => {
      val name = aLines.head

      val transitions = aLines.tail.map(parseLine)
      val states = transitions.map{case (from, to, _, _, _, _) => List(from, to)}.flatten.distinct
      val channels = transitions.map{case (_, _, _, chn, _, _) => chn}.distinct
      val messages = transitions.map{case (_, _, _, _, msg, _) => msg}.distinct
      val startState = transitions.map{case (_, _, _, _, _, initial) => initial}.flatten.head
      val cleanTransitions = transitions.map{case (from, to, tType, chn, msg, _) => (from, to, tType, chn, msg)}
      (name, states, cleanTransitions, channels, messages, startState)
    })

    val channels = automatons.map{case (_, _, _, chn, _, _) => chn}.flatten.flatten
    val messages = automatons.map{case (_, _, _, _, msg, _) => msg}.flatten.flatten
    val cleanedAutomatons = automatons.map
      {case (name, states, transitions, _, _, startState) => (name, states, transitions, startState)}
    (protoName, protoType, protoCapacity, channels, messages, cleanedAutomatons)
  }

  def writeAutomatonFile(protocolName: String,
    protocolType: String,
    protocolCapacity: Int,
    channels: List[String],
    messages: List[String],
    automatons: List[(String, List[String], List[(String, String, String, Option[String], Option[String])], String)])
  : String = {

    val rolesXml = automatons.map{case (name, states, transitions, startState) =>
      val statesXml = states.map(state =>
        if (state == startState) {
          <state type="initial">{state}</state>
        } else {
          <state>{state}</state>
        }
      )

      transitions.map{case (from, to, tType, chn, msg) =>
        <pre></pre>
      }

      <role name={name}>
        <states>
          {statesXml}
        </states>
      </role>
    }

    val messagesXml = <messages>{messages.map(m => <message>{m}</message>)}</messages>
    val channelsXml = <channels>{channels.map(c => <message>{c}</message>)}</channels>

    val x = <protocol name={protocolName} medium={protocolType} capacity={protocolCapacity.toString}>
      {messagesXml}
      {channelsXml}
      {rolesXml}
    </protocol>
    println(x)

    ""
  }

  def main(args:Array[String]) {

    //val (pName, pType, pCapacity, chn, msg, autos) = readAutomatonFile("sample_automatons/test.txt")
    //writeAutomatonFile(pName, pType, pCapacity, chn, msg, autos)
    //return

    val filename = "sample_xml/message_test.xml"
    val inputFile = "result/input.smt2"
    val outputFile = "result/output.txt"
    val outputDot = "result/path.dot"
    val originalDot = "result/original.dot"


    val sys = new Sys(XML.loadFile(filename))
    val presburgerString = Presburger.makePresburger(sys, 1)
    Files.write(Paths.get(inputFile), presburgerString.getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get(originalDot), Dot.make(sys.automatons.head).getBytes(StandardCharsets.UTF_8))

    val optionFun = getOption(args)
    val z3Path = optionFun("z3")
    if (z3Path.isDefined) {
      val invokeCommand = z3Path.get + " " + inputFile
      val z3Result = invokeCommand.!!
      val result = parseResult(z3Result)
      if (result.isDefined) {
        println("satisfiable")
        if (optionFun("pres") == Some("y")) {
          Files.write(Paths.get(outputFile), z3Result.getBytes(StandardCharsets.UTF_8))
        }

        val occurrences = result.get.filter{case (str, int) => str.startsWith("occ_") && int == 1}.map{case (str, _) => str.stripPrefix("occ_")}
        val occTransitions = occurrences.map(sys.transitionFromVarName).toSet
        Files.write(Paths.get(outputDot), Dot.makeHl2(sys.automatons.head, occTransitions).getBytes(StandardCharsets.UTF_8))

        val sequence = result.get.filter{case (str, _) => str.startsWith("seq_")}.toList.sortBy(_._2).map{case (str, _) => str.stripPrefix("seq_")}
        val occurringSequences = sequence.filter(tStr => occurrences.contains(tStr))
        println(occurringSequences)
        val res = result.get
        occurringSequences.map(seq => println(seq, res("seq_" + seq)))

      } else {
        println("unsatisfiable")
      }
    }


    /*
    val xml = XML.loadFile(filename)
    val process = (xml \\ "protocol" \\ "role").head


    val original = Automaton.fromXml(process)
    val (normalised, _, _) = original.normaliseNames(100)
    Files.write(Paths.get("dot/0normalised.dot"), Dot.make(normalised).getBytes(StandardCharsets.UTF_8))

    //val (combined, transitions) = Phase.makeCondensedPrint(original, 5)
    //val (condensedSend, _) = Phase.condenseSendAutomaton(original.sendCopy)
    //val reachable = combined.reachableCopy

    //val reachableNames = Some(combined.reachableStates.map(_.name))

    Files.write(Paths.get("dot/1original.dot"), Dot.make(original).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/2send.dot"), Dot.make(original.sendCopy).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/3send_scc.dot"), Dot.highlightScc(original.sendCopy).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/4send_condensed.dot"), Dot.make(condensedSend).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/5combined.dot"), Dot.makeHighlighted(combined, transitions, reachableNames).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("dot/6combined_reachable.dot"), Dot.makeHighlighted(reachable, transitions, None).getBytes(StandardCharsets.UTF_8))
    */
  }
}