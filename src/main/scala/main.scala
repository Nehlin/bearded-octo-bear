import java.io.ByteArrayInputStream

import automaton._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._
import scala.sys.process._
import scala.collection.mutable.{MutableList => MList}

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

  def dfaReachability(inputFile: String,
                      z3Path: String,
                      numPhases: Int,
                      resultDir: String,
                      outputOriginalDot: Boolean,
                      outputPhaseDot: Boolean,
                      outputPresburger: Boolean,
                      outputPathDot: Boolean,
                      outputSequence: Boolean,
                      outputMap: Boolean): Unit = {

    val sys = new Sys(Automaton.readAutomatonFile(inputFile))

    if (outputOriginalDot) {
      for (auto <- sys.automatons) {
        Files.write(Paths.get(resultDir + auto.automatonName + "_original.dot"), Dot.make(auto).getBytes(StandardCharsets.UTF_8))
      }
    }

    // To avoid double computing this if its used bot here and in the path
    val phaseResult = if (outputPhaseDot) {
      val (reachable, autos, _) = sys.automatonsWithPhases(numPhases)
      if (reachable) {
        for (auto <- autos) {
          Files.write(Paths.get(resultDir + auto.automatonName + "_phase.dot"), Dot.make(auto).getBytes(StandardCharsets.UTF_8))
        }
        Some(autos)
      } else {
        println("End state is unreachable, phase automatons not printed.")
        None
      }
    } else {
      None
    }

    val presburgerResult = Presburger.makePresburger(sys, numPhases)
    if (presburgerResult.isEmpty ) {
      println("construction shows target state is unreachable")
      println("unsat")
      scala.sys.exit()
    }

    if (outputPresburger) {
      Files.write(Paths.get(resultDir + "presburger.smt2"), presburgerResult.get.getBytes(StandardCharsets.UTF_8))
    }

    if (outputMap) {
      val stateMapString = sys.translationMaps.map(translationMap => {
        translationMap.keys.map(key => key + " => " + translationMap(key))
      }).flatten.sorted.mkString("\n")

      val channelMapString = sys.channelMap.keys.map(key => key + " => " + sys.channelMap(key)).toList.sorted.mkString("\n")
      val messageMapString = sys.messageMap.keys.map(key => key + " => " + sys.messageMap(key)).toList.sorted.mkString("\n")

      val outputString =
        "States:\n" +
        stateMapString + "\n" +
        "Channels:\n" +
        channelMapString +
        "Messages:\n" +
        messageMapString

      Files.write(Paths.get(resultDir + "map.txt"), outputString.getBytes(StandardCharsets.UTF_8))
    }

    val z3 = List(z3Path, "-in")
    val is = new ByteArrayInputStream(presburgerResult.get.getBytes("UTF-8"))

    val lines = (z3 #< is).lineStream_!
    val model = if (lines.head.trim == "sat") {
      println("sat")
      lines.tail.mkString("\n")
    } else if (lines.head.trim == "unsat") {
      println("unsat")
      scala.sys.exit()
    } else {
      val errors = lines.mkString("\n")
      println("error:\n" + errors)
      scala.sys.exit(-1)
    }

    if (outputPathDot) {
      val phaseAutomatons = phaseResult.getOrElse{
        val (_, autos, _) = sys.automatonsWithPhases(numPhases)
        autos
      }

      val modelMap = ModelParser.parseZ3(model)

      val occurrences = modelMap.filter{case (str, int) => str.startsWith("occ_") && int == 1}.map{case (str, _) => str.stripPrefix("occ_")}
      val occTransitions = occurrences.map(sys.transitionFromVarName).toSet

      for (auto <- phaseAutomatons) {
        Files.write(Paths.get(resultDir + auto.automatonName + "_path.dot"),
          Dot.makeHl2(auto, occTransitions).getBytes(StandardCharsets.UTF_8))
      }
    }

    if (outputSequence) {
      val modelMap = ModelParser.parseZ3(model)
      val occurrences = modelMap.filter{case (str, int) => str.startsWith("occ_") && int == 1}.map{case (str, _) => str.stripPrefix("occ_")}

      val sequence = modelMap.filter{case (str, _) => str.startsWith("seq_")}.toList.sortBy(_._2).map{case (str, _) => str.stripPrefix("seq_")}
      val occurringSequences = sequence.filter(tStr => occurrences.contains(tStr))
      if (occurringSequences.isEmpty) {
        println("no occurring seq, this is probably wrong")
      }


    }
  }

  def pushdownReachability(inputFile: String,
                           z3Path: String,
                           numPhases: Int,
                           resultDir: String,
                           outputOriginalDot: Boolean,
                           outputPresburger: Boolean,
                           outputModel: Boolean,
                           outputMap: Boolean): Unit = {

    val (autos, _, _) = Automaton.readAutomatonFile(inputFile).unzip3
    val auto = autos.head
    Stack.makeGrammar(auto)
    Stack.pushdownToNfa(auto)

    //val (depAuto, _) = Stack.dependencyGraph(auto)
    //Files.write(Paths.get(resultDir + depAuto.automatonName + "_dep.dot"), Dot.highlightScc(depAuto).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get(resultDir + combAuto.automatonName + "_comb.dot"), Dot.highlightScc(combAuto).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get(resultDir + addAuto.automatonName + "_added.dot"), Dot.highlightScc(addAuto).getBytes(StandardCharsets.UTF_8))

    if (outputOriginalDot) {
      for (auto <- autos) {
        Files.write(Paths.get(resultDir + auto.automatonName + "_original.dot"), Dot.make(auto).getBytes(StandardCharsets.UTF_8))
      }
    }

  }

  def main(args:Array[String]) {

    // TODO: handle initial state being removed

    val inputFile = "send_experiments/ex4.txt"
    //val inputFile = "sample_automatons/ABP_F.txt"
    val resultDir = "result/"
    val numPhases = 6

    val optionFun = getOption(args)
    val z3PathOpt = optionFun("z3")
    if (!z3PathOpt.isDefined) {
      println("z3 path is not set")
      scala.sys.exit(-1)
    }
    val z3Path = z3PathOpt.get

    pushdownReachability(inputFile, z3Path, numPhases, resultDir, true, true, true, true)
    //dfaReachability(inputFile, z3Path, numPhases, resultDir, true, false, true, true, true, true)

    //val sys = new Sys(Automaton.readAutomatonFile(inputFile))




    //Files.write(Paths.get(resultDir + auto.automatonName + ".dot"), Dot.make(auto).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get(resultDir + auto.automatonName + "_t.dot"), Dot.makeHl2(x, y).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get(resultDir + auto.automatonName + "_h.dot"), Dot.makeHl2(hoppo, hoppot).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get(resultDir + auto.automatonName + "_r.dot"), Dot.makeHl2(hoppo.reachableCopy, hoppot).getBytes(StandardCharsets.UTF_8))

    return
/*
    val (_, autoWithPhases, _) = sys.automatonsWithPhases(numPhases)
    for (auto <- sys.automatons) {
      Files.write(Paths.get(resultDir + auto.automatonName + "_original.dot"),
        Dot.make(auto).getBytes(StandardCharsets.UTF_8))
    }
    for (auto <- autoWithPhases) {

      Files.write(Paths.get(resultDir + auto.automatonName + "_phase.dot"),
        Dot.make(auto).getBytes(StandardCharsets.UTF_8))
    }

    for (tm <- sys.translationMaps) {
      println(tm)
    }

                                  val presburgerResult = Presburger.makePresburger(sys, numPhases)
                                  if (presburgerResult.isEmpty ) {
                                    println("construction shows target state is unreachable")
                                    println("unsat")
                                    scala.sys.exit()
                                  }
    val presburgerString = presburgerResult.get
    if (optionFun("pres") == Some("y")) {
      Files.write(Paths.get(resultDir + "input.smt2"), presburgerString.getBytes(StandardCharsets.UTF_8))
    }

    val z3 = List(z3Path, "-in")
    val is = new ByteArrayInputStream(presburgerString.getBytes("UTF-8"))

    val lines = (z3 #< is).lineStream_!
    val model = if (lines.head.trim == "sat") {
      println("sat")
      lines.tail.mkString("\n")
    } else if (lines.head.trim == "unsat") {
      println("unsat")
      scala.sys.exit()
    } else {
      val errors = lines.mkString("\n")
      println("error:\n" + errors)
      scala.sys.exit(-1)
    }
*/
    /*
    val modelMap = ModelParser.parseZ3(model)

    val occurrences = modelMap.filter{case (str, int) => str.startsWith("occ_") && int == 1}.map{case (str, _) => str.stripPrefix("occ_")}
    val occTransitions = occurrences.map(sys.transitionFromVarName).toSet

    for (auto <- autoWithPhases) {
      Files.write(Paths.get(resultDir + auto.automatonName + "_path.dot"),
        Dot.makeHl2(auto, occTransitions).getBytes(StandardCharsets.UTF_8))
    }

    val sequence = modelMap.filter{case (str, _) => str.startsWith("seq_")}.toList.sortBy(_._2).map{case (str, _) => str.stripPrefix("seq_")}
    val occurringSequences = sequence.filter(tStr => occurrences.contains(tStr))
    if (occurringSequences.isEmpty) {
      println("no occurring seq, this is probably wrong")
    }
    for (t <- occurringSequences) {
      val (from, to, trans) = if (t.contains("_Nop_")) {
        val sp = t.split("_Nop_")
        (sp(0), sp(1), "")
      } else {
        val (sp, op) = if (t.contains("_Rcv_")) {
          (t.split("_Rcv_"), "?")
        } else {
          (t.split("_Snd_"), "!")
        }
        val fr = sp(0).split('_')
        val to = sp(1).split('_')
        (fr(0) + "_" + fr(1), to(1) + "_" + to(2), fr(2) + " " + op + " " + to(0))
      }
      println(from + " -> " + to + "    " + trans)
    }
    */
  }
}