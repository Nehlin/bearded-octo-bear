import java.io.ByteArrayInputStream
import scala.collection.mutable.{Set => MSet}

import automaton._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._
import scala.sys.process._
import scala.collection.mutable.{MutableList => MList}

object Y {

  def requiredSettings = List[(String, String)](
    ("z3", "path/to/z3\tlocation of the z3 binary"),
    ("inputFile", "path/to/input_file\tlocation of file describing the processes"),
    ("numPhases", "x\tnumber of phases to test for")
  )

  def defaultSettings = List[(String, (String, String))](
    ("outputDir", ("result", "path/to/output_dir\tdirectory where output files are written. will be created if not found")),
    ("outputPresburger", ("n", "generated presburger will be written to disk")),
    ("outputOriginalDot", ("n", ".dot-file for original automatons will be generated")),
    ("outputPhaseDot", ("n", ".dot-file for the phase-automatons will be generated")),
    ("outputModel", ("n", "the model will be generated if it exists")),
    ("outputPath", ("n", "a .dot-file containing a highlighted path to the invalid state will be generated")),
    ("outputSequence", ("n", "a sequence of transitions leading to the invalid state is generated")),
    ("outputMap", ("n", "outputs a map to translate the normalised names back to original names")),
    ("isPushdown", ("n", "treats the input as a pushdown automaton. otherwise the automaton will be treated as a NFA")),
    ("clearResultDir", ("y", "removes all files in the result directory before writing new files"))
  )

  def printHelp = {

    def writeHelpLine(flag: String, example: String, desc: String): String = {
      "    -" + flag + " " + example + "  -  " + desc
    }

    def requiredString(flag: String, description: String): String = {
      val sp = description.split("\t")
      val example = sp(0)
      val desc = sp(1)

      writeHelpLine(flag, example, desc)
    }

    def optionalString(optional:(String, (String, String))): String = {
      val (flag, (default, description)) = optional

      val (example, desc) = if (description.contains("\t")) {
        val sp = description.split("\t")
        (sp(0), sp(1))
      } else {
        ("y/n", "iff y, " + description)
      }

      writeHelpLine(flag, example + " [" + default + "]", desc)
    }

    // TODO: update Y to leerbook once settled on name
    println("Usage: Y -key1 value1 -key2 value2 ...")
    println("")

    println("Values specified in the config file (config.txt) are overwritten by values sent via the command line.")
    for ((flag, description) <- requiredSettings) {
      println(requiredString(flag, description))
    }
    println("")

    println("List of optional parameters (default value shown in [brackets]):")
    for (setting <- defaultSettings) {
      println(optionalString(setting))
    }
  }

  def readConfig(): Map[String, String] = {
    val configLines = Files.readAllLines(Paths.get("config.txt"), StandardCharsets.UTF_8).toList
    configLines.map(line => {
      val keyValue = line.split("=")
      (keyValue(0), keyValue(1))
    }).toMap
  }

  def readArgs(args:Array[String]): Map[String, String] = {
    // TODO: fix this
    Map[String, String]()
  }

  def getOption(args:Array[String]): String => Option[String] = {
    val argsConfig = readArgs(args)
    val fileConfig = readConfig()

    val default = defaultSettings.toMap

    def getOpt(optionName: String): Option[String] = {
      if (argsConfig.contains(optionName)) {
        Some(argsConfig(optionName))
      } else if (fileConfig.contains(optionName)) {
        Some(fileConfig(optionName))
      } else if (default.contains(optionName)) {
        Some(default(optionName)._1)
      } else {
        None
      }
    }

    getOpt
  }

  def translateToBool(strVal: String): Boolean = {
    strVal == "y"
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

    Files.write(Paths.get(resultDir + auto.automatonName + "_original.dot"), Dot.make(auto).getBytes(StandardCharsets.UTF_8))

    val (rules, condensedDependencyGraph, translationMap, ruleMap, intermediateAutomatons) =
      Stack.pushdownToNfaCreateHelpDataSend(auto)

    Files.write(Paths.get(resultDir + condensedDependencyGraph.automatonName + "_dep.dot"), Dot.make(condensedDependencyGraph).getBytes(StandardCharsets.UTF_8))

    val (depAuto, _) = Stack.dependencyGraph(auto)
    Files.write(Paths.get(resultDir + depAuto.automatonName + "_dep.dot"), Dot.highlightScc(depAuto).getBytes(StandardCharsets.UTF_8))

    val wordsBetween = Stack.pushdownToNfaCreateHelpDataReceive(auto)
    val (states, transitions) = Stack.makeReceiveBetweenPair("E", "C", wordsBetween).get
    val ax = new Automaton(states.toSet, transitions.toSet, None, "apekatt!")
    Files.write(Paths.get(resultDir + ax.automatonName + "_rec.dot"), Dot.make(ax).getBytes(StandardCharsets.UTF_8))

    /*
    if (outputOriginalDot) {
      for (auto <- autos) {
        //Files.write(Paths.get(resultDir + auto.automatonName + "_original.dot"), Dot.make(auto).getBytes(StandardCharsets.UTF_8))
      }
    }
    */

  }

  def main(args:Array[String]) {

    printHelp

    // TODO: handle initial state being removed


    val optionFun = getOption(args)

    def getSettingOrExitWithError(settingName: String, errorMsg: String): String = {
      val configOption = optionFun(settingName)
      if (!configOption.isDefined) {
        Console.err.println(errorMsg)
        scala.sys.exit(-1)
      }
      configOption.get
    }

    def getSetting(settingName: String): String = {
      optionFun(settingName).get
    }

    def getSettingBool(settingName: String): Boolean = {
      translateToBool(getSetting(settingName))
    }

    // required options
    val z3Path = getSettingOrExitWithError("z3", "z3 path is not set. (-z3 path/to/z3)")
    val inputFile = getSettingOrExitWithError("inputFile", "no input file specified. (-inputFile path/to/input_file)")
    val numPhases = getSettingOrExitWithError("numPhases", "number of phases not specified. (-numPhases x)").toInt

    // optional options (default values exist if the user do not specify them)
    val resultDirRaw = getSetting("outputDir")
    val resultDir = resultDirRaw + (if (resultDirRaw.endsWith("/")) "" else "/")
    val outputPresburger = getSettingBool("outputPresburger")
    val outputOriginalDot = getSettingBool("outputOriginalDot")
    val outputPhaseDot = getSettingBool("outputPhaseDot")
    val outputModel = getSettingBool("outputModel")
    val outputPath = getSettingBool("outputPath")
    val outputSequence = getSettingBool("outputSequence")
    val outputMap = getSettingBool("outputMap")
    val isPushdown = getSettingBool("isPushdown")
    val clearResultDir = getSettingBool("clearResultDir")

    val dirPath = Paths.get(resultDir)
    if (!Files.exists(dirPath)) {
      Files.createDirectory(dirPath)

    }

    if (isPushdown) {
      pushdownReachability(inputFile, z3Path, numPhases, resultDir, outputOriginalDot, outputPresburger, outputModel, outputMap)
    } else {
      dfaReachability(inputFile, z3Path, numPhases, resultDir, outputOriginalDot, outputPhaseDot,
        outputPresburger, outputPath, outputSequence, outputMap)
    }

  }
}