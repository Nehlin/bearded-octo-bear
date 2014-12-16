import scala.xml._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets


object Y {

  def main(args:Array[String]) {

    val filename = if (args.length > 0) {
      args(0)
    } else {
      "sample_xml/simple.xml"
    }

    val xml = XML.loadFile(filename)
    val process = (xml \\ "protocol" \\ "role").head

    // Creates the states and adds them to constructionMap
    val automaton = Automaton.fromXml(process)

    println(automaton)

    Files.write(Paths.get("graph.dot"), automaton.toDot.getBytes(StandardCharsets.UTF_8))
  }
}