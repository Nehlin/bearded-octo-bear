import yautomaton.automaton._
import scala.xml._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object Y {

  def main(args:Array[String]) {

    val xml = XML.loadFile("ABP.xml")
    val process = (xml \\ "protocol" \\ "role").head

    // Creates the states and adds them to constructionMap
    val automaton = YAutomaton.fromXml(process)

    Files.write(Paths.get("graph.dot"), automaton.toDot.getBytes(StandardCharsets.UTF_8))
  }
}