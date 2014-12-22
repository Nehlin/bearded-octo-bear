import automaton.Automaton
import automaton.Dot

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
    val original = Automaton.fromXml(process)

    val sendPart = original.sendCopy(1)
    val receivePart = original.receiveCopy(2)
    val sendPart2 = original.sendCopy(3)
    val receivePart2 = original.receiveCopy(4)

    val receiveTransitions = original.receiveTransitions
    val sendTransitions = original.sendTransitions
    val (combined, transitions1)  = Automaton.combine(sendPart, receivePart, receiveTransitions)
    val (combined2, transitions2) = Automaton.combine(combined, sendPart2, sendTransitions)
    val (combined3, transitions3) = Automaton.combine(combined2, receivePart2, receiveTransitions)

    val transitions = transitions1 ++ transitions2 ++ transitions3
    println(transitions)

    Files.write(Paths.get("graph_original.dot"), Dot.make(original).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("graph_send.dot"), Dot.make(sendPart).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("graph_receive.dot"), Dot.make(receivePart).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("graph_combined.dot"), Dot.makeHighlighted(combined3, transitions).getBytes(StandardCharsets.UTF_8))
    //Files.write(Paths.get("graph_copy.dot"), Dot.highlightScc(receivePart).getBytes(StandardCharsets.UTF_8))
  }
}