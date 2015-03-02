// TODO: rename states to ensure collision free behaviour from _(index), _tmp, etc

import automaton._

import scala.xml._

import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object Y {

  def main(args:Array[String]) {

    val filename = if (args.length > 0) {
      args(0)
    } else {
      "sample_xml/simple_scc.xml"
      //"sample_xml/scc_example.xml"
    }


    val xml = XML.loadFile(filename)
    val process = (xml \\ "protocol" \\ "role").head

    val original = Automaton.fromXml(process)
    val normalised = original.normaliseNames
    Files.write(Paths.get("dot/0original.dot"), Dot.make(original).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/0normalised.dot"), Dot.make(normalised).getBytes(StandardCharsets.UTF_8))

    val (combined, transitions) = Phase.makeCondensedPrint(original, 4, List(2, 1))
    val (condensedSend, _) = Phase.condenseSendAutomaton(original.sendCopy)
    val reachable = combined.reachableCopy

    val reachableNames = Some(combined.reachableStates.map(_.name))

    Files.write(Paths.get("dot/1original.dot"), Dot.make(original).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/2send.dot"), Dot.make(original.sendCopy).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/3send_scc.dot"), Dot.highlightScc(original.sendCopy).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/4send_condensed.dot"), Dot.make(condensedSend).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/5combined.dot"), Dot.makeHighlighted(combined, transitions, reachableNames).getBytes(StandardCharsets.UTF_8))
    Files.write(Paths.get("dot/6combined_reachable.dot"), Dot.makeHighlighted(reachable, transitions, None).getBytes(StandardCharsets.UTF_8))
  }
}