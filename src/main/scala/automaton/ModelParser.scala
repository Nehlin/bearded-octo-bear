package automaton

object ModelParser {
  def parseZ3(modelString: String) = {
    val lines = modelString.split('\n').map(line => line.trim).filter(line => !line.startsWith(";;")).toList
    val fullString = lines.mkString(" ").replace('\t', ' ').replace('\r', ' ').replace('\n', ' ').replaceAll(" +", " ")

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

    val (from, to) = parenInterval(fullString)
    val ms = fullString.substring(from + 1, to - 1)
    val statements = getModelStrings(ms)


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

    val validStatements = statements.filter(line =>
      line.startsWith("define-fun occ_") ||
      line.startsWith("define-fun seq_") ||
      line.startsWith("define-fun match_")
    )

    validStatements.map(extractValue).flatten
  }
}
