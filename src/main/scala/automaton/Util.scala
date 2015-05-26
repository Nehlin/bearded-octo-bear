package automaton

object Util {
  /**
   * Generates a unique and safe state name from an index.
   *
   * States should be renamed to ensure proper behaviour when generating new states. An automaton requires state names
   * to be unique. Some new state names are automatically generated from existing ones and to ensure this does not
   * cause name collisions, the names must follow a certain format. In order not to impose this limitation on the input
   * format, the names are change.
   *
   * An example when this could otherwise cause a problem would be in the k-saturation. Consider a state called A in
   * the XML. While working with generating the final automaton, this becomes a state with index 1. The name of this
   * state would be A_1. Suppose that we k-saturating this state with k = 2, generating the states A0_1,
   * A1_1. If another state from the XML would already be called A_1, A0_1 or A1_1, this would cause duplicate states.
   *
   * @param i Natural number representing the index of a state. Each number creates a unique state name
   * @return  Unique and safe name for the index. Names are of the form A...Z, Aa...Az, Ba...Bz
   */
  def makeIdentifier(i: Int): String = {
    val range = 'a' to 'z'

    if (i < range.length) {
      range(i).toUpper.toString
    } else {
      makeIdentifier(i / range.length - 1) + range(i % range.length)
    }
  }

}
