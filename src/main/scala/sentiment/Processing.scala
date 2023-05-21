package sentiment


class Processing {

  /** ********************************************************************************************
   *
   * Aufgabe 1
   *
   * ********************************************************************************************
   */
  def getWords(line: String): List[String] = {
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    line
      .replaceAll("[^A-za-z]", " ")
      .split(" ")
      .map(_.toLowerCase)
      .filter(x => x.nonEmpty)
      .toList
  }

  def getAllWords(l: List[(Int, String)]): List[String] = {
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */
    l.flatMap(tuple => getWords(tuple._2))
  }

  def countWords(l: List[String]): List[(String, Int)] = {
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */
    // group by word
    l.groupBy(identity)
      // mapValues on Map is deprecated
      .view
      .mapValues(_.size)
      .toList
  }

  /** ********************************************************************************************
   *
   * Aufgabe 2
   *
   * ********************************************************************************************
   */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    l.flatMap({
      case (lineNum, line) => getWords(line).map(word => (lineNum, word))
    })
  }

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    // group by word
    l.groupBy(tuple => tuple._2)
      // map view in order to get mapValues
      .view
      .mapValues(_.map(_._1))
      .toMap
  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    // for each word look it up in invInd and return the list or Nil
    // afterwards flatten all lists into a single list
    words.flatMap(invInd.getOrElse(_, Nil).distinct)
  }

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    words.foldLeft(invInd.values.flatten.toSet)((conjunction, string) => {
      conjunction.intersect(invInd.getOrElse(string, Nil).toSet)
    }).toList
  }
}


object Processing {

  def getData(filename: String): List[(Int, String)] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20", " "))
    val iter = src.getLines()
    var c = -1
    val result = (for (row <- iter) yield {
      c = c + 1; (c, row)
    }).toList
    src.close()
    result
  }
}