package sentiment

import scala.::

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
    line.replaceAll("\\d+", "").toLowerCase().split("\\W+").toList
  }

  def getAllWords(l: List[(Int, String)]): List[String] = {
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */
    l.flatMap(x => getWords(x._2)).filter(s => s.nonEmpty)
  }

  def countWords(l: List[String]): List[(String, Int)] = {
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */
    l.groupBy(identity)
      .map { case (word, occurrences) => (word, occurrences.length) }
      .toList
  }

  /** ********************************************************************************************
    *
    * Aufgabe 2
    *
    * ********************************************************************************************
    */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = {
    l.flatMap(x => getWords(x._2).filter(s => s.nonEmpty).map(i => (x._1, i)))
  }

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    l.foldLeft(Map[String, List[Int]]())((a,b) => a.updated(b._2, b._1 :: a.getOrElse(b._2, Nil)))
      .mapValues(_.reverse).toMap
  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    ???
  }

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = ???
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