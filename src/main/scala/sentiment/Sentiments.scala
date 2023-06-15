package sentiment

import java.awt.{Color, GridLayout}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities

/**
  * @author hendrik
  * modified by akarakochev
  */
class Sentiments(sentiFile: String) {

  val sentiments: Map[String, Int] = getSentiments(sentiFile)

  val proc = new Processing()

  /** ********************************************************************************************
    *
    * Aufgabe 5
    *
    * ********************************************************************************************
    */

  def getDocumentGroupedByCounts(filename: String, wordCount: Int): List[(Int, List[String])] = {
    val input = getTextFileInputAsListOfString(filename)

    def split(ll: List[String], count: Int): List[(Int, List[String])] = ll.splitAt(wordCount) match {
      case (x,Nil) => List((count,x))
      case (x,xs) => (count, x) :: split(xs, count+1)
    }
    split(input,1)
  }

  def getDocumentSplitByPredicate(filename: String, predicate:String=>Boolean): List[(Int, List[String])] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url).toList.mkString
    val in = getWordsNoLowerCase(src)

    def split(ll: List[String], count: Int): List[(Int, List[String])] = {
      val l = ll.dropWhile(!predicate(_)).dropWhile(predicate)
      l match{
        case Nil => Nil
        case _ =>
          (count, l.takeWhile(!predicate(_)).map(x => x.toLowerCase).foldLeft(List[String]()){
            (list, words) => list.appended(words)
          }) :: split(l, count+1)
      }
    }

    split(in, 1)
  }


  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = l match{
    case Nil => Nil
    case x::xs =>
      val knownWords = x._2.flatMap(word => sentiments.get(word));
      (
        x._1 - 1,
        knownWords.reduceLeft((a,b) => a + b).toDouble / knownWords.size,
        knownWords.size.toDouble / x._2.size.toDouble
      ) :: analyseSentiments(xs)
  }


  /** ********************************************************************************************
    *
    * Helper Functions
    *
    * ********************************************************************************************
    */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20"," "))
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel:String="Abschnitt", title:String="Sentiment-Analyse"): Unit = {

    //create xy series
    val sentimentsSeries: XYSeries = new XYSeries("Sentiment-Werte")
    data.foreach { case (i, sentimentValue, _) => sentimentsSeries.add(i, sentimentValue) }
    val relWordsSeries: XYSeries = new XYSeries("Relative Haeufigkeit der erkannten Worte")
    data.foreach { case (i, _, relWordsValue) => relWordsSeries.add(i, relWordsValue) }

    //create xy collections
    val sentimentsDataset: XYSeriesCollection = new XYSeriesCollection()
    sentimentsDataset.addSeries(sentimentsSeries)
    val relWordsDataset: XYSeriesCollection = new XYSeriesCollection()
    relWordsDataset.addSeries(relWordsSeries)

    //create renderers
    val relWordsDot: XYDotRenderer = new XYDotRenderer()
    relWordsDot.setDotHeight(5)
    relWordsDot.setDotWidth(5)
    relWordsDot.setSeriesShape(0, ShapeUtilities.createDiagonalCross(3, 1))
    relWordsDot.setSeriesPaint(0, Color.BLUE)

    val sentimentsDot: XYDotRenderer = new XYDotRenderer()
    sentimentsDot.setDotHeight(5)
    sentimentsDot.setDotWidth(5)

    //create xy axis
    val xax: NumberAxis = new NumberAxis(xlabel)
    val y1ax: NumberAxis = new NumberAxis("Sentiment Werte")
    val y2ax: NumberAxis = new NumberAxis("Relative Haeufigfkeit")

    //create plots
    val plot1: XYPlot = new XYPlot(sentimentsDataset, xax, y1ax, sentimentsDot)
    val plot2: XYPlot = new XYPlot(relWordsDataset, xax, y2ax, relWordsDot)

    val chart1: JFreeChart = new JFreeChart(plot1)
    val chart2: JFreeChart = new JFreeChart(plot2)
    val frame: ApplicationFrame = new ApplicationFrame(title)
    frame.setLayout(new GridLayout(2,1))

    val chartPanel1: ChartPanel = new ChartPanel(chart1)
    val chartPanel2: ChartPanel = new ChartPanel(chart2)

    frame.add(chartPanel1)
    frame.add(chartPanel2)
    frame.pack()
    frame.setVisible(true)
    
    println("Please press enter....")
    System.in.read()
    frame.setVisible(false)
    frame.dispose
  }
  def getTextFileInputAsListOfString(filename: String): List[String] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url)
    proc.getWords(src.getLines().toList.mkString("\n"))
  }
  def getWordsNoLowerCase(line:String):List[String]={
    line.map(char => if(!char.isLetter)  " " else char).mkString
      .split("\\s+").toList
  }
}
