package sentiment

import java.awt.{Color, GridLayout}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities

import scala.io.Source

/**
 * @author hendrik
 *         modified by akarakochev
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
    val source = Source.fromResource(filename).mkString
    val words = proc.getWords(source)
    (1 to words.length).toList.zip(words.grouped(wordCount))
  }

  def getDocumentSplitByPredicate(filename: String, predicate: String => Boolean): List[(Int, List[String])] = {
    val source = Source.fromResource(filename)
      .getLines()
      // cut all lines before the first predicate
      .dropWhile(!predicate(_))
      // combine paragraphs from left to right
      .foldLeft(List.empty[String]) {
        (acc, nextLine) => {
          if (predicate(nextLine)) acc.appended("")
          else {
            acc match {
              case Nil => List(nextLine)
              // copy without last element and append the new line
              case _ :: _ => acc.init :+ acc.last + nextLine
            }
          }
        }
      }
      .map(paragraph => proc.getWords(paragraph))
    (1 to 10000).toList.zip(source)
  }

  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {
    l.map {
      case (index: Int, words: List[String]) =>
        // create the 3-tuple which is returned
        (index,
          // convert words to sentiment scores
          words
            .filter(sentiments.contains)
            .map(word => sentiments(word))
            .foldLeft(0.0)((acc, sentimentVal) => acc + sentimentVal) / words.count(sentiments.contains),
          // calculate used words
          words
            .count(sentiments.contains).toDouble / words.length.toDouble
        )
    }
  }

  /** ********************************************************************************************
   *
   * Helper Functions
   *
   * ********************************************************************************************
   */

  def getSentiments(filename: String): Map[String, Int] = {
    val url = getClass.getResource("/" + filename).getPath
    val src = scala.io.Source.fromFile(url.replaceAll("%20", " "))
    val iter = src.getLines()
    val result: Map[String, Int] = (for (row <- iter) yield {
      val seg = row.split("\t"); (seg(0) -> seg(1).toInt)
    }).toMap
    src.close()
    result
  }

  def createGraph(data: List[(Int, Double, Double)], xlabel: String = "Abschnitt", title: String = "Sentiment-Analyse"): Unit = {

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
    frame.setLayout(new GridLayout(2, 1))

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
}