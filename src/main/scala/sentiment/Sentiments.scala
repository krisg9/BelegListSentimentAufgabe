package sentiment

import java.awt.{Color, GridLayout}
import org.jfree.chart.{ChartPanel, JFreeChart}
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYDotRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.ui.ApplicationFrame
import org.jfree.util.ShapeUtilities

import scala.annotation.tailrec
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
    @tailrec
    def helper(list: List[(Int, List[String])], woerter: List[String], int: Int): List[(Int, List[String])] = {
      if (woerter.length < wordCount) list :+ (int, woerter.take(wordCount))
      else helper(list :+ (int, woerter.take(wordCount)), woerter.slice(wordCount, woerter.length), int + 1)
    }

    val source = Source.fromResource(filename)
    val woerterA = source.getLines().mkString(" ").toLowerCase
      .replace(".", "")
      .replace("!", "")
      .replace("?", "")
      .replace(",", "")
      .replace("'", " ")
      .replace(";", "")
      .replace("--", " ")
      .split(" ")
    source.close()

    helper(List.empty[(Int, List[String])], woerterA.toList, 1)
  }

  def getDocumentSplitByPredicate(filename: String, predicate: String => Boolean): List[(Int, List[String])] = {
    @tailrec
    def helper(zeilen: List[String], erg: List[(Int, List[String])], int: Int): List[(Int, List[String])] = {
      if (zeilen.isEmpty) erg.slice(1, erg.length)
      else if (erg.isEmpty ||
        predicate(zeilen.head)) {
        helper(zeilen.slice(1, zeilen.length), erg :+ (int, List.empty), int + 1)
      } else {
        //val t = erg.last
        //val i = t._1
        //val l = t._2
        //val ln = zeilen(0).split(" ").toList
        //val le: List[String] = l ++ ln
        //erg.updated(erg.length - 1, (i, le))
        helper(zeilen.slice(1, zeilen.length)
          , erg.updated(erg.length - 1, (erg.last._1, erg.last._2 ++
            zeilen.head.split(" ").toList)), int)
      }
    }

    def fixFormat(string: String): String = {
      string.toLowerCase
        .replace(".", "")
        .replace("!", "")
        .replace("?", "")
        .replace(",", "")
        .replace("'", " ")
        .replace(";", "")
        .replace("--", " ")
    }

    val source = Source.fromResource(filename)
    val zeilenA = source.getLines().mkString("\n").linesIterator.toList

    helper(zeilenA, List.empty[(Int, List[String])], 0).map {
      case (num, strList) => (num, strList.map(fixFormat(_)))
    }
  }

  def analyseSentiments(l: List[(Int, List[String])]): List[(Int, Double, Double)] = {
    def helper(l: List[(Int, List[String])], erg:List[(Int, Double, Double)]):{
    //abbruch bedingung
    //neues toupel
    //toupel füllen
    }

    helper(l,List.empty[(Int, Double,Double)])
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
