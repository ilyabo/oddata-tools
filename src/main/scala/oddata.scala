package oddata

import _root_.cli.Cli
import com.Ostermiller.util.CSVParser
import geosearch.GeoSearch
import scala.collection._
import mutable.ListBuffer
import countries.{Countries, Country}
import java.io._
import xml.XML

case class OriginDest(origin:Node, dest:Node)

case class Flow(originDest:OriginDest, attr:String, value:Option[BigDecimal])

case class Node(code:String, name:String, lat:Option[BigDecimal], lon:Option[BigDecimal])



trait NodeSearch {
  def getNode(code:String):Option[Node]
}

object NodeInMapSearch {
  def apply(map:Map[String,Node]) = new NodeSearch {
    def getNode(code:String) = map.get(code)
  }
}

object NodeGeoSearch {
  def apply(countryCode:String, featureClass:String, featureCode:String) = new NodeSearch {
    def getNode(code:String) = GeoSearch.find(code, countryCode, featureClass, featureCode) match {
      case Some(entity) => Some(new Node(code, entity.name, Some(entity.lat), Some(entity.lon)))
      case _ => None
    }
  }
}

object NodeCountrySearch {
  def byName = new NodeSearch {
    def getNode(name:String) = node(name, Countries.getCountryByName(name))
  }
  def byCode = new NodeSearch {
    def getNode(code:String) = node(code, Countries.getCountryByCode(code))
  }
  private def node(nameOrCode:String, country:Option[Country]) = country match {
    case Some(country) => Some(new Node(country.code, country.name, Some(country.lat), Some(country.lon)))
    case _ => None
  }
}



object ODData {
  def fileReader(fileName:String, encoding:String = "utf-8") = new InputStreamReader(new FileInputStream(fileName), encoding)
  def stringReader(text:String) = new StringReader(text)
  trait CsvLineAttrNameResolver {
    def apply(header:Seq[String], line:Seq[String], attrCol:Int):String
  }

  def loadNodesFromCsv(reader:Reader, delim:Char, idColName:String, nameColName:String, latColName:String, lonColName:String) = {
    val nodeMap = mutable.Map.empty[String, Node]
    val lines = CSVParser.parse(reader, delim, "nrtf", "\n\r\t\f", "#!")
    val header = lines(0)

    val idCol = indexOfCol(idColName, header)
    val nameCol = indexOfCol(nameColName, header)
    val latCol = indexOfCol(latColName, header)
    val lonCol= indexOfCol(lonColName, header)

    for (li <- 1 until lines.length) {
      val line = lines(li)
      nodeMap(line(idCol)) = new Node(line(idCol), line(nameCol), parseBigDecimal(line(latCol), li), parseBigDecimal(line(lonCol), li))
    }
    nodeMap
  }

  def indexOfCol(colName:String, colNames:Array[String]):Int = {
    val idx = colNames.findIndexOf(_ == colName)
    if (idx < 0)
      throw new IllegalArgumentException("Column '" + colName + "' not found. Available columns: " + colNames)
    idx
  }

  def parseBigDecimal(str:String, line:Int = -1) =
    try {
      if (str.trim.length == 0)
        None  // no need to print the warning message
      else
        Some(BigDecimal(str))
    } catch {
      case nfe:NumberFormatException =>
        printf("Warn: Cannot parse number '%s' in line %d. Adding a None-value flow.\n", str, line); None
    }

}


class ODData(nodeFactory:NodeSearch) {

  private val nodesByCode = mutable.Map.empty[String, Node]
  private val flowAttrs = mutable.LinkedHashSet.empty[String]
  private val flowsByOriginDest = mutable.LinkedHashMap.empty[OriginDest, ListBuffer[Flow]]

  def getNodes =
    List[Node]() ++ nodesByCode.values  // convert to immutable

  def getNodesByCode =
    Map[String, Node]() ++ nodesByCode  // convert to immutable

  def getFlowAttrs =
    List[String]() ++ flowAttrs

  def getFlowsByOriginDest =
    Map[OriginDest, Seq[Flow]]() ++ flowsByOriginDest  // convert to immutable map

  def getFlow(origin:Node, dest:Node, attr:String) = {
    val flows = flowsByOriginDest.get(new OriginDest(origin, dest))
    if (flows.isDefined)
      flows.get.find(_.attr.equals(attr))
    else
      None
  }

  val defaultAttrNameResolver = new ODData.CsvLineAttrNameResolver {
    def apply(header:Seq[String], line:Seq[String], attrCol:Int):String = header(attrCol)
  }

  def loadFlowlistFromCsv(reader:Reader, delim:Char, originColName:String, destColName:String, attrColNames:String
                          /*attrNames:ODData.CsvLineAttrNameResolver = defaultAttrNameResolver*/) {
    val lines = CSVParser.parse(reader, delim, "nrtf", "\n\r\t\f", "#!")
    val header = lines(0)

    val originCol = ODData.indexOfCol(originColName, header)
    val destCol = ODData.indexOfCol(destColName, header)
    val attrs = CSVParser.parse(attrColNames)(0)
    val attrCols = mutable.Map.empty[String, Int]
    for (attr <- attrs) {
      attrCols(attr) = ODData.indexOfCol(attr, header)  // will throw an exception if attr not found
    }


    for (li <- 1 until lines.length) {
      val line = lines(li)
      for (attr <- attrs) {
        addFlow(getNode(line(originCol)), getNode(line(destCol)), attr,  //attrNames(header, line, attrCol),
          ODData.parseBigDecimal(line(attrCols(attr)), li))
      }
    }
  }

  def loadODMatrixCsv(reader:Reader, flowAttr:Option[String], delim:Char) {
    printf("Loading OD matrix ")
    val lines = CSVParser.parse(reader, delim, "nrtf", "\n\r\t\f", "#!")
    var li = 0
    var flowAttrName = flowAttr.getOrElse("unknown")
    if (lines(0).length == 1) {
      // if no attr provided and the first line is a caption, use it
      if (flowAttr.isEmpty) flowAttrName = lines(0)(0)
      li = li + 1  // skip this line
    }
    printf("for flow attribute '%s'\n", flowAttrName)
    if (lines.length > li) {
      val header = lines(li)
      for (i <- (li + 1) until lines.length) {
        val line = lines(i)
        val origin = getNode(line(0))
        for (j <- 1 to line.length - 1) {
          val dest = getNode(header(j))
          val flow = addFlow(origin, dest, flowAttrName, ODData.parseBigDecimal(line(j), j+1))
        }
      }
    } else {
      printf("Warn: empty matrix for attr '%s'\n", flowAttrName)
    }
  }

  private def addFlow(origin:Node, dest:Node, attr:String, value:Option[BigDecimal]) = {
    val od = new OriginDest(origin, dest)
    val flow = new Flow(od, attr, value)
//    println("addFlow: " + flow)
    if (!flowsByOriginDest.contains(od)) {
      flowsByOriginDest.put(od, mutable.ListBuffer.empty[Flow])
    }
    flowsByOriginDest(od) += flow
    flowAttrs += flow.attr
  }

  private var numNodesNotFoundByFactory = 0

  def getNumNodesNotFound = {
    numNodesNotFoundByFactory
  }

  private def getNode(code:String) =
    if (nodesByCode.contains(code)) {
      nodesByCode(code)
    } else {
      val nodeOption = nodeFactory.getNode(code)
      if (nodeOption.isEmpty) {
        numNodesNotFoundByFactory += 1
      }
      val node = nodeOption.getOrElse(new Node(code, code, None, None))
      nodesByCode(code) = node
      node
    }

}


object ODDataToolsMain {

  def usage() = {
"""
Usage: java -jar oddata-tools.jar <command> [parameters] <..input files..> <output file>

List of supported commands:

  l2g [parameters] <nodes.csv> <flows.csv> <output.graphml>

    Convert two CSV files containing lists of nodes and flows with their
    attributes to GraphML.

    Required l2g parameters:
     -idCol     Name of the node id column in <nodes.csv>.
     -nameCol   Name of the node names column in <nodes.csv>.
     -latCol    Name of the node latiutude column in <nodes.csv>.
     -lonCol    Name of the node longitude column in  in <nodes.csv>.
     -originCol Name of the flow origins column in <flows.csv>.
     -destCol   Name of the flow destinations column in <flows.csv>.
     -attrCols  Comma-separated list of names of the flow attribute
                columns in <flows.csv>.

    Optional l2g parameters:
     -delim     CSV delimiter (default is ',')

    Example:
        java -jar oddata-tools.jar \
           l2g \
             -idCol Code  -nameCol Name  -latCol Lat  -lonCol Lon  \
             -originCol Origin  -destCol Dest  -attrCols 1999,2000,2001,2002 \
           nodes.csv flows.csv output.graphml


  m2g [parameters] <od-matrices.csv> <output.graphml>

    Convert the given CSV file containing OD-matrices to GraphML.



"""
  }

  def main(args:Array[String]) = {

    def isBooleanOption(name:String) = false

    println(try {
      if (args.length == 0)
        usage
      else {
        val options = Cli.parseOptions(args, isBooleanOption)
        Cli.removeOptions(args, isBooleanOption) match {

          case "l2g" :: nodesCsv :: flowsCsv :: out :: Nil =>
            if (Cli.checkOptions(options,
                List("delim", "idCol", "nameCol", "latCol", "lonCol", "originCol", "destCol", "attrCols"),
                List("idCol", "nameCol", "latCol", "lonCol", "originCol", "destCol", "attrCols"))) {
              listToGraphML(nodesCsv, flowsCsv, out, options)
              "Done"
            } else {
              usage
            }

          case "m2g" :: odmCsv :: out :: Nil =>
            odmToGraphML(odmCsv, out, options)
            "Done"

          case _ => usage
        }
      }
    } catch {
      case (e:Exception) => "Error: " + e.getClass.getSimpleName + ": " + e.getMessage
    })
  }

  def listToGraphML(nodesCsv:String, flowsCsv:String, outputFile:String, options:Map[String,String]) {
    val delim = options.getOrElse("delim", ",").charAt(0)

    val nodes = ODData.loadNodesFromCsv(ODData.fileReader(nodesCsv),
      delim, options("idCol"), options("nameCol"), options("latCol"), options("lonCol"))

    val data = new ODData(NodeInMapSearch(nodes))

    data.loadFlowlistFromCsv(ODData.fileReader(flowsCsv), delim,
      options("originCol"), options("destCol"), options("attrCols"))

    saveToGraphML(data, getDatasetName(flowsCsv), outputFile)
  }

  def getDatasetName(filename:String) = {
    var name = new File(filename).getName
    val sep = name.lastIndexWhere(_ == '.')
    if (sep > 0) name = name.dropRight(name.length - sep)
    name
  }

  def saveToGraphML(data:ODData, datasetName:String, outputFile:String) {
    println("Generating GraphML")
    val xml = <graphml xmlns="http://graphml.graphdrawing.org/xmlns"
                xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
                xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns
                http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">

        <key attr.type="double" attr.name="lat" id="lat" for="node"/>
        <key attr.type="double" attr.name="lon" id="lon" for="node"/>
        <key attr.name="name" attr.type="string" id="name" for="node"/>
        <key attr.name="fullname" attr.type="string" id="fullname" for="node"/>
        <key attr.name="code" attr.type="string" id="code" for="node"/>
        {
          for (attr <- data.getFlowAttrs) yield {
            // The id parameter below should be the last one!
            // Otherwise lines vanish because of some strange bug
            <key for="edge" attr.type="double"  attr.name={attr} id={attr}/>
          }
        }
      <graph id={datasetName} edgedefault="directed">
      {
        for (node <- data.getNodesByCode.values) yield {
          <node id={node.code}>
            <data key="code">{node.code}</data>
            <data key="name">{node.name}</data>
            <data key="lat">{node.lat.getOrElse("")}</data>
            <data key="lon">{node.lon.getOrElse("")}</data>
          </node>
        }
      }
      {
        for ((od, flows) <- data.getFlowsByOriginDest) yield {
          <edge source={od.origin.code} target={od.dest.code}>
            {
              for (flow <- flows if (flow.value.isDefined)) yield {
                <data key={flow.attr}>{flow.value.get}</data>
              }
            }
          </edge>
        }
      }
      </graph>
    </graphml>

    println("Writing GraphML output")
    XML.save(outputFile, xml, "UTF-8", true, null)
  }


  def odmToGraphML(odmCsv:String, out:String, options:Map[String,String]) {

  }
}