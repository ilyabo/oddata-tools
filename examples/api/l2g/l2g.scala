import oddata._

val nodes = ODData.loadNodesFromCsv(
  reader = ODData.fileReader("nodes.csv"),
  delim = ',', idColName = "Code", nameColName = "Name", latColName = "Lat", lonColName = "Lon")

val data = new ODData(NodeInMapSearch(nodes))
data.loadFlowlistFromCsv(
  reader = ODData.fileReader("flows.csv"),
  delim = ',', originColName = "Origin", destColName = "Dest", attrColNames = "1999,2000,2001,2002")

data.saveToGraphML("myflows", "output.graphml")
