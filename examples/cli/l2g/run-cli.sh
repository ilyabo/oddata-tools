java -jar ../../../target/scala_2.8.1/oddata-tools-cli.jar \
  l2g \
    -idCol Code  -nameCol Name  -latCol Lat  -lonCol Lon  \
    -originCol Origin  -destCol Dest  -attrCols 1999,2000,2001,2002  \
   nodes.csv flows.csv output.graphml
