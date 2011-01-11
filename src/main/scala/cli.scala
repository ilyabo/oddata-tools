package cli

import collection.mutable.ListBuffer
import scala.collection._


object Cli {

  def checkOptions(provided:Map[String, String], supported:List[String], required:List[String]):Boolean = {
    var rv = true
    for (opt <- provided.keys if (!supported.contains(opt))) {
      println("Option '" + opt + "' shouldn't be provided in this context")
      rv = false
    }

    for (opt <- required if (!provided.keySet.contains(opt))) {
      println("Option '" + opt + "' is required")
      rv = false
    }
    rv
  }

  def parseOptions(args:Array[String], isBooleanOption:(String => Boolean)) = {
    val opts = mutable.Map.empty[String,String]
    for (i <- 0 until args.length) {
      if (args(i).startsWith("-")) {
        val name = args(i).drop(1)
        opts(name) = if (isBooleanOption(name)) "true" else {
          if (i + 1 >= args.length)
            throw new IllegalArgumentException("Option " + name + " value is missing")
          args(i + 1)
        }
      }
    }
    opts
  }

  def removeOptions(args:Array[String], isBooleanOption:(String => Boolean)) = {
    val argsOnly = ListBuffer.empty[String]
    var skipTwo = false
    for (i <- 0 until args.length) {
      if (args(i).startsWith("-")) {
        if (!isBooleanOption(args(i))) skipTwo = true
      } else {
        if (!skipTwo) argsOnly += args(i)
        skipTwo = false
      }
    }
    argsOnly.toList
  }

}
