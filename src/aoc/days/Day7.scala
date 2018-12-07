package aoc.days

import java.util

import aoc.Day

import scala.collection.mutable
import scala.util.control.Breaks

object Day7 extends Day(7) {
  override protected def A(input: String): Any = {
    var orders = new mutable.HashSet[(String, String)]()
    val jobs = new mutable.HashSet[String]()
    val regex = " [A-Z] ".r
    for (line <- input.split("\n")) {
      val results = regex.findAllIn(line).toList
      orders.add((results(0).replaceAll("\\s", ""), results(1).replaceAll("\\s", "")))
      jobs.add(results(0).replaceAll("\\s", ""))
      jobs.add(results(1).replaceAll("\\s", ""))
    }
    var outputOrder = ""
    while(jobs.nonEmpty) {
      Breaks.breakable {
        for (c <- 'A' to 'Z') {
          //No ordering pairs such that the current job has a dependant job
          if (orders.count(a => a._2 == c.toString) == 0 && !outputOrder.contains(c)) {
            outputOrder += c.toString
            orders = orders.filter(a => a._1 != c.toString)
            jobs.remove(c.toString)
            Breaks.break
          }
        }
      }
    }
    outputOrder
  }

  override protected def B(input: String): Any = {
    var orders = new mutable.HashSet[(String, String)]()
    val jobs = new mutable.HashSet[String]()
    val regex = " [A-Z] ".r
    for (line <- input.split("\n")) {
      val results = regex.findAllIn(line).toList
      orders.add((results(0).replaceAll("\\s", ""), results(1).replaceAll("\\s", "")))
      jobs.add(results(0).replaceAll("\\s", ""))
      jobs.add(results(1).replaceAll("\\s", ""))
    }

    var beganProcessing = ""
    var currentSecond = 0
    val cleanupMap = new mutable.HashMap[Int, mutable.HashSet[Char]]()
    //5 workers
    //worker implemented by map from second -> job
    val workers = List(new mutable.HashMap[Int, String](), new mutable.HashMap[Int, String](),
      new mutable.HashMap[Int, String](), new mutable.HashMap[Int, String](), new mutable.HashMap[Int, String]())
    while(jobs.nonEmpty) {
      if (cleanupMap.get(currentSecond).nonEmpty) {
        for (c <- cleanupMap(currentSecond)) {
          orders = orders.filter(a => a._1 != c.toString)
          jobs.remove(c.toString)
        }
      }
      for (c <- 'A' to 'Z') {
        if (orders.count(a => a._2 == c.toString) == 0 && !beganProcessing.contains(c)) {
          Breaks.breakable {
            for (i <- workers.indices) {
              if (workers(i).get(currentSecond).isEmpty) {
                var limit = currentSecond + 60 + charToNum(c)
                for (j <- currentSecond until limit) {
                  workers(i).put(j, c.toString)
                }
                beganProcessing += c
                val myset = cleanupMap.getOrElse(limit, new mutable.HashSet[Char]())
                myset.add(c)
                cleanupMap.put(limit, myset)
                Breaks.break
              }
            }
          }
        }
      }
      currentSecond += 1
    }
    var currentMax = 0
    for (worker <- workers) {
      if (worker.maxBy(_._1)._1 > currentMax) {
        currentMax = worker.maxBy(_._1)._1
      }
    }
    currentMax + 1
  }

  protected def printWork(workers : List[mutable.HashMap[Int, String]]): Unit = {
    print("t\t")
    for(j <- workers.indices) {
      print(j)
      print("\t")
    }
    println
    for (i <- 0 to 1050) {
      print(i)
      print("\t")
      for(j <- workers.indices) {
        print(workers(j).getOrElse(i, "."))
        print("\t")
      }
      println
    }
  }

  protected def charToNum(c: Char): Int = c.toInt - 64


  override protected def test(): Unit = {
    assert(A("Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin.")=="CABDFE")
  }
}
