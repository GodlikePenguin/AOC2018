package aoc.days

import java.text.SimpleDateFormat
import java.util.Date

import aoc.Day

import scala.collection.mutable

object Day4 extends Day(4) {

  override protected def A(input: String): Any = {
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    val sortedInput = input.split("\n").sortBy(a => format.parse(a.split("]")(0).replace("[", "")))
    var asleepMap = new mutable.HashMap[String, mutable.Set[(java.util.Date, java.util.Date)]]()
    var currentId = ""
    var startTime = new java.util.Date()
    var endTime = new java.util.Date()
    sortedInput.foreach(line => {
      if (line.contains("#")) {
        currentId = """\d{2,4}""".r.findFirstIn(line.split("]")(1)).get
      } else if (line.contains("asleep")) {
        startTime = format.parse(line.split("]")(0).replace("[", ""))
      } else if (line.contains("up")) {
        endTime = format.parse(line.split("]")(0).replace("[", ""))
        var old = asleepMap.getOrElse(currentId, new mutable.HashSet[(java.util.Date, java.util.Date)]())
        old.add((startTime, endTime))
        asleepMap.put(currentId, old)
      }
    })
    val guardWhoSleepsTheMost = findSleepyGuard(asleepMap)
    val minuteAsleepMost = findSleepyMinute(asleepMap(guardWhoSleepsTheMost))
    Integer.parseInt(guardWhoSleepsTheMost) * minuteAsleepMost
  }

  def findSleepyGuard(stringToTuples: mutable.HashMap[String, mutable.Set[(Date, Date)]]) : String = {
    stringToTuples.maxBy(entry => entry._2.foldLeft(0L)((a, b) => a + (b._2.getTime - b._1.getTime)))._1
  }

  def findSleepyMinute(dates: mutable.Set[(Date, Date)]): Int = {
    var times = new mutable.HashMap[Int, mutable.Set[String]]()
    dates.foreach(pair => {
      var currentDate = pair._1
      while(currentDate.before(pair._2)) {
        var old = times.getOrElse(currentDate.getMinutes, new mutable.HashSet[String]())
        old.add(currentDate.toString)
        times.put(currentDate.getMinutes, old)
        currentDate.setTime(currentDate.getTime + 1000L*60)
      }
    })
    times.maxBy(a => a._2.size)._1
  }

  def findSleepyMinuteWithFreq(dates: mutable.Set[(Date, Date)]): (Int, Int) = {
    var times = new mutable.HashMap[Int, mutable.Set[String]]()
    dates.foreach(pair => {
      var currentDate = pair._1
      while(currentDate.before(pair._2)) {
        var old = times.getOrElse(currentDate.getMinutes, new mutable.HashSet[String]())
        old.add(currentDate.toString)
        times.put(currentDate.getMinutes, old)
        currentDate.setTime(currentDate.getTime + 1000L*60)
      }
    })
    val maxMinute = times.maxBy(a => a._2.size)
    (maxMinute._1, maxMinute._2.size)
  }

  override protected def B(input: String): Any = {
    val format = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    val sortedInput = input.split("\n").sortBy(a => format.parse(a.split("]")(0).replace("[", "")))
    var asleepMap = new mutable.HashMap[String, mutable.Set[(java.util.Date, java.util.Date)]]()
    var currentId = ""
    var startTime = new java.util.Date()
    var endTime = new java.util.Date()
    sortedInput.foreach(line => {
      if (line.contains("#")) {
        currentId = """\d{2,4}""".r.findFirstIn(line.split("]")(1)).get
      } else if (line.contains("asleep")) {
        startTime = format.parse(line.split("]")(0).replace("[", ""))
      } else if (line.contains("up")) {
        endTime = format.parse(line.split("]")(0).replace("[", ""))
        var old = asleepMap.getOrElse(currentId, new mutable.HashSet[(java.util.Date, java.util.Date)]())
        old.add((startTime, endTime))
        asleepMap.put(currentId, old)
      }
    })

    var maxMins = 0
    var guardId = ""
    var exactMinute = 0
    asleepMap.foreach(guard => {
      val freqMinuteAndHowManyTimes = findSleepyMinuteWithFreq(asleepMap(guard._1))
      if (freqMinuteAndHowManyTimes._2 > maxMins) {
        maxMins = freqMinuteAndHowManyTimes._2
        guardId = guard._1
        exactMinute = freqMinuteAndHowManyTimes._1
      }
    })
    Integer.parseInt(guardId) * exactMinute
  }

  override protected def test(): Unit = {
    assert(A("[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up")==240)
    assert(B("[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up")==4455)
  }
}
