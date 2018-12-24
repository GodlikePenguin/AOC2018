package aoc.days

import aoc.Day

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

object Day24 extends Day(24) {

  def parseWeaknessImmunities(input: String): (Array[String], Array[String]) = {
    var parseInput = input.replaceAll("\\(", "").replaceAll("\\)", "")
    val weaknessess = new ArrayBuffer[String]()
    val immunities = new ArrayBuffer[String]()
    for (group <- parseInput.split("; ")) {
      if (group.contains("weak")) {
        weaknessess ++= group.split(" ").takeRight(group.split(" ").length-2).map(a => a.replaceAll(",", ""))
      } else if (group.contains("immune")) {
        immunities ++= group.split(" ").takeRight(group.split(" ").length-2).map(a => a.replaceAll(",", ""))
      }
    }
    (weaknessess.toArray, immunities.toArray)
  }

  def parseUnits(input: String): (Array[Group], Array[Group]) = {
    var immune = new ArrayBuffer[Group]()
    var infection = new ArrayBuffer[Group]()
    var seenInfection = false
    for (line <- input.split("\n")) {
      Breaks.breakable {
        if (line.contains("Infection")) {
          seenInfection = true
        }
        if (line.split(" ").length < 4) {
          Breaks.break
        }
        val breakDown = line.split(" ")
        val units = breakDown(0).toInt
        val hitpoints = breakDown(4).toInt
        val initiative = breakDown(breakDown.length-1).toInt
        val attack = (breakDown(breakDown.length-5), breakDown(breakDown.length-6).toInt)
        var weaknesses = new Array[String](0)
        var immunities = new Array[String](0)
        if ("\\(.*\\)".r.findFirstIn(line).nonEmpty) {
          val (a, b) = parseWeaknessImmunities("\\(.*\\)".r.findFirstIn(line).get)
          weaknesses = a
          immunities = b
        }
        if (seenInfection) {
          infection += new Group(units = units, hitpoints = hitpoints, weaknesses = weaknesses, immunities = immunities, attack = attack, initiative = initiative, groupType = "infection")
        } else {
          immune += new Group(units = units, hitpoints = hitpoints, weaknesses = weaknesses, immunities = immunities, attack = attack, initiative = initiative, groupType = "immune")
        }
      }
    }
    (immune.toArray, infection.toArray)
  }

  def getTargetGroupFor(group: Group, enemies: Array[Group]): Group = {
    var target: Group = new Group(0, 0, Array(), Array(group.attack._1), ("", 0), 0, "")
    for (enemy <- enemies) {
      if (enemy.weaknesses.contains(group.attack._1) && !target.weaknesses.contains(group.attack._1)) {
        target = enemy
      } else if (enemy.weaknesses.contains(group.attack._1) && target.weaknesses.contains(group.attack._1)) {
        if (enemy.getEffectivePower > target.getEffectivePower) {
          target = enemy
        } else if (enemy.getEffectivePower == target.getEffectivePower) {
          if (enemy.initiative > target.initiative) {
            target = enemy
          }
        }
      } else if (!enemy.weaknesses.contains(group.attack._1) && !target.weaknesses.contains(group.attack._1)) {
        if (enemy.getEffectivePower > target.getEffectivePower) {
          target = enemy
        } else if (enemy.getEffectivePower == target.getEffectivePower) {
          if (enemy.initiative > target.initiative) {
            target = enemy
          }
        }
      } else if (target.immunities.contains(group.attack._1) && !enemy.immunities.contains(group.attack._1)) {
        target = enemy
      }
    }
    if (target.immunities.contains(group.attack._1)) {
      target = new Group(0, 0, Array(), Array(), ("", 0), 0, "")
    }
    target
  }

  override protected def A(input: String): Any = {
    var chosen = new mutable.HashSet[Group]()
    val (immune, infection) = parseUnits(input)
    while (immune.count(_.units <= 0) != immune.length  && infection.count(_.units <= 0) != infection.length) {
      for (group <- (immune.filter(_.units > 0) ++ infection.filter(_.units > 0)).sortWith((a, b) => (a.getEffectivePower > b.getEffectivePower) || (a.getEffectivePower == b.getEffectivePower && a.initiative > b.initiative))) {
        if (group.groupType == "infection") {
          var localTarget = getTargetGroupFor(group, (immune.filter(_.units > 0).toSet diff chosen).toArray)
          group.setTarget(localTarget)
          chosen.add(localTarget)
        } else {
          var localTarget = getTargetGroupFor(group, (infection.filter(_.units > 0).toSet diff chosen).toArray)
          group.setTarget(localTarget)
          chosen.add(localTarget)
        }
      }
      for (group <- (immune.filter(_.units > 0) ++ infection.filter(_.units > 0)).sortBy(_.initiative)) {
        group.attackTarget
      }
    }
    if (immune.nonEmpty) {
      return immune.foldLeft(0)((accum, group) => accum + group.units)
    } else {
      return infection.foldLeft(0)((accum, group) => accum + group.units)
    }
  }

  override protected def B(input: String): Any = ???

  override protected def test(): Unit = {
    assert(A("Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4")==5216)
  }
}

class Group(var units: Int, var hitpoints: Int, var weaknesses: Array[String], var immunities: Array[String], var attack: (String, Int), var initiative: Int, var groupType: String) {
  var target: Group = _

  def getEffectivePower: Int = units * attack._2

  def setTarget(newTarget: Group): Unit = target = newTarget

  def attackTarget: Unit = {
    var multiplier = 1
    if (target.immunities.contains(attack._1)) {
      multiplier = 0
    } else if (target.weaknesses.contains(attack._1)) {
      multiplier = 2
    }
    if (target.hitpoints != 0) {
      target.units = target.units - Math.floor((getEffectivePower * multiplier) / target.hitpoints).toInt
    }
  }
}
