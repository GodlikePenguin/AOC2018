package aoc.days

import aoc.Day

import scala.collection.mutable

object Day15 extends Day(15){
  override protected def A(input: String): Any = {
    var elves = new mutable.HashSet[Monster]()
    var goblins = new mutable.HashSet[Monster]()
    var arena = new mutable.HashMap[(Int, Int), String]()
    input.split("\n").zipWithIndex.foreach(line => {
      line._1.zipWithIndex.foreach(char => {
        if (char._1 == '#') arena.put((char._2, line._2), "#")
        if (char._1 == 'G') goblins.add(new Monster(x = char._2, y = line._2, monsterType = 'G'))
        if (char._1 == 'E') elves.add(new Monster(x = char._2, y = line._2, monsterType = 'E'))
      })
    })
    while(elves.nonEmpty && goblins.nonEmpty) {
      for (monster <- (elves ++ goblins).toList.sortWith(monsterSorter)) {
        if (monster.monsterType == 'G') {
          
        } else {

        }
      }
    }
  }

  def BFS(x: Int, y: Int, maze: mutable.HashMap[(Int, Int), String], goalSet: mutable.HashSet[Monster]): List[(Int, Int)] = {
    if (maze(x, y) == "#") {
      return List()
    }
    if (adjacentToGoal(x, y, goalSet)) {
      return List((x, y))
    } else {
      return BFS(x+1, y, maze, goalSet) ++ BFS(x-1, y, maze, goalSet) ++ BFS(x, y+1, maze, goalSet) ++ BFS(x, y-1, maze, goalSet)
    }
  }

  def adjacentToGoal(x: Int, y: Int, goalSet: mutable.HashSet[Monster]): Boolean = {
    for (m <- goalSet) {
      if (Math.abs(m.x-x) == 1 || Math.abs(m.y-y) == 1) {
        return true
      }
    }
    false
  }

  def monsterSorter(m1 : Monster, m2 : Monster) : Boolean = {
    m1.y < m2.y || (m1.y == m2.y && m1.x < m2.x)
  }

  override protected def B(input: String): Any = {}

  override protected def test(): Unit = {
    A("#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########")
  }
}

class Monster(var x: Int = 0, var y: Int = 0, var hitpoints: Int = 200, var monsterType: Char) {

}
