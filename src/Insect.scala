import scala.swing._
//this is a test


object Food{
  var count:Int = 0
}

abstract class Insect{
  val pos:Place
  var armor:Int
  def move()
  def beattacked(strength:Int) = {
    this.armor -= strength
  }
}

abstract class Ant extends Insect {
  val cost:Int
}

class NoAnt(position:Place) extends Ant{
  val pos = position
  var armor = 0
  val cost = 0
  def move() = {}
}

class HarvesterAnt(position:Place) extends Ant {
  val pos:Place = position
  var armor:Int = 1
  val cost:Int = 2
  def move() = Food.count += 1
}

class ThrowerAnt(position:Place) extends Ant {
  val pos:Place = position
  var armor:Int = 1
  val cost:Int = 2
  def move() = {
    this.pos.inside match {
      case List() => {}
      case hd::l => hd.beattacked(1)
    }
  }
}

class Bee(position:Place) extends Insect {
  val pos:Place = position
  var armor:Int = 2
  def move() ={
    this.pos.ant match {
      case a:NoAnt => {}
      case a => a.beattacked(1)
    }
  }
}

class Place(position:Point, entree:Place, sortie:Place){
  val pos:Point = position
  val in:Place = entree
  val out:Place = sortie
  val ant:Ant = new NoAnt(this)
  var inside: List[Insect]  = List()
  def addBee(bee:Bee){
     this.inside = this.inside ++ List(bee)
  }
}