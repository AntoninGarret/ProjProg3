import scala.swing._
import scala.swing.{ SimpleSwingApplication, MainFrame, Panel }
import scala.swing.event._
import java.awt.event.{ ActionEvent, ActionListener }
import java.awt.{ Color, Graphics2D, Point, geom, MouseInfo }
import javax.swing.{ ImageIcon, Timer }
import scala.util.Random

trait Insect {
  var pl: Place
  var armor: Int
  def move()
  def beattacked(strength: Int):Unit = {
    this.armor -= strength
    if (this.armor <= 0) {
      this match {
        case a: FireAnt =>{
          a.reduce_armor()
          a.pl.ant = new NoAnt(a.pl)
        }
        case a: Ant => a.pl.ant = new NoAnt(a.pl)
        case a: Bee => a.pl.removeBee(a)
      }
    }
  }
  val im: Image
}

trait Ant extends Insect {
  val cost: Int
  val strength1: Int
  val strength2: Int
  var strength: Int
  def doubleStrength() = this.strength = strength2 
  val blocksPath: Boolean
}

class NoAnt(position: Place) extends Ant {
  var pl = position
  var armor = 1
  val cost = 0
  def move() = {}
  val blocksPath = false
  val im: Image = (new ImageIcon("img/remover.png")).getImage()
  val strength1 = 0
  var strength = strength1
  val strength2 = 2 * strength1 
}

class HarvesterAnt(position: Place) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_harvester.png")).getImage()
  var armor: Int = 1
  val cost: Int = 2
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  def move() = Game.food += strength
  val blocksPath = true
}

class Leaf(startp:Place){
   val im: Image = (new ImageIcon("img/leaf.png")).getImage()
   var x = startp.pos.x
   var display = false
   var goal = startp
   def deplace():Unit ={
      if (this.x < this.goal.pos.x - 30){
        this.x += 2
      }
      else
        this.display = false
   }
   def reset (start: Place, end:Place)={
      this.x = start.pos.x + 40
      this.goal = end
      this.display = true
   }
}

trait ThrowerAnt extends Ant {
  var armor: Int = 1
  val cost: Int = 2
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  val blocksPath = true
  val ant_leaf : Leaf
}

class ShortThrowerAnt(position:Place) extends ThrowerAnt{
  val im: Image = (new ImageIcon("img/ant_shortthrower.png")).getImage()
  var pl: Place = position
  val ant_leaf = new Leaf(this.pl)
  def move() = {
    var hit = false
    var place_checked = this.pl
    for (i <- 0 to 2){
      if (!hit){
        place_checked match{
          case p: RightPlace => hit = true
          case _ => {}
        }
        place_checked.inside match {
          case List()  => {}
          case hd :: l => {
            hd.beattacked(strength)
            hit = true
            this.ant_leaf.reset(this.pl,place_checked)
          }
        }
        place_checked = place_checked.in
      }
    }
  }
}

class LongThrowerAnt(position:Place) extends ThrowerAnt{
  val im: Image = (new ImageIcon("img/ant_longthrower.png")).getImage()
  var pl: Place = position
  val ant_leaf = new Leaf(this.pl)
  def move() = {
    var hit = false
    var place_checked = this.pl
    for(i <- 0 to 2){
      place_checked match{
          case p: RightPlace => hit = true
          case _ => {}
        }
    }
    while(!hit){
      if (!hit){
        place_checked match{
          case p: RightPlace => hit = true
          case _ => {}
        }
        place_checked.inside match {
          case List()  => {}
          case hd :: l => {
            hd.beattacked(strength)
            hit = true
            this.ant_leaf.reset(this.pl,place_checked)
          }
        }
        place_checked = place_checked.in
      }
    }
  }
}

class ScubaAnt(position: Place) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_scuba.png")).getImage()
  var armor: Int = 1
  val cost: Int = 5
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  def move() = {
    this.pl.inside match {
      case List()  => {}
      case hd :: l => hd.beattacked(strength)
    }
  }
  val blocksPath = true
}

class NinjaAnt(position: Place) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_ninja.png")).getImage()
  var armor: Int = 1
  val cost: Int = 6
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  def move() = {
    for (bee <- pl.inside) {
      bee.beattacked(strength)
    }
  }
  val blocksPath = false
}

class HungryAnt(position: Place) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_hungry.png")).getImage()
  var armor: Int = 1
  val cost: Int = 4
  var stop = 0
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  def move() = {
    if (this.stop <= 0) {
      pl.inside match {
        case Nil =>
        case l   => {pl.removeBee(scala.util.Random.shuffle(pl.inside).head)
        this.stop += 3}
      }
      
    } else {
      this.stop -= 1
    }
  }
  val blocksPath = true
}

class BodyguardAnt(position: Place, toProtect: Ant) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_weeds.png")).getImage()
  var protectedAnt: Ant = toProtect
  var armor: Int = 2
  val cost: Int = 4
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  def move() {this.protectedAnt.move()}
  override def doubleStrength() {protectedAnt.doubleStrength()}
  override def beattacked(strength: Int) {
    this.armor -= strength
    if (this.armor <= 0) {
      protectedAnt.pl = this.pl
      this.pl.ant = protectedAnt
    }
  }
  val blocksPath = true
}

class QueenAnt(position: Place) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_queen.png")).getImage()
  var armor: Int = 2
  val cost: Int = 6
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1 
  def move() = {
    this.pl.out.doubleLeft()
    this.pl.doubleRight()
    this.pl.inside match {
      case List()  => {}
      case hd :: l => hd.beattacked(strength)
    }
  }
  val blocksPath = true
}

class WallAnt(position:Place) extends Ant{
  var pl:Place = position
  val im: Image = (new ImageIcon("img/ant_wall.png")).getImage()
  var armor: Int = 4
  val cost: Int = 4
  val strength1 = 1
  var strength = strength1
  val strength2 = 2 * strength1
  val blocksPath = true
  def move() ={}
}

class FireAnt(position:Place) extends Ant{
  var pl:Place = position
  val im: Image = (new ImageIcon("img/ant_fire.png")).getImage()
  var armor: Int = 1
  val cost: Int = 5
  val strength1 = 3
  var strength = strength1
  val strength2 = 2 * strength1
  val blocksPath = true
  def move() ={}
  def reduce_armor() = {
    for (b:Bee <- this.pl.inside){
      b.beattacked(this.strength)
    }
  }
}

class Bee(position: Place) extends Insect {
  val im: Image = (new ImageIcon("img/bee.png")).getImage()
  var pl: Place = position
  var x = pl.pos.x
  var dx = 0
  var pos: Point = pl.pos
  var armor: Int = 2
  def deplace() {
    if (this.dx > 0 & this.x > 40) {
      this.x -= 2
      this.dx -= 2
    }
  }
  def move() = {
    this.pl.ant.blocksPath match {
      case false => {
        this.pl.out match {
          case p: LeftPlace => Game.beesWin = true
          case p => p.ant match {
            case a: QueenAnt => Game.beesWin = true
            case a: BodyguardAnt => a.protectedAnt match {
              case a: QueenAnt => Game.beesWin = true
              case _ =>
            }
            case _ =>
          }
        }
        this.pl.removeBee(this)
        this.pl.out.addBee(this)
        this.pl = this.pl.out
        this.dx += 93
      }
      case true => pl.ant.beattacked(1)
    }
  }
}

abstract class Place {
  var in: Place = this
  var out: Place = this
  val pos: Point
  var ant: Ant = new NoAnt(this)
  var inside: List[Bee] = List()
  def addBee(bee: Bee) {
    this.inside = this.inside ++ List(bee)
  }
  def removeBee(bee: Bee) {
    def removeFrom(list: List[Bee]): List[Bee] = {
      list.head match {
        case x if (x == bee) => list.tail
        case otherbee        => (otherbee :: (removeFrom(list.tail)))
      }
    }
    this.inside = removeFrom(this.inside)
  }
  /* val boite = new geom.GeneralPath
  this.boite.moveTo(pos.x, pos.y)
  this.boite.lineTo(pos.x + 93, pos.y)
  this.boite.lineTo(pos.x + 93, pos.y + 98)
  this.boite.lineTo(pos.x, pos.y + 98)
  this.boite.lineTo(pos.x, pos.y)*/
  val im = (new ImageIcon("img/tunnel.png")).getImage()
  var isWater: Boolean = false
  def drown() {
    if (this.isWater) {
      this.ant match {
        case a: ScubaAnt =>
        case a: QueenAnt =>
        case a           => this.ant = new NoAnt(this)
      }
    }
  }
  def isInPlace(p:Point):Boolean ={
      if ((p.x < this.pos.x + 93) & (p.x > this.pos.x) & (p.y < this.pos.y + 93) & (p.y > this.pos.y)) return true
      else return false
  }
  def doubleRight() {
    this.ant.doubleStrength
    this.in match {
      case pl: MiddlePlace => pl.doubleRight()
      case pl => pl.ant.doubleStrength() 
    }
  }
  def doubleLeft() {
    this.ant.doubleStrength
    this.out match {
      case pl: MiddlePlace => pl.doubleLeft()
      case pl => pl.ant.doubleStrength()
    }
  }
}

class MiddlePlace(position: Point) extends Place {
  val pos = position
}

class LeftPlace(position: Point) extends Place {
  val pos = position
}

class RightPlace(position: Point) extends Place {
  val pos = position
}

class MiddleWaterPlace(position:Point) extends MiddlePlace(position) {
  override val im = (new ImageIcon("img/water_tunnel.png")).getImage()
  this.isWater = true
}

object placesSet {
  var tunnels: List[List[Place]] = List()
  for (tun <- 0 to 2) {
    var places: List[Place] = List(new RightPlace(new Point(40 + 7 * 93, 400 - (tun * 98))))
    for (i <- 1 to 6) {
      if (i==5) {
        places = (new MiddleWaterPlace(new Point(40 + (7 - i) * 93, 400 - (tun * 98)))) :: places
      } else {
        places = (new MiddlePlace(new Point(40 + (7 - i) * 93, 400 - (tun * 98)))) :: places
      }
    }
    places = (new LeftPlace(new Point(40, 400 - (tun * 98)))) :: places
    places(0).in = places(1)
    for (i <- 1 to 6) {
      places(i).in = places(i + 1)
      places(i).out = places(i - 1)
    }
    places(7).out = places(6)
    tunnels = places :: tunnels
  }
}



