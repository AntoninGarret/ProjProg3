import scala.swing._
import scala.swing.{ SimpleSwingApplication, MainFrame, Panel }
import scala.swing.event._
import java.awt.event.{ ActionEvent, ActionListener }
import java.awt.{ Color, Graphics2D, Point, geom, MouseInfo }
import javax.swing.{ ImageIcon, Timer }

object Food{
  var count:Int = 0
}

trait Insect{
  var pl: Place
  var armor:Int
  def move()
  def beattacked(strength:Int) = {
    this.armor -= strength
  }
  val im: Image
}

trait Ant extends Insect {
  val cost:Int
}

class NoAnt(position:Place) extends Ant{
  var pl = position
  var armor = 1
  val cost = 0
  def move() = {}
  val im: Image = (new ImageIcon("img/ant_freeze.png")).getImage( )
}

class HarvesterAnt(position:Place) extends Ant {
  var pl: Place = position
  val im: Image = (new ImageIcon("img/ant_harvester.png")).getImage( )
  var armor:Int = 1
  val cost:Int = 2
  def move() = Food.count += 1
}

class ThrowerAnt(position:Place) extends Ant {
  var pl:Place = position
  val im: Image = (new ImageIcon("img/ant_thrower.png")).getImage( )
  var armor:Int = 1
  val cost:Int = 2
  def move() = {
    this.pl.inside match {
      case List() => {}
      case hd::l => hd.beattacked(1)
    }
  }
}

class Bee(position:Place) extends Insect {
  val im: Image = (new ImageIcon("img/bee.png")).getImage( )
  var pl:Place = position
  var pos: Point = pl.pos
  var armor:Int = 2
  def move() ={
    this.pl.ant match {
      case a:NoAnt => {
        this.pl.removeBee(this)
        this.pl.out.addBee(this)
        this.pl = this.pl.out
      }
      case a => a.beattacked(1)
    }
  }
}

abstract class Place {
  var in: Place = this
  var out: Place = this
  val pos:Point
  var ant:Ant = new NoAnt(this)
  var inside: List[Bee]  = List()
  def addBee(bee:Bee){
    this.inside = this.inside ++ List(bee)
  }
  def removeBee(bee:Bee){
    def removeFrom(list:List[Bee]): List[Bee] = {
      list.head match{
        case x if(x==bee) => list.tail
        case otherbee => (otherbee::(removeFrom(list.tail)))
      }
    }
    this.inside = removeFrom(this.inside)
  }
  def update() {
    if (this.ant.armor <= 0){
      this.ant = new NoAnt(this)
    }
    if (this.inside.head.armor <= 0) {
      this.inside = this.inside.tail
    }
  } 
 /* val boite = new geom.GeneralPath
  this.boite.moveTo(pos.x, pos.y)
  this.boite.lineTo(pos.x + 93, pos.y)
  this.boite.lineTo(pos.x + 93, pos.y + 98)
  this.boite.lineTo(pos.x, pos.y + 98)
  this.boite.lineTo(pos.x, pos.y)*/
  val im = (new ImageIcon("img/tunnel.png")).getImage( )

}

class MiddlePlace(position:Point) extends Place {
  val pos = position
}
  
class LeftPlace(position: Point) extends Place {
  val pos = position
}

class RightPlace(position: Point) extends Place {
  val pos = position
}

object placesSet {
  var places: List[Place] = List(new RightPlace(new Point(40 + 7*93, 200)))
  for (i <- 1 to 6){
     places = (new MiddlePlace(new Point(40 + (7-i)*93, 200)))::places
  }
  places = (new LeftPlace(new Point(40, 200)))::places
  places(0).in = places(1)
  for (i <- 1 to 6) {
    places(i).in = places(i+1)
    places(i).out = places(i-1)
  }
  places(7).out = places(6)
}


