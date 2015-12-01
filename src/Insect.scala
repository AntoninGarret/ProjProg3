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
}

class HarvesterAnt(position:Place) extends Ant {
  var pl: Place = position
  val im = (new ImageIcon("img/ant_harvester.png")).getImage( )
  var armor:Int = 1
  val cost:Int = 2
  def move() = Food.count += 1
}

class ThrowerAnt(position:Place) extends Ant {
  var pl:Place = position
  val im = (new ImageIcon("img/ant_thrower.png")).getImage( )
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
  val im = (new ImageIcon("img/bee.png")).getImage( )
  var pl:Place = position
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
  val in: Place
  val out: Place
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
}

class MiddlePlace(position:Point, entree:Place, sortie:Place) extends Place {
  val pos = position
  val in:Place = entree
  val out:Place = sortie
}
  
class LeftPlace(position: Point, entree:Place) extends Place {
  val pos = position
  val in: Place = entree
  val out: Place = this
}

class RightPlace(position: Point, sortie:Place) extends Place {
  val pos = position
  val in: Place = this
  val out: Place = sortie
}

object placesSet {
  val pt1 = new Point(10, 10)
  val pt2 = new Point(20, 10)
  val pt3 = new Point(30, 10)
  val pt4 = new Point(40, 10)
  val pl1: Place = new MiddlePlace(pt1, pl2, pl1)
  val pl2: Place = new MiddlePlace(pt2, pl3, pl1)
  val pl3: Place = new MiddlePlace(pt3, pl4, pl2)
  val pl4: Place = new MiddlePlace(pt4, pl4, pl3)
  val places = List(pl1, pl2, pl3, pl4)
}

object Game extends SimpleSwingApplication {
  
  lazy val ui = new Panel {
    
    
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      for (pl <- placesSet.places) {
        pl.ant match{
          case a: NoAnt => 
          case a => g.drawImage(a.im, pl.pos.x, pl.pos.y, peer)
        }
        pl.inside match{
          case Nil => {}
          case bee::others => {
            g.drawImage(bee.im, pl.pos.x, pl.pos.y, peer)
            g.drawString("X"+pl.inside.length, pl.pos.x, pl.pos.y)
          }
        }
      }       
    }
  }
}
