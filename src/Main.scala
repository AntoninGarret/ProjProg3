import java.awt.Color
import java.awt.Graphics2D
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import scala.swing.Dimension
import scala.swing.Image
import scala.swing.MainFrame
import scala.swing.Panel
import scala.swing.SimpleSwingApplication
import scala.swing.event.KeyTyped
import scala.swing.event.MousePressed
import javax.swing.ImageIcon
import javax.swing.Timer
import java.awt.Point

object Game extends SimpleSwingApplication {

  var food = 10
  var currentAnt: Option[Ant] = None
  var market = List(new MiddlePlace(new Point(10 + 2 * 93, 10)))
  for (i <- 1 to 2) {
    market = new MiddlePlace(new Point(10 + (2 - i) * 93, 10)) :: market
  }
  val ants = List(new HarvesterAnt(market(0)), new ThrowerAnt(market(1)), new ScubaAnt(market(2)))

  lazy val ui = new Panel {

    background = Color.white
    preferredSize = new Dimension(800, 700)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case KeyTyped(_, 'n', _, _) => for (pl <- placesSet.tunnels(0)) {
        pl.drown()
        pl.ant.move()
        for (bee <- pl.inside) {
          bee.move()
        }
      }
      case KeyTyped(_, 'b', _, _) => placesSet.tunnels(0)(7).addBee(new Bee(placesSet.tunnels(0)(7)))
      case e: MousePressed =>
        if (currentAnt == None) {
          e.point match {
            case p if (10 < p.y & p.y < 98) => {
              if (food >= ants((p.x - 10) / 93).cost) {
                currentAnt = Some(ants((p.x - 10) / 93))
                food -= ants((p.x - 10) / 93).cost
              }
            }
            case _ =>
          }
        } 
        else {
          for(tunnel:List[Place] <- placesSet.tunnels){
            for(place:Place <- tunnel){
              
              if (place.isInPlace(e.point)){currentAnt match {
                case Some(a: HarvesterAnt) => {
                  place.ant = new HarvesterAnt(place)
                  currentAnt = None
                }
                case Some(a: ThrowerAnt) => {
                  place.ant = new ThrowerAnt(place)
                  currentAnt = None
                }
                case Some(a: ScubaAnt) => {
                  place.ant = new ScubaAnt(place)
                  currentAnt = None
                }
              }  
              }
            }
          }    
        }
      case _ =>
          
        }
    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      currentAnt match {
        case None    =>
        case Some(a) => g.drawImage(a.im, 700, 580, peer)
      }

      g.drawString("Food : " + food, 10, 580)
      for (ant <- ants) {
        g.drawImage(ant.im, ant.pl.pos.x, ant.pl.pos.y, peer)
      }
      for (i <- 0 to 2) {
        for (pl <- placesSet.tunnels(i)) {
          /* g.setColor(Color.black)
    	    g.draw(pl.boite)*/
          if (pl.isWater) {
            g.setColor(Color.blue)
            g.fillRect(pl.pos.x, pl.pos.y, 93, 98)
          }
          g.drawImage(pl.im, pl.pos.x, pl.pos.y, peer)
          pl.ant match {
            case a: NoAnt =>
            case a        => g.drawImage(a.im, pl.pos.x, pl.pos.y + 20, peer)
          }
          pl.inside match {
            case Nil => {}
            case bee :: others => {
              g.drawImage(bee.im, pl.pos.x + 30, pl.pos.y - 10, peer)
              g.setColor(Color.white)
              g.drawString("X" + pl.inside.length, pl.pos.x + 60, pl.pos.y + 30)
            }
          }
        }
      }
    }
  }
  class MyTimer extends ActionListener {
    /* Configuration */
    val fpsTarget = 50 // Desired amount of frames per second
    var delay = 1000 / fpsTarget

    /* The swing timer */
    val timer = new Timer(delay, this)
    timer.setCoalesce(true) // Please restart by yourself
    timer.start() // Let's go

    /* react to the timer events */
    def actionPerformed(e: ActionEvent): Unit = {
      ui.repaint() // Tell Scala that the image should be redrawn
    }
  }
  val t = new MyTimer()

  // Part 4: Main initialization: Create a new window and populate it
  //////////////////////////////
  def top = new MainFrame {
    title = "Ants vs SomeBees"
    contents = ui
  }

}