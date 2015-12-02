import scala.swing._
import java.awt.event
import java.awt.event.ActionListener
import javax.swing.Timer
import java.awt.event.ActionEvent
import java.awt.{ Color, Graphics2D, Point, geom, MouseInfo }
import scala.swing.event.MousePressed
import scala.swing.event.KeyTyped

object Game extends SimpleSwingApplication {
  
  lazy val ui = new Panel {
    
    background = Color.white
    preferredSize = new Dimension(800, 600)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case KeyTyped(_, 'n', _, _)=> {for (pl <- placesSet.places) {
        pl.ant.move()
        for (bee <- pl.inside) {
          bee.move()
        }
      }
      placesSet.places(7).addBee(new Bee(placesSet.places(7)))}
      case e: MousePressed =>
    }
    
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      for (pl <- placesSet.places) {
       /* g.setColor(Color.black)
        g.draw(pl.boite)*/
        g.drawImage(pl.im, pl.pos.x, pl.pos.y, peer) 
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
  class MyTimer extends ActionListener {
    /* Configuration */
    val fpsTarget = 50 // Desired amount of frames per second
    var delay = 1000 / fpsTarget

    /* The swing timer */
    val timer = new Timer(delay, this)
    timer.setCoalesce(true) // Please restart by yourself
    timer.start()           // Let's go
    
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