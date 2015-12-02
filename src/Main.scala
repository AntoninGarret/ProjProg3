import scala.swing._
import java.awt.event
import java.awt.event.ActionListener
import javax.swing.Timer
import java.awt.event.ActionEvent
import java.awt.{ Color, Graphics2D, Point, geom, MouseInfo }
import scala.swing.event.MousePressed
import scala.swing.event.KeyTyped
import javax.swing.ImageIcon

object Game extends SimpleSwingApplication {
  
  var food = 10
  var currentAnt = ""
  val im_thrower: Image = (new ImageIcon("img/ant_thrower.png")).getImage( )
  val im_harvester: Image = (new ImageIcon("img/ant_harvester.png")).getImage( )
  
  lazy val ui = new Panel {
    
    background = Color.white
    preferredSize = new Dimension(800, 600)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case KeyTyped(_, 'n', _, _) => for (pl <- placesSet.places) {
        pl.ant.move()
        for (bee <- pl.inside) {
          bee.move()
        }
        pl.update()
      }
      case KeyTyped(_, 'b', _, _) => placesSet.places(7).addBee(new Bee(placesSet.places(7)))
      case e: MousePressed => 
        if (currentAnt == "") {
          e.point match {
            case p if (10 < p.x & p.x < 76 &  10 < p.y & p.y < 76) => {
              if (food >= 2) {
                currentAnt = "Harvester Ant" 
                food -= 2}}
            case p if (76 < p.x & p.x < 142 &  10 < p.y & p.y < 76) => {
              if (food >= 2) {
                currentAnt = "Thrower Ant" 
                food -= 2}}
            case _ =>
            }
        } else {
          e.point match {
            case p if (200 < p.y & p.y < 298) => currentAnt match {
            case "Harvester Ant" => {placesSet.places((p.x-10)/93).ant = new HarvesterAnt(placesSet.places((p.x-10)/93))
                                     currentAnt = ""}
            case "Thrower Ant" => {placesSet.places((p.x-10)/93).ant = new ThrowerAnt(placesSet.places((p.x-10)/93))
                                   currentAnt = ""}
            case _ =>
            }
        }
      }
    }
    
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.drawString(currentAnt, 700, 580)
      g.drawString(""+food, 10, 580)
      g.drawImage(im_harvester, 10, 10, peer)
      g.drawImage(im_thrower, 76, 10, peer)
      for (pl <- placesSet.places) {
       /* g.setColor(Color.black)
        g.draw(pl.boite)*/
        g.drawImage(pl.im, pl.pos.x, pl.pos.y, peer) 
        pl.ant match{
          case a: NoAnt => 
          case a => g.drawImage(a.im, pl.pos.x, pl.pos.y+20, peer)
        }
        pl.inside match{
          case Nil => {}
          case bee::others => {
            g.drawImage(bee.im, pl.pos.x+30, pl.pos.y-10, peer)
            g.drawString("X"+pl.inside.length, pl.pos.x + 60, pl.pos.y+30)
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