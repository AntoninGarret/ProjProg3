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
import scala.util.Random

object Game extends SimpleSwingApplication {

  var food = 10
  var nextWave = 3
  var beesPerWave = 1

  var NoQueen = true
  var currentAnt: Option[Ant] = None

  val nbAnts = 8
  var market = List(new MiddlePlace(new Point(10, 10)))
  for (i <- 1 to 7) {
    market = new MiddlePlace(new Point(10 + i * 93, 10)) :: market
  }
  for (i <- 0 to (nbAnts - 7)){
    market = new MiddlePlace(new Point(10 + i * 93, 103)) :: market
  }
  market(0).ant = new HarvesterAnt(market(0))
  market(1).ant = new ShortThrowerAnt(market(1))
  market(2).ant = new LongThrowerAnt(market(2))
  market(3).ant = new ScubaAnt(market(3))
  market(4).ant = new NinjaAnt(market(4))
  market(5).ant = new HungryAnt(market(5))
  market(6).ant = new BodyguardAnt(market(6), new NoAnt(market(6)))
  market(7).ant = new QueenAnt(market(7))
  market(8).ant = new WallAnt(market(8))
  market(9).ant = new FireAnt(market(9))

  var beesWin = false

  lazy val ui = new Panel {

    background = Color.white
    preferredSize = new Dimension(800, 700)

    focusable = true
    listenTo(mouse.clicks, mouse.moves, keys)

    reactions += {
      case KeyTyped(_, 'n', _, _) => {
        nextWave match{
          case 0 =>{
            for (i <-0 to (beesPerWave-1)){
              var a = Random.nextInt(3)
              placesSet.tunnels(a)(7).addBee(new Bee(placesSet.tunnels(a)(7)))
            }
            nextWave = 3
            if (beesPerWave != 7) beesPerWave +=1
          }
          case _ => nextWave -=1
        }
        for (i <- 0 to 2) {
          for (pl <- placesSet.tunnels(i)) {
            pl.drown()
            pl.ant.move()
            for (bee <- pl.inside) {
              bee.move()
            }
          }
        }
      }
      case KeyTyped(_, 'b', _, _) => placesSet.tunnels(0)(7).addBee(new Bee(placesSet.tunnels(0)(7)))
      case e: MousePressed =>
        if (currentAnt == None) {
          for (pl <- market){
              if (pl.isInPlace(e.point)) {
                if (food >= pl.ant.cost){
                  currentAnt = Some(pl.ant)
                  food -= pl.ant.cost
                }  
              /*case p if (10 < p.y & p.y < 98) => {
                if (food >= ants((p.x - 10) / 93).cost) {
                  currentAnt = Some(ants((p.x - 10) / 93))
                  food -= ants((p.x - 10) / 93).cost
                }*/
              }
          }
        } 
        else {
          for (tunnel: List[Place] <- placesSet.tunnels) {
            for (place: Place <- tunnel) {
              if (place.isInPlace(e.point)) {
                currentAnt match {
                  case Some(a: HarvesterAnt) => {
                    place.ant = new HarvesterAnt(place)
                    currentAnt = None
                  }
                  case Some(a: ShortThrowerAnt) => {
                    place.ant = new ShortThrowerAnt(place)
                    currentAnt = None
                  }
                  case Some(a: LongThrowerAnt) => {
                    place.ant = new LongThrowerAnt(place)
                    currentAnt = None
                  }
                  case Some(a: ScubaAnt) => {
                    place.ant = new ScubaAnt(place)
                    currentAnt = None
                  }
                  case Some(a: NinjaAnt) => {
                    place.ant = new NinjaAnt(place)
                    currentAnt = None
                  }
                  case Some(a: HungryAnt) => {
                    place.ant = new HungryAnt(place)
                    currentAnt = None
                  }
                  case Some(a: BodyguardAnt) => {
                    place.ant = new BodyguardAnt(place, place.ant)
                    currentAnt = None
                  }
                  case Some(a: WallAnt) => {
                    place.ant = new WallAnt(place)
                    currentAnt = None
                  }
                  case Some(a: FireAnt) => {
                    place.ant = new FireAnt(place)
                    currentAnt = None
                  }
                  case Some(a: QueenAnt) => {
                    if (NoQueen) {
                      place.ant = new QueenAnt(place)
                      NoQueen = false
                    }
                    currentAnt = None
                  }
                  case _ =>
                }
              }
            }
          }
        }
      case _ =>

    }

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      currentAnt match {
        case None    =>
        case Some(a) => g.drawImage(a.im, 700, 580, peer)
      }

      g.drawString("Food : " + food, 10, 580)
      g.drawString("next wave in : " + nextWave + " turns", 10, 600)
      for (pl <- market) {
        g.drawImage(pl.ant.im, pl.ant.pl.pos.x, pl.ant.pl.pos.y, peer)
      }
      if (beesWin) {
        g.drawString("The bees win", 400, 580)
      }
      for (i <- 0 to 2) {
        for (pl <- placesSet.tunnels(i)) {
          /* g.setColor(Color.black)
    	    g.draw(pl.boite)*/
          g.drawImage(pl.im, pl.pos.x, pl.pos.y, peer)
          pl.ant match {
            case a: NoAnt =>
            case a: ThrowerAnt =>{
                g.drawImage(a.im, pl.pos.x, pl.pos.y + 20, peer)
                if (a.ant_leaf.display) g.drawImage(a.ant_leaf.im, a.ant_leaf.x, pl.pos.y + 30, peer)
              }
            case a: BodyguardAnt => a.protectedAnt match {
              case p: NoAnt => g.drawImage(a.im, pl.pos.x, pl.pos.y + 20, peer)
              case p =>  g.drawImage(p.im, pl.pos.x, pl.pos.y + 20, peer)
                          g.drawImage(a.im, pl.pos.x, pl.pos.y + 20, peer)
            }
            case a => g.drawImage(a.im, pl.pos.x, pl.pos.y + 20, peer)
          }
          pl.inside match {
            case Nil => {}
            case bee :: others => {
              g.drawImage(bee.im, bee.x + 30, pl.pos.y - 10, peer)
              g.setColor(Color.white)
              g.drawString("X" + pl.inside.length, bee.x + 60, pl.pos.y + 30)
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
        for (i <- 0 to 2) {
          for (pl <- placesSet.tunnels(i)) {
            pl.ant match{
              case a: ThrowerAnt =>{
                a.ant_leaf.deplace()
              }
              case _ =>{}
            }
            for (bee <- pl.inside) {
              bee.deplace()
            }
          }
        }
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