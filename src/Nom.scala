import java.awt.{Color, Dimension}
import scala.swing._

object FirstTest extends SimpleSwingApplication{
 lazy val ui = new Panel{
  background = Color.white
  preferredSize = new Dimension(800, 600)
 }
 /*Create a new window and populate in */
 def top = new MainFrame{
  title = "Simple Demo Application"
  contents = ui
 }
}
