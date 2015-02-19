package jbeynon.kakuro

import scala.util.{Failure, Success, Random}

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import scalatags.JsDom.all._

import rx._

import org.scalajs.dom
import dom.{ document, Element }
import dom.raw.{ Node, KeyboardEvent }

/**
 * A minimal binding between Scala.Rx and Scalatags and Scala-Js-Dom.
 * Taken from lihaoyi's TodoMVC example.
 */
object Framework {
  /**
   * Wraps reactive strings in spans, so they can be referenced/replaced
   * when the Rx changes.
   */
  implicit def RxStr[T](r: Rx[T])(implicit f: T => Frag): Modifier = {
    rxMod(Rx(span(r())))
  }
 
  /**
   * Sticks some Rx into a Scalatags fragment, which means hooking up an Obs
   * to propagate changes into the DOM via the element's ID. Monkey-patches
   * the Obs onto the element itself so we have a reference to kill it when
   * the element leaves the DOM (e.g. it gets deleted).
   */
  implicit def rxMod(r: Rx[HtmlTag]): Modifier = {
    def rSafe = r.toTry match {
      case Success(v) => v.render
      case Failure(e) => span(e.toString, backgroundColor := "red").render
    }
    var last = rSafe
    Obs(r, skipInitial = true){
      val newLast = rSafe
      last.parentElement.replaceChild(newLast, last)
      last = newLast
    }
    last
  }
  implicit def RxAttrValue[T: AttrValue] = new AttrValue[Rx[T]]{
    def apply(t: Element, a: Attr, r: Rx[T]): Unit = {
      Obs(r){ implicitly[AttrValue[T]].apply(t, a, r())}
    }
  }
  implicit def RxStyleValue[T: StyleValue] = new StyleValue[Rx[T]]{
    def apply(t: Element, s: Style, r: Rx[T]): Unit = {
      Obs(r){ implicitly[StyleValue[T]].apply(t, s, r())}
    }
  }
}

import Framework._

object KakuroUI extends JSApp {
  val game = Var(sampleGame)

  def main {}

  def removeChildren(node: Node) {
    while (node.hasChildNodes) {
      node.removeChild(node.firstChild)
    }
  }

  def handleKeypress(e: KeyboardEvent) {
    val mappings = Map(
      "0" -> (0, true), ")" -> (0, false),
      "1" -> (1, true), "!" -> (1, false),
      "2" -> (2, true), "@" -> (2, false),
      "3" -> (3, true), "#" -> (3, false),
      "4" -> (4, true), "$" -> (4, false),
      "5" -> (5, true), "%" -> (5, false),
      "6" -> (6, true), "^" -> (6, false),
      "7" -> (7, true), "&" -> (7, false),
      "8" -> (8, true), "*" -> (8, false),
      "9" -> (9, true), "(" -> (9, false))

    val cellRegex = "cell(\\d+)_(\\d+)".r
    val position = e.srcElement.id match {
      case cellRegex(x, y) => Position(x.toInt, y.toInt)
    }

    mappings.get(e.charCode.toChar.toString) match {
      case Some((value, true)) => game() = game().copy(pens = updateGrid[Int](game().pens, game().board, position, _ => value))
      case Some((value, false)) =>
        if (getGrid(game().pencils, game().board, position).contains(value))
          game() = game().copy(pencils = updateGrid[Set[Int]](game().pencils, game().board, position, _ - value))
        else
          game() = game().copy(pencils = updateGrid[Set[Int]](game().pencils, game().board, position, _ + value))
      case _ =>
    }
  }

  @JSExport
  def drawTable {
    val dest = document.getElementById("kakuroTable")
    removeChildren(dest)

    val tab = table(borderCollapse := "collapse")(
      for { y <- 0 until game().board.size } yield {
        tr(
          for { x <- 0 until game().board.size } yield {
            val value = getGrid(game().board.values, game().board, Position(x, y))

            td(
              id := s"cell${x}_${y}",
              tabindex := 1,
              textAlign := "center",
              width := "5em", height := "5em",
              border := "2px solid black",
              padding := "0px",
              if (value < 1 || value > 9) backgroundColor := "gray"
              else onkeypress := handleKeypress _)(
              if (value >= 1 && value <= 9) Rx {
                if (getGrid(game().pens, game().board, Position(x, y)) > 0)
                  getGrid(game().pens, game().board, Position(x, y)).toString
                else
                  getGrid(game().pencils, game().board, Position(x, y)).toSeq.sorted.mkString(", ")
              })
          }
        )
      }
    )

    dest.appendChild(tab.render)

    val (across, down) = createClues(game().board)

    across.foreach({case (Position(x, y), value) =>
      val cell = document.getElementById(s"cell${x}_${y}")
      cell.appendChild(document.createTextNode(s"$value\u2192"))
    })

    down.foreach({case (Position(x, y), value) =>
      val cell = document.getElementById(s"cell${x}_${y}")
      cell.appendChild(document.createElement("br"))
      cell.appendChild(document.createTextNode(s"$value\u2193"))
    })
  }
}
