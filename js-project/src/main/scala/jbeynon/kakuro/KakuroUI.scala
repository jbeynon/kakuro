package jbeynon.kakuro

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import scalatags.JsDom.all._

import rx._

import org.scalajs.dom
import dom.document
import dom.raw.{ Node, KeyboardEvent }

import Framework._

object KakuroUI extends JSApp {
  val game = Var(sampleGame)

  def main {}

  def removeChildren(node: Node) {
    while (node.hasChildNodes) {
      node.removeChild(node.firstChild)
    }
  }

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

  def handleKeypress(e: KeyboardEvent) {
    val position = Position(e.srcElement.attributes.getNamedItem("data-x").value.toInt, e.srcElement.attributes.getNamedItem("data-y").value.toInt)

    mappings.get(e.charCode.toChar.toString) match {
      case Some((value, true)) =>
        game() = game().copy(pens = updateGrid[Int](game().pens, game().board, position, _ => value))
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
    val (across, down) = createClues(game().board)

    val dest = document.getElementById("kakuroTable")
    removeChildren(dest)

    dest.appendChild(
      table(borderCollapse := "collapse")(
        for { y <- 0 until game().board.size } yield {
          tr(
            for { x <- 0 until game().board.size } yield {
              val position = Position(x, y)
              val value = getGrid(game().board.values, game().board, position)

              td(
                data.x := x, data.y := y,
                textAlign := "center",
                width := "5em", height := "5em",
                border := "2px solid black",
                padding := "0px",
                if (value < 1 || value > 9) {
                  backgroundColor := "gray"
                } else {
                  Seq(
                    onkeypress := handleKeypress _,
                    tabindex := 1)
                }
              )(
                if (value >= 1 && value <= 9) {
                  Rx {
                    if (getGrid(game().pens, game().board, position) > 0)
                      span(fontWeight := "bold", fontSize := "3em")(getGrid(game().pens, game().board, position).toString)
                    else
                      new StringFrag(getGrid(game().pencils, game().board, position).toSeq.sorted.mkString(", "))
                  }
                } else {
                  Seq(
                    across.get(position).map(value => new StringFrag(s"$value\u2192")),
                    Some(br),
                    down.get(position).map(value => new StringFrag(s"$value\u2193")))
                }
              )
            }
          )
        }
      ).render)
  }
}
