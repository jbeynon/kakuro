package jbeynon.kakuro

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

import org.scalajs.dom
import dom.document
import dom.raw.{ Node, KeyboardEvent }

import org.scalajs.jquery.jQuery

import scalatags.JsDom.all._

import rx._

import Framework._

object KakuroUI extends JSApp {
  val game = Var(sampleGame)

  def main {}

  def removeChildren(node: Node) {
    while (node.hasChildNodes) {
      node.removeChild(node.firstChild)
    }
  }

  var shiftKey = false

  val mappings = Map(
    '1' -> 1, '2' -> 2, '3' -> 3,
    '4' -> 4, '5' -> 5, '6' -> 6,
    '7' -> 7, '8' -> 8, '9' -> 9)

  def handleKeyDown(e: KeyboardEvent) {
    val position = Position(e.srcElement.attributes.getNamedItem("data-x").value.toInt, e.srcElement.attributes.getNamedItem("data-y").value.toInt)

    e.keyCode match {
      case 16 => // Shift
        shiftKey = true
      case x if mappings.contains(x.toChar) => 
        mappings(x.toChar) match {
          case value if shiftKey == false =>
            game() = game().copy(pens = updateGrid[Int](game().pens, game().board, position, _ => value))
          case value if shiftKey == true =>
            if (getGrid(game().pencils, game().board, position).contains(value))
              game() = game().copy(pencils = updateGrid[Set[Int]](game().pencils, game().board, position, _ - value))
            else
              game() = game().copy(pencils = updateGrid[Set[Int]](game().pencils, game().board, position, _ + value))
        }
      case 8 => // Backspace
        game() = game().copy(pens = updateGrid[Int](game().pens, game().board, position, _ => 0))
      case 38 => // Up
        val spaces = for {
          y <- 0 until position.y
          newPosition = position.copy(y = y)
          if getGrid(game().board.values, game().board, newPosition) > 0
        } yield newPosition
        val newPosition = spaces.lastOption.getOrElse(position)
        jQuery(s"[data-x='${ newPosition.x }'][data-y='${ newPosition.y }']").focus
        e.preventDefault
      case 40 => // Down
        val spaces = for {
          y <- position.y + 1 until game().board.size
          newPosition = position.copy(y = y)
          if getGrid(game().board.values, game().board, newPosition) > 0
        } yield newPosition
        val newPosition = spaces.headOption.getOrElse(position)
        jQuery(s"[data-x='${ newPosition.x }'][data-y='${ newPosition.y }']").focus
        e.preventDefault
      case 37 => // Left
        val spaces = for {
          x <- 0 until position.x
          newPosition = position.copy(x = x)
          if getGrid(game().board.values, game().board, newPosition) > 0
        } yield newPosition
        val newPosition = spaces.lastOption.getOrElse(position)
        jQuery(s"[data-x='${ newPosition.x }'][data-y='${ newPosition.y }']").focus
        e.preventDefault
      case 39 => // Right
        val spaces = for {
          x <- position.x + 1 until game().board.size
          newPosition = position.copy(x = x)
          if getGrid(game().board.values, game().board, newPosition) > 0
        } yield newPosition
        val newPosition = spaces.headOption.getOrElse(position)
        jQuery(s"[data-x='${ newPosition.x }'][data-y='${ newPosition.y }']").focus
        e.preventDefault
      case _ =>
    }
  }

  def handleKeyUp(e: KeyboardEvent) {
    e.keyCode match {
      case 16 => shiftKey = false
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
                    onkeydown := handleKeyDown _,
                    onkeyup := handleKeyUp _,
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
