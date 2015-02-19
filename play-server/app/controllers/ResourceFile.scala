package controllers

import play.api.mvc.{Action, Controller}

object ResourceFile extends Controller {
  def at(file: String) = Action {
    Option(play.Play.application.resourceAsStream(file))
      .map({ s => Ok(io.Source.fromInputStream(s).mkString) })
      .getOrElse(NotFound)
  }
}
