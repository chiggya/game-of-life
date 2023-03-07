package logic

import cats.effect.*
import cats.implicits.*
import scala.concurrent.duration.*
import Life.*
import IOFunctions.*


object LifeMain extends IOApp:
  
  def beginLife(g: Grid): IO[Unit] = 
    clearScreen *>
    showCells(g) *>
    goto(width + 10, height) *>
    IO.sleep(350.millis) >>
    beginLife(nextGeneration(g))
  
  def run(args: List[String]): IO[ExitCode] =
    beginLife(Patterns.pulsar).as(ExitCode.Success)

