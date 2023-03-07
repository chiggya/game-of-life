package logic

import cats.effect.*
import cats.implicits.*
import Life.*

object IOFunctions:
  

  def clearScreen: IO[Unit] = 
    IO.print("\u001B[2J")

  def goto(p: Pos): IO[Unit] = 
    val (x,y) = p
    IO.print(s"\u001B[${x};${y}H")

  def writeTo(p: Pos, s: String): IO[Unit] =
    goto(p) *> IO.print(s)
  
  def showCells(g: Grid): IO[Unit] = 
    (for
      p <- g
    yield writeTo(p, "O")).sequence_



object Patterns:
  val glider: Grid = List((2,4), (3,2), (3,4), (4,3), (4,4))

  val pulsar: Grid = List(
    (2, 4),(2, 5),(2, 6),(2, 10),(2, 11),(2, 12),
           (4, 2),(4, 7),( 4, 9),(4, 14),
           (5, 2),(5, 7),( 5, 9),(5, 14),
           (6, 2),(6, 7),( 6, 9),(6, 14),
    (7, 4),(7, 5),(7, 6),(7, 10),(7, 11),(7, 12),
    (9, 4),(9, 5),(9, 6),(9, 10),(9, 11),(9, 12),
           (10,2),(10,7),(10, 9),(10,14),
           (11,2),(11, 7),(11, 9),(11,14),
           (12,2),(12,7),(12,9),(12,14),
    (14,4),(14,5),(14,6),(14,10),(14,11),(14,12))

  val toad: Grid = List(
          (7,7),(7,8),(7,9),
    (8,6),(8,7),(8,8)
  )
  val blinker: Grid = List(
    (7,7),(7,8),(7,9)
  )