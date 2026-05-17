import scala.io.StdIn
import java.io.PrintWriter

trait Monad1[M[_]] {
  def pure[X](x: X): M[X]

  def flatMap[X, Y](mx: M[X])(f: X => M[Y]): M[Y]

  def map[X, Y](mx: M[X])(f: X => Y): M[Y] =
    flatMap(mx)(x => pure(f(x)))

  def flatten[X](mmx: M[M[X]]): M[X] =
    flatMap(mmx)(identity)

  def fish[A, B, C](f1: A => M[B], f2: B => M[C]): A => M[C] =
    a => flatMap(f1(a))(f2)
}

trait Monad2[M[_]] {
  def pure[X](x: X): M[X]

  def fish[A, B, C](f1: A => M[B], f2: B => M[C]): A => M[C]

  def flatMap[X, Y](mx: M[X])(f: X => M[Y]): M[Y] =
    fish[Unit, X, Y](_ => mx, f)(())

  def map[X, Y](mx: M[X])(f: X => Y): M[Y] =
    fish[Unit, X, Y](_ => mx, x => pure(f(x)))(())

  def flatten[X](mmx: M[M[X]]): M[X] =
    fish[Unit, M[X], X](_ => mmx, identity)(())
}

trait Monad3[M[_]] {
  def pure[X](x: X): M[X]

  def map[X, Y](mx: M[X])(f: X => Y): M[Y]

  def flatten[X](mmx: M[M[X]]): M[X]

  def flatMap[X, Y](mx: M[X])(f: X => M[Y]): M[Y] =
    flatten(map(mx)(f))

  def fish[A, B, C](f1: A => M[B], f2: B => M[C]): A => M[C] =
    a => flatten(map(f1(a))(f2))
}

// Monoid для логов
trait Monoid[L]:
  def unit: L
  def combine(a: L, b: L): L

given Monoid[Vector[String]] with
  def unit = Vector.empty
  def combine(a: Vector[String], b: Vector[String]) = a ++ b

case class Reader[Env, A](run: Env => A) {
  def map[B](f: A => B): Reader[Env, B] = Reader(e => f(run(e)))
  def flatMap[B](f: A => Reader[Env, B]): Reader[Env, B] = Reader(e => f(run(e)).run(e))
}
object Reader {
  def pure[Env, A](a: A): Reader[Env, A] = Reader(_ => a)
  def ask[Env]: Reader[Env, Env] = Reader(identity)
}

case class Writer[Log, A](run: (Log, A)) {
  def map[B](f: A => B): Writer[Log, B] = Writer((run._1, f(run._2)))
  def flatMap[B](f: A => Writer[Log, B])(using L: Monoid[Log]): Writer[Log, B] = {
    val (log1, a) = run
    val (log2, b) = f(a).run
    Writer((L.combine(log1, log2), b))
  }
}
object Writer {
  def pure[Log, A](a: A)(using L: Monoid[Log]): Writer[Log, A] = Writer((L.unit, a))
  def tell[Log](log: Log): Writer[Log, Unit] = Writer((log, ()))
}

extension [L, A](w: Writer[L, A])
  def >>[B](mb: Writer[L, B])(using M: Monoid[L]): Writer[L, B] = w.flatMap(_ => mb)

case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State(s => { val (s2, a) = run(s); (s2, f(a)) })
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => { val (s2, a) = run(s); f(a).run(s2) })
}
object State {
  def pure[S, A](a: A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def put[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](f: S => S): State[S, Unit] = State(s => (f(s), ()))
}

case class IO[A](unsafeRun: () => A) {
  def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
  def runNow(): A = unsafeRun()
}
object IO {
  def pure[A](a: A): IO[A] = IO(() => a)
}

extension [A](io: IO[A])
  def *>[B](mb: IO[B]): IO[B] = io.flatMap(_ => mb)

case class Config(
                   totalSpaces: Int,
                   ratePerHour: Double,
                   lostTicketPenalty: Double,
                   roundUp: Double => Long
                 )

/*def parkingCost(hours: Double): Reader[Config, Double] = Reader { config =>
  config.roundUp(hours).toDouble * config.ratePerHour
}
def lostTicketCost: Reader[Config, Double] = Reader(_.lostTicketPenalty)
def bill(entryTime: Double, exitTime: Double): Reader[Config, Double] = parkingCost(exitTime - entryTime)
def canEnter(freePlaces: Int): Reader[Config, Boolean] = Reader(config => freePlaces > 0)*/

type Log = Vector[String]
type Command = (String, ParkState, Config, Log) => IO[Unit]

case class ParkState(
                      occupiedSpaces: Set[Int],           // номера занятых мест (1..totalSpaces)
                      carToSpace: Map[String, Int],       // машина -> номер места
                      carMap: Map[String, Double],        // машина -> время въезда
                      currentHour: Double,                // текущий час (используется при выезде)
                      revenue: Double
                    )

type ParkingProgram[A] = Reader[Config, Writer[Log, State[ParkState, A]]]

def enterCar(carNumber: String): ParkingProgram[Either[String, Int]] =
  Reader { config =>
    Writer(
      (Vector(s"Въезд: $carNumber"),
        State[ParkState, Either[String, Int]] { state =>
          val occupied = state.occupiedSpaces
          val allSpaces = (1 to config.totalSpaces).toSet
          val freeSpaces = allSpaces diff occupied
          freeSpaces.headOption match {
            case Some(space) =>
              val newState = state.copy(
                occupiedSpaces = occupied + space,
                carToSpace = state.carToSpace + (carNumber -> space),
                carMap = state.carMap + (carNumber -> state.currentHour)
              )
              (newState, Right(space))
            case None =>
              (state, Left(s"Нет свободных мест (всего ${config.totalSpaces})"))
          }
        }
      )
    )
  }

def exitCar(carNumber: String): ParkingProgram[Either[String, Double]] =
  Reader { config =>
    Writer.tell(Vector(s"Выезд: $carNumber")) >> Writer.pure(
      State[ParkState, Either[String, Double]] { state =>
        state.carMap.get(carNumber) match {
          case Some(entryTime) =>
            val hoursRaw = state.currentHour - entryTime
            val hours = if (hoursRaw < 0) 0.0 else config.roundUp(hoursRaw).toDouble
            val cost = hours * config.ratePerHour
            val space = state.carToSpace(carNumber)
            val newState = state.copy(
              occupiedSpaces = state.occupiedSpaces - space,
              carToSpace = state.carToSpace - carNumber,
              carMap = state.carMap - carNumber,
              revenue = state.revenue + cost
            )
            (newState, Right(cost))
          case None =>
            (state, Left(s"Машина $carNumber не найдена на парковке"))
        }
      }
    )
  }

def reportLostTicket(carNumber: String): ParkingProgram[Either[String, Double]] =
  Reader { config =>
    Writer.tell(Vector(s"Потерян билет для $carNumber, штраф ${config.lostTicketPenalty}")) >> Writer.pure(
      State[ParkState, Either[String, Double]] { state =>
        state.carMap.get(carNumber) match {
          case Some(_) =>
            val space = state.carToSpace(carNumber)
            val penalty = config.lostTicketPenalty
            val newState = state.copy(
              occupiedSpaces = state.occupiedSpaces - space,
              carToSpace = state.carToSpace - carNumber,
              carMap = state.carMap - carNumber,
              revenue = state.revenue + penalty
            )
            (newState, Right(penalty))
          case None =>
            (state, Left(s"Машина $carNumber не найдена"))
        }
      }
    )
  }

def nextHour(delta: Double): ParkingProgram[Either[String, Unit]] =
  Reader { config =>
    Writer.tell(Vector(s"Переключение времени на +$delta")) >> Writer.pure(
      State[ParkState, Either[String, Unit]] { state =>
        if (delta > 0) {
          val newState = state.copy(currentHour = state.currentHour + delta)
          (newState, Right(()))
        } else {
          (state, Left("Время должно быть положительным"))
        }
      }
    )
  }

object ParkingApp {
  def printLine(msg: String): IO[Unit] = IO(() => println(msg))
  def readLine: IO[String] = IO(() => StdIn.readLine())

  def readCarNumber: IO[String] = {
    def loop: IO[String] =
      for {
        _ <- printLine("Введите номер машины:")
        input <- readLine
        trimmed = input.trim
        _ <- if (trimmed.isEmpty) printLine("Номер не может быть пустым!") *> loop
        else if (Set("enter", "exit", "lost", "next", "quit").contains(trimmed.toLowerCase))
          printLine("Номер не может совпадать с названием команды!") *> loop
        else IO.pure(())
      } yield trimmed
    loop
  }

  def readPositiveDouble(prompt: String): IO[Double] = {
    def loop: IO[Double] =
      for {
        _ <- printLine(prompt)
        line <- readLine
        value <- try {
          val d = line.toDouble
          if (d <= 0) printLine("Значение должно быть больше нуля!") *> loop
          else IO.pure(d)
        } catch {
          case _: NumberFormatException => printLine("Ошибка: введите число!") *> loop
        }
      } yield value
    loop
  }

  def writeLogToFile(log: Vector[String]): IO[Unit] = IO(() => {
    val pw = new PrintWriter("parking.log")
    try log.foreach(pw.println)
    finally pw.close()
  })

  // Обработчики
  def handleEnter(carNumber: String, state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] = {
    val prog = enterCar(carNumber)
    val (log, stateProg) = prog.run(config).run
    val (newState, result) = stateProg.run(state)
    log.foreach(println)
    val extraMsg = result match {
      case Right(space) =>
        val freeSpaces = config.totalSpaces - newState.occupiedSpaces.size
        s"Машина $carNumber заехала на место $space в момент ${state.currentHour}. Свободно мест: $freeSpaces"
      case Left(err) => s"Ошибка: $err"
    }
    printLine(extraMsg) *> loop(newState, config, logAcc ++ log ++ Vector(extraMsg))
  }

  def handleExit(carNumber: String, state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] = {
    val prog = exitCar(carNumber)
    val (log, stateProg) = prog.run(config).run
    val (newState, result) = stateProg.run(state)
    log.foreach(println)
    val extraMsg = result match {
      case Right(cost) =>
        val entryTime = state.carMap.getOrElse(carNumber, 0.0)
        val hours = config.roundUp(newState.currentHour - entryTime).toDouble
        val freeSpaces = config.totalSpaces - newState.occupiedSpaces.size
        s"Машина $carNumber выехала в момент ${newState.currentHour}. Стоял $hours ч., стоимость $cost руб. Свободно мест: $freeSpaces"
      case Left(err) => s"Ошибка: $err"
    }
    printLine(extraMsg) *> loop(newState, config, logAcc ++ log ++ Vector(extraMsg))
  }

  def handleLost(carNumber: String, state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] = {
    val prog = reportLostTicket(carNumber)
    val (log, stateProg) = prog.run(config).run
    val (newState, result) = stateProg.run(state)
    log.foreach(println)
    val extraMsg = result match {
      case Right(penalty) =>
        val freeSpaces = config.totalSpaces - newState.occupiedSpaces.size
        s"Потерян билет для $carNumber. Штраф $penalty руб. Свободно мест: $freeSpaces"
      case Left(err) => s"Ошибка: $err"
    }
    printLine(extraMsg) *> loop(newState, config, logAcc ++ log ++ Vector(extraMsg))
  }

  def handleNext(state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] =
    readPositiveDouble("На сколько часов увеличить время? (например 1.5):").flatMap { delta =>
      val prog = nextHour(delta)
      val (log, stateProg) = prog.run(config).run
      val (newState, result) = stateProg.run(state)
      log.foreach(println)
      val extraMsg = result match {
        case Right(_) => s"Время изменено на +$delta. Текущее время: ${newState.currentHour}"
        case Left(err) => s"Ошибка: $err"
      }
      printLine(extraMsg) *> loop(newState, config, logAcc ++ log ++ Vector(extraMsg))
    }

  val commandMap: Map[String, Command] = Map(
    "enter" -> ((cn, s, c, l) => handleEnter(cn, s, c, l)),
    "exit" -> ((cn, s, c, l) => handleExit(cn, s, c, l)),
    "lost" -> ((cn, s, c, l) => handleLost(cn, s, c, l)),
    "next" -> ((_, s, c, l) => handleNext(s, c, l)),
    "quit" -> ((_, s, c, l) => writeLogToFile(l) *> printLine("Логи сохранены в parking.log"))
  )

  def unknownCommand(state: ParkState, config: Config, logAcc: Vector[String]): Command = (_, s, c, l) =>
    printLine("Неизвестная команда") *> loop(s, c, l)

  def getCommand(action: String, state: ParkState, config: Config, logAcc: Log): Command =
    commandMap.getOrElse(action, unknownCommand(state, config, logAcc))

  def loop(state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] =
    for {
      carNumber <- readCarNumber
      commandsStr = commandMap.keys.toList.sorted.mkString("/")
      _ <- printLine(s"Действие ($commandsStr):")
      action <- readLine.map(_.toLowerCase)
      cmd = getCommand(action, state, config, logAcc)
      _ <- cmd(carNumber, state, config, logAcc)
    } yield ()

  def run(): IO[Unit] = {
    val config = Config(3, 50.0, 200.0, (d: Double) => math.ceil(d).toLong)
    val initialState = ParkState(Set.empty, Map.empty, Map.empty, 0.0, 0.0)
    loop(initialState, config, Vector.empty)
  }

  @main
  def main(): Unit = run().runNow()
}


