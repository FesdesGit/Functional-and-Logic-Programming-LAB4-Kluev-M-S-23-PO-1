import scala.io.StdIn
import java.io.PrintWriter

object ParkingApp {
  def printLine(msg: String): IO[Unit] = IO(() => println(msg))
  def readLine: IO[String] = IO(() => StdIn.readLine())

  def readCarNumber: IO[String] = {
    def loop: IO[String] = for {
      _ <- printLine("Введите номер машины:")
      input <- readLine
      trimmed = input.trim
      _ <- if (trimmed.isEmpty) printLine("Номер не может быть пустым!").flatMap(_ => loop)
      else if (Set("enter", "exit", "lost", "next", "quit").contains(trimmed.toLowerCase))
        printLine("Номер не может совпадать с названием команды!").flatMap(_ => loop)
      else IO.pure(())
    } yield trimmed
    loop
  }

  def readPositiveDouble(prompt: String): IO[Double] = {
    def loop: IO[Double] = for {
      _ <- printLine(prompt)
      line <- readLine
      value <- try {
        val d = line.toDouble
        if (d <= 0) printLine("Значение должно быть больше нуля!").flatMap(_ => loop)
        else IO.pure(d)
      } catch {
        case _: NumberFormatException => printLine("Ошибка: введите число!").flatMap(_ => loop)
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
  def msgAfterEnter(config: Config, carNumber: String, newState: ParkState, state: ParkState, space: Int):String = {
    val freeSpaces = config.totalSpaces - newState.occupiedSpaces.size
    s"Машина $carNumber заехала на место $space в момент ${state.currentHour}. Свободно мест: $freeSpaces"
  }

  def handleEnter(carNumber: String, state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] = {
    val prog = enterCar(carNumber)
    val (log, stateProg) = prog.run(config).run
    val (newState, result) = stateProg.run(state)
    log.foreach(println)
    val extraMsg = result match {
      case Right(space) => msgAfterEnter(config, carNumber, newState, state, space)
      case Left(err) => s"Ошибка: $err"
    }
    printLine(extraMsg).flatMap(_ => loop(newState, config, logAcc ++ log ++ Vector(extraMsg)))
  }

  def msgAfterExit(state: ParkState, config: Config, carNumber: String, newState: ParkState, cost: Double): String = {
    val entryTime = state.carMap.getOrElse(carNumber, 0.0)
    val hours = config.roundUp(newState.currentHour - entryTime).toDouble
    val freeSpaces = config.totalSpaces - newState.occupiedSpaces.size
    s"Машина $carNumber выехала в момент ${newState.currentHour}. Стоял $hours ч., стоимость $cost руб. Свободно мест: $freeSpaces"
  }

  def handleExit(carNumber: String, state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] = {
    val prog = exitCar(carNumber)
    val (log, stateProg) = prog.run(config).run
    val (newState, result) = stateProg.run(state)
    log.foreach(println)
    val extraMsg = result match {
      case Right(cost) => msgAfterExit(state, config, carNumber, newState, cost)
      case Left(err) => s"Ошибка: $err"
    }
    printLine(extraMsg).flatMap(_ => loop(newState, config, logAcc ++ log ++ Vector(extraMsg)))
  }

  def msgAfterLost(config: Config, carNumber: String, newState: ParkState, penalty: Double):String = {
    val freeSpaces = config.totalSpaces - newState.occupiedSpaces.size
    s"Потерян билет для $carNumber. Штраф $penalty руб. Свободно мест: $freeSpaces"
  }

  def handleLost(carNumber: String, state: ParkState, config: Config, logAcc: Vector[String]): IO[Unit] = {
    val prog = reportLostTicket(carNumber)
    val (log, stateProg) = prog.run(config).run
    val (newState, result) = stateProg.run(state)
    log.foreach(println)
    val extraMsg = result match {
      case Right(penalty) => msgAfterLost(config, carNumber, newState, penalty)
      case Left(err) => s"Ошибка: $err"
    }
    printLine(extraMsg).flatMap(_ => loop(newState, config, logAcc ++ log ++ Vector(extraMsg)))
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
      printLine(extraMsg).flatMap(_ => loop(newState, config, logAcc ++ log ++ Vector(extraMsg)))
    }

  val commandMap: Map[String, Command] = Map(
    "enter" -> ((cn, s, c, l) => handleEnter(cn, s, c, l)),
    "exit" -> ((cn, s, c, l) => handleExit(cn, s, c, l)),
    "lost" -> ((cn, s, c, l) => handleLost(cn, s, c, l)),
    "next" -> ((_, s, c, l) => handleNext(s, c, l)),
    "quit" -> ((_, s, c, l) => writeLogToFile(l).flatMap(_ => printLine("Логи сохранены в parking.log")))
  )

  def unknownCommand(state: ParkState, config: Config, logAcc: Vector[String]): Command = (_, s, c, l) =>
    printLine("Неизвестная команда").flatMap(_ => loop(s, c, l))

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


