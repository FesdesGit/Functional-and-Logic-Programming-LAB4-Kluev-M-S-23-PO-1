def updateSpace(state: ParkState, space: Int, occupied: Set[Int], carNumber: String): (ParkState, Either[String, Int]) =
  val newState = state.copy(
    occupiedSpaces = occupied + space,
    carToSpace = state.carToSpace + (carNumber -> space),
    carMap = state.carMap + (carNumber -> state.currentHour)
  )
  (newState, Right(space))

def enterCar(carNumber: String): ParkingProgram[Either[String, Int]] =
  val log = Vector(s"Въезд: $carNumber")
  def valueFromConfig(config: Config) = State[ParkState, Either[String, Int]] { state =>
    val occupied = state.occupiedSpaces
    val allSpaces = (1 to config.totalSpaces).toSet
    val freeSpaces = allSpaces diff occupied
    freeSpaces.headOption match {
      case Some(space) => updateSpace(state, space, occupied, carNumber)
      case None => (state, Left(s"Нет свободных мест (всего ${config.totalSpaces})"))
    }
  }
  Reader { config =>
    Writer(log, valueFromConfig(config))
  }

def updateExit(state: ParkState, entryTime: Double, config: Config, carNumber: String): (ParkState, Either[String, Double]) =
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

def exitCar(carNumber: String): ParkingProgram[Either[String, Double]] =
  val log = Vector(s"Выезд: $carNumber")
  def valueFromConfig(config: Config) = State[ParkState, Either[String, Double]] { state =>
    state.carMap.get(carNumber) match {
      case Some(entryTime) => updateExit(state, entryTime, config, carNumber)
      case None => (state, Left(s"Машина $carNumber не найдена на парковке"))
    }
  }
  Reader { config =>
    Writer(log, valueFromConfig(config))
  }

def updateLost(state: ParkState, config: Config, carNumber: String): (ParkState, Either[String, Double]) =
  val space = state.carToSpace(carNumber)
  val penalty = config.lostTicketPenalty
  val newState = state.copy(
    occupiedSpaces = state.occupiedSpaces - space,
    carToSpace = state.carToSpace - carNumber,
    carMap = state.carMap - carNumber,
    revenue = state.revenue + penalty
  )
  (newState, Right(penalty))

def reportLostTicket(carNumber: String): ParkingProgram[Either[String, Double]] =
  def valueFromConfig(config: Config) = State[ParkState, Either[String, Double]] { state =>
    state.carMap.get(carNumber) match {
      case Some(_) => updateLost(state, config, carNumber)
      case None => (state, Left(s"Машина $carNumber не найдена"))
    }
  }
  Reader { config =>
    Writer(Vector(s"Потерян билет для $carNumber, штраф ${config.lostTicketPenalty}"), valueFromConfig(config))
  }

def nextHour(delta: Double): ParkingProgram[Either[String, Unit]] =
  val log = Vector(s"Переключение времени на +$delta")
  def valueState = State[ParkState, Either[String, Unit]] { state =>
    if (delta > 0) {
      val newState = state.copy(currentHour = state.currentHour + delta)
      (newState, Right(()))
    } else {
      (state, Left("Время должно быть положительным"))
    }
  }
  Reader { config =>
    Writer(log, valueState)
  }