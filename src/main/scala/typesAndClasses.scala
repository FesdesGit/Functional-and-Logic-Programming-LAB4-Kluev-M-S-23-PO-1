type Log = Vector[String]
type Command = (String, ParkState, Config, Log) => IO[Unit]
type ParkingProgram[A] = Reader[Config, Writer[Log, State[ParkState, A]]]

case class Config(
  totalSpaces: Int,
  ratePerHour: Double,
  lostTicketPenalty: Double,
  roundUp: Double => Long
)

case class ParkState(
  occupiedSpaces: Set[Int],           // номера занятых мест (1..totalSpaces)
  carToSpace: Map[String, Int],       // машина -> номер места
  carMap: Map[String, Double],        // машина -> время въезда
  currentHour: Double,                // текущий час (используется при выезде)
  revenue: Double
)