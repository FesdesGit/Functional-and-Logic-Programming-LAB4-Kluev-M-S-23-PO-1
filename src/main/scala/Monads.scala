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