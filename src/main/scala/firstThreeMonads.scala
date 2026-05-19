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