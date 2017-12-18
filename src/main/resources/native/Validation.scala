package definiti.native

sealed trait Validation[+A] {
  def isValid: Boolean

  def map[B](f: A => B): Validation[B]

  def flatMap[B](f: A => Validation[B]): Validation[B]

  def andThen[B](f: => Validation[B]): Validation[B]
}

case class Valid[+A](value: A) extends Validation[A] {
  override def isValid: Boolean = true

  override def map[B](f: A => B): Validation[B] = Valid(f(value))

  override def flatMap[B](f: A => Validation[B]): Validation[B] = f(value)

  override def andThen[B](f: => Validation[B]): Validation[B] = f
}

case class Invalid(errors: Seq[String]) extends Validation[Nothing] {
  override def isValid: Boolean = false

  override def map[B](f: (Nothing) => B): Validation[B] = Invalid(errors)

  override def flatMap[B](f: Nothing => Validation[B]): Validation[Nothing] = Invalid(errors)

  override def andThen[B](f: => Validation[B]): Validation[B] = Invalid(errors)
}

object Invalid {
  def apply(errors: String*)(implicit dummyImplicit: DummyImplicit): Invalid = new Invalid(errors)
}