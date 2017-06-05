package definiti.native

sealed trait Validation[+A] {
  def isValid: Boolean

  def map[B](f: A => B): Validation[B]
}

case class Valid[+A](value: A) extends Validation[A] {
  override def isValid: Boolean = true

  override def map[B](f: A => B): Validation[B] = Valid(f(value))
}

case class Invalid(errors: Seq[String]) extends Validation[Nothing] {
  override def isValid: Boolean = false

  override def map[B](f: (Nothing) => B): Validation[B] = Invalid(errors)
}

object Invalid {
  def apply(errors: String*)(implicit dummyImplicit: DummyImplicit): Invalid = new Invalid(errors)
}

case class Verifications[-A](verifications: Seq[Verification[A]]) {
  def verify[B <: A](value: B): Validation[B] = {
    val validations = verifications.map(_.verify(value))
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      val allErrors = validations.collect { case Invalid(errors) => errors }
      Invalid(allErrors.flatten)
    }
  }
}

object Verifications {
  def apply[A](verifications: Verification[A]*)(implicit dummyImplicit: DummyImplicit): Verifications[A] = new Verifications(verifications)
}

case class Verification[-A](verification: A => Boolean, message: String) {
  def verify[B <: A](value: B): Validation[B] = {
    if (verification(value)) {
      Valid(value)
    } else {
      Invalid(message)
    }
  }
}

object Verification {
  def apply[A](message: String)(verification: A => Boolean): Verification[A] = new Verification(verification, message)
}