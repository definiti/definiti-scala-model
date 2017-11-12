package definiti.native

sealed trait Verification[A] {
  def verify[B <: A](value: B): Validation[B]

  def andThen(message: String)(check: A => Boolean): Verification[A]

  def andThen(verification: Verification[A]): Verification[A]

  def andBefore(beforeVerification: Verification[A]): Verification[A]

  def withMessage(message: String): Verification[A]

  def from[B](f: B => A): Verification[B]
}

object Verification {
  def apply[A](): Verification[A] = new NoVerification[A]

  def apply[A](message: String)(check: A => Boolean): Verification[A] = {
    new SomeVerification(new NoVerification[A], check, message)
  }

  def traverse[A](verifications: Verification[A]*)(implicit dummyImplicit: DummyImplicit): Verification[A] = {
    traverse(verifications)
  }
  def traverse[A](verifications: Seq[Verification[A]]): Verification[A] = {
    verifications.toList match {
      case Nil => new NoVerification[A]
      case head :: tail => head.andThen(traverse(tail))
    }
  }
}

final class NoVerification[A] extends Verification[A] {
  override def verify[B <: A](value: B): Validation[B] = Valid(value)

  override def andThen(message: String)(check: A => Boolean): Verification[A] = new SomeVerification(this, check, message)

  override def andThen(verification: Verification[A]): Verification[A] = verification

  override def andBefore(beforeVerification: Verification[A]): Verification[A] = beforeVerification

  override def withMessage(message: String): Verification[A] = this

  override def from[B](f: B => A): Verification[B] = new NoVerification[B]
}

final class SomeVerification[A](inner: Verification[A], check: A => Boolean, message: String) extends Verification[A] {
  override def verify[B <: A](value: B): Validation[B] = {
    inner.verify(value) match {
      case Invalid(errors) => Invalid(errors)
      case Valid(validValue) =>
        if (check(validValue)) {
          Valid(validValue)
        } else {
          Invalid(message)
        }
    }
  }

  override def andThen(message: String)(check: A => Boolean): Verification[A] = {
    new SomeVerification(this, check, message)
  }

  override def andThen(verification: Verification[A]): Verification[A] = verification match {
    case _: NoVerification[A] => this
    case target: SomeVerification[A] => verification.andBefore(this)
  }

  override def andBefore(beforeVerification: Verification[A]): Verification[A] = {
    new SomeVerification(inner.andBefore(beforeVerification), check, message)
  }

  override def withMessage(message: String): Verification[A] = new SomeVerification(inner, check, message)

  override def from[B](f: B => A): Verification[B] = {
    new SomeVerification[B](inner.from(f), f.andThen(check), message)
  }
}