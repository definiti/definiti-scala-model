package definiti.native

sealed trait Verification[A] {
  def verify[B <: A](value: B): Validation[B]

  def andThen(message: String)(check: A => Boolean): Verification[A] = {
    new VerificationGroup[A](Seq(this, new ValueVerification[A](check, message)))
  }

  def andThen(verification: Verification[A]): Verification[A] = {
    new VerificationGroup[A](Seq(this, verification))
  }

  def andBefore(beforeVerification: Verification[A]): Verification[A] = {
    new VerificationGroup[A](Seq(beforeVerification, this))
  }

  def withMessage(message: String): Verification[A] = this

  def from[B](f: B => A): Verification[B] = {
    new VerificationMap[A, B](this, f)
  }
}

object Verification {
  def apply[A](): Verification[A] = new NoVerification[A]

  def apply[A](message: String)(check: A => Boolean): Verification[A] = {
    new ValueVerification(check, message)
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

final class VerificationGroup[A](verifications: Seq[Verification[A]]) extends Verification[A] {
  override def verify[B <: A](value: B) = {
    val validations = verifications.map(_.verify(value))
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      Invalid {
        validations.collect {
          case Invalid(errors) => errors
        }.flatten
      }
    }
  }
}

final class VerificationMap[A, C](verification: Verification[A], map: C => A) extends Verification[C] {
  override def verify[B <: C](value: B): Validation[B] = {
    verification.verify(map(value)) match {
      case Valid(_) => Valid(value)
      case Invalid(errors) => Invalid(errors)
    }
  }
}

final class NoVerification[A] extends Verification[A] {
  override def verify[B <: A](value: B): Validation[B] = Valid(value)
}

final class ValueVerification[A](check: A => Boolean, message: String) extends Verification[A] {
  override def verify[B <: A](value: B): Validation[B] = {
    if (check(value)) {
      Valid(value)
    } else {
      Invalid(message)
    }
  }

  override def withMessage(message: String) = new ValueVerification(check, message)
}

final class ListVerification[A](verification: Verification[A] = Verification[A]()) extends Verification[List[A]] {
  override def verify[B <: List[A]](value: B) = {
    val validations = value.map(verification.verify)
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      Invalid {
        validations.collect {
          case Invalid(errors) => errors
        }.flatten
      }
    }
  }
}

final class OptionVerification[A](verification: Verification[A] = Verification[A]()) extends Verification[Option[A]] {
  override def verify[B <: Option[A]](value: B) = {
    value
      .map {
        case Valid(_) => Valid(value)
        case Invalid(errors) => Invalid(errors)
      }
      .getOrElse(Valid(value))
  }
}