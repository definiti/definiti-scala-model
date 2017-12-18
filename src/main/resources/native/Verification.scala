package definiti.native

sealed trait Verification[A] {
  final def verify[B <: A](value: B): Validation[B] = validate("", value)

  private[native] def validate[B <: A](path: String, value: B): Validation[B]

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

  def from[B](f: B => A, path: String): Verification[B] = {
    new VerificationMap[A, B](this, f, path)
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
    new VerificationGroup(verifications)
  }
}

final class VerificationGroup[A](verifications: Seq[Verification[A]]) extends Verification[A] {
  override private[native] def validate[B <: A](path: String, value: B) = {
    val validations = verifications.map(_.validate(path, value))
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      Validation.squashErrors(validations)
    }
  }
}

final class VerificationMap[A, C](verification: Verification[A], map: C => A, subPath: String) extends Verification[C] {
  override private[native] def validate[B <: C](path: String, value: B): Validation[B] = {
    val innerPath = if (path.nonEmpty) path + "." + subPath else subPath
    verification.validate(innerPath, map(value)) match {
      case Valid(_) => Valid(value)
      case Invalid(errors) => Invalid(errors)
    }
  }
}

final class NoVerification[A] extends Verification[A] {
  override private[native] def validate[B <: A](path: String, value: B): Validation[B] = Valid(value)
}

final class ValueVerification[A](check: A => Boolean, message: String) extends Verification[A] {
  override private[native] def validate[B <: A](path: String, value: B): Validation[B] = {
    if (check(value)) {
      Valid(value)
    } else {
      Invalid(Error(path, message))
    }
  }

  override def withMessage(message: String) = new ValueVerification(check, message)
}

final class ListVerification[A](verification: Verification[A] = Verification[A]()) extends Verification[List[A]] {
  override private[native] def validate[B <: List[A]](path: String, value: B) = {
    val validations = value.zipWithIndex.map {
      case (current, index) => verification.validate(path + s"[${index}]", current)
    }
    if (validations.forall(_.isValid)) {
      Valid(value)
    } else {
      Validation.squashErrors(validations)
    }
  }
}

final class OptionVerification[A](verification: Verification[A] = Verification[A]()) extends Verification[Option[A]] {
  override private[native] def validate[B <: Option[A]](path: String, value: B) = {
    value
      .map(verification.validate(path, _))
      .map {
        case Valid(_) => Valid(value)
        case Invalid(errors) => Invalid(errors)
      }
      .getOrElse(Valid(value))
  }
}