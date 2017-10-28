package definiti.native

sealed trait Validation[+A] {
  def isValid: Boolean

  def map[B](f: A => B): Validation[B]

  def flatMap[B](f: A => Validation[B]): Validation[B]

  def andThen[B](f: => Validation[B]): Validation[B]
}

object Validation {
  def apply[A, B](fa: => Validation[A])(map: A => B)(verify: B => Boolean): Validation[B] = {
    fa match {
      case Valid(va) =>
        val vb = map(va)
        if (verify(vb)) {
          Valid(vb)
        } else {
          Invalid()
        }
    }
  }

  type V[A] = Validation[A]

  def all[A, B](va: V[A], vb: V[B]): V[(A, B)] = {
    (va, vb) match {
      case (Valid(a), Valid(b)) => Valid((a, b))
      case _ => Invalid(collectErrors(va, vb))
    }
  }
  def all[A, B, C](va: V[A], vb: V[B], vc: V[C]): V[(A, B, C)] = {
    (va, vb, vc) match {
      case (Valid(a), Valid(b), Valid(c)) => Valid((a, b, c))
      case _ => Invalid(collectErrors(va, vb, vc))
    }
  }
  def all[A, B, C, D](va: V[A], vb: V[B], vc: V[C], vd: V[D]): V[(A, B, C, D)] = {
    (va, vb, vc, vd) match {
      case (Valid(a), Valid(b), Valid(c), Valid(d)) => Valid((a, b, c, d))
      case _ => Invalid(collectErrors(va, vb, vc, vd))
    }
  }
  def all[A, B, C, D, E](va: V[A], vb: V[B], vc: V[C], vd: V[D], ve: V[E]): V[(A, B, C, D, E)] = {
    (va, vb, vc, vd, ve) match {
      case (Valid(a), Valid(b), Valid(c), Valid(d), Valid(e)) => Valid((a, b, c, d, e))
      case _ => Invalid(collectErrors(va, vb, vc, vd, ve))
    }
  }

  def collectErrors(validations: Validation[_]*): Seq[String] = {
    validations
      .collect { case Invalid(errors) => errors }
      .flatten
  }
}

case class Valid[+A](value: A) extends Validation[A] {
  override def isValid: Boolean = true

  override def map[B](f: A => B): Validation[B] = Valid(f(value))

  override def flatMap[B](f: A => Validation[B]) = f(value)

  override def andThen[B](f: => Validation[B]): Validation[B] = f
}

case class Invalid(errors: Seq[String]) extends Validation[Nothing] {
  override def isValid: Boolean = false

  override def map[B](f: (Nothing) => B): Validation[B] = Invalid(errors)

  override def flatMap[B](f: Nothing => Validation[B]) = Invalid(errors)

  override def andThen[B](f: => Validation[B]): Validation[B] = Invalid(errors)
}

object Invalid {
  def apply(errors: String*)(implicit dummyImplicit: DummyImplicit): Invalid = new Invalid(errors)
}