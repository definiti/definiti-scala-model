package definiti.native

object ListExtension {
  @inline def nonEmpty[A](list: List[A]): Boolean = list.nonEmpty

  @inline def isEmpty[A](list: List[A]): Boolean = list.isEmpty

  @inline def head[A](list: List[A]): A = list.head

  @inline def forall[A](list: List[A], f: A => Boolean): Boolean = list.forall(f)

  @inline def exists[A](list: List[A], f: A => Boolean): Boolean = list.exists(f)

  @inline def foldLeft[A, B](list: List[A], startValue: B, f: (B, A) => B): B = list.foldLeft(startValue)(f)
}
