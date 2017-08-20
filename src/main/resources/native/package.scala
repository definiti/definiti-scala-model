package definiti

import java.util.Date

package object native {
  trait Tagged[+V, +T]
  type @@[+V, +T] = V with Tagged[V, T]

  implicit def orderedDateExtension(date: Date): OrderedDateExtension = new OrderedDateExtension(date)
}
