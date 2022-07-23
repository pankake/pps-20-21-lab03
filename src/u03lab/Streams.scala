package u03lab

import u03.Streams.Stream
import u03.Streams.Stream._

import scala.annotation.tailrec

object Streams {

  object Stream {
    @tailrec
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match {
      case (Cons(_, tail), n) if n > 0 => drop(tail())(n - 1)
      case (stream, 0) => stream
      case (Empty(), _) => Empty()
    }

    def constant[A](element: A): Stream[A] = iterate(element)(x => x)

    def fib(): Stream[Int] = {
      def _fib(prev: Int, curr: Int): Stream[Int] = cons(prev, _fib(curr, prev+curr))
      _fib(0, 1)
    }
  }
}