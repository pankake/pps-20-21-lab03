package u03lab

import u03.Lists.List._
import u03.Lists._

import scala.annotation.tailrec
import u02.Optionals._
import u02.Optionals.Option._
import u02.SumTypes._

object Lists {

  object List {

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_, t) if n > 0 => drop(t, n - 1)
      case _ => l
    }

    def flatMap[A, B](l: List[A])(mapper: A => List[B]): List[B] = l match {
      case Cons(head, tail) => append(mapper(head), flatMap(tail)(mapper))
      case _ => Nil()
    }

    def map[A,B](l: List[A])(mapper: A => B): List[B] = flatMap(l)(value => Cons(mapper(value), Nil()))

    def filter[A](l: List[A])(mapper: A => Boolean): List[A] = flatMap(l) ({
      case value if mapper(value) => Cons(value, Nil())
      case _ => Nil()
    })

    def max(l: List[Int]): Option[Int] = l match {
      case Cons(head, tail) => Some(Math.max(head, getOrElse(max(tail), Int.MinValue)))
      case _ => None()
    }

    def peopleToCourse(l: List[Person]): List[String] = flatMap(l)({
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    })

    @tailrec
    def foldLeft[A, B](l: List[A])(accumulator: B)(f: (B, A) => B): B = l match {
      case Cons(head, tail) => foldLeft(tail)(f(accumulator, head))(f)
      case _ => accumulator
    }

    def foldRight[A, B](l: List[A])(accumulator: B)(f: (A, B) => B): B = l match {
      case Cons(head, tail) => f(head, foldRight(tail)(accumulator)(f))
      case _ => accumulator
    }
  }
}