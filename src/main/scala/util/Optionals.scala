package util

// overall module
object Optionals:

  // type "public" definition, exposing structure
  enum Optional[A]:
    case Just(a: A)
    case Empty() // here parens are needed because of genericity
  // operations (/algorithms)

  import Optional.*
  extension [A](opt: Optional[A])
    def isEmpty: Boolean = opt match
      case Empty() => true
      case _ => false

    def isNotEmpty: Boolean = !isEmpty

    def orElse[B >: A](orElse: B): B = opt match
      case Just(a) => a
      case _ => orElse

    def flatMap[B](f: A => Optional[B]): Optional[B] = opt match
      case Just(a) => f(a)
      case _ => Empty()

    def map[B](f: A => B): Optional[B] = opt.flatMap(a => Just(f(a)))

    def filter(f: A => Boolean): Optional[A] = opt.flatMap:
      case a if f(a) => Just(a)
      case _ => Empty()


@main def tryOptionals =
  import Optionals.* // to work with Optionals (to see Optional type)
  import Optional.* // to directly access algorithms

  val s1: Optional[Int] = Just(1)
  val s2: Optional[Int] = Empty()

  println(s1) // Some(1)
  println(s1.isEmpty) // false
  println(s1.orElse(0)) // 1
  println(s2.orElse(0)) // 0
  println(s1.map(i => "val: " + 1)) // Some("val: 1")
  println(s2.map(i => "val: " + 1)) // None
