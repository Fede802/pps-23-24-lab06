package ex1


// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()

  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h) // pattern for scala.Option
    case _ => None // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] = map((_, value))

  def length(): Int = foldLeft(0)((a, _) => a + 1)

  def zipWithIndex: List[(A, Int)] = foldRight(Nil[(A, Int)](), length() - 1)((l, a) => ((l, a._2) :: a._1, a._2 - 1))._1

  def partition(predicate: A => Boolean): (List[A], List[A]) =
    foldRight((Nil(), Nil()))((l, a) => if predicate(l) then (l :: a._1, a._2) else (a._1, l :: a._2))

  def span(predicate: A => Boolean): (List[A], List[A]) =
    foldLeft((Nil[A](), Nil[A]()))((a, l) => if a._2.head.isEmpty && predicate(l) then (l :: a._1, a._2) else (a._1, l :: a._2)) match
      case (l1,l2) => (l1.reverse(), l2.reverse())

  def takeRight(n: Int): List[A] = foldRight(Nil())((l, a) => if a.length() < n then l :: a else a)

  def collect(predicate: PartialFunction[A, A]): List[A] = foldRight(Nil())((l, a) => if predicate.isDefinedAt(l) then predicate(l) :: a else a)

  def reverse(): List[A] = foldLeft(Nil[A]())((a, l) => l :: a)

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)
