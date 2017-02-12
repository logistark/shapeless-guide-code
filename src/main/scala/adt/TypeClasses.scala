package adt

import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}


/**
  * @author @Logistark
  */

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc
  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
        def encode(value: A) = func(value)
    }

  implicit val stringEncoder: CsvEncoder[String] =
    pure(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    pure(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    pure(bool => List(if(bool) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    pure(hnil => Nil)

  implicit val doubleEncoder: CsvEncoder[Double] =
    pure(d => List(d.toString))

  implicit val floatEncoder: CsvEncoder[Float] =
    pure(f => List(f.toString))

  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[CsvEncoder[H]],
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    pure {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  implicit val cnilEncoder: CsvEncoder[CNil] =
    pure(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    hEncoder: Lazy[CsvEncoder[H]],
    tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = pure {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A,R],
    rEncoder: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = pure(a => rEncoder.value.encode(gen.to(a)))
}

  object Main {


    case class Employee(name: String, number: Int, manager: Boolean)

    case class IceCream(name: String, numCherries: Int, inCone: Boolean)

    sealed trait Shape

    final case class Rectange(width: Double, height: Double) extends Shape

    final case class Circle(radius: Double) extends Shape

    sealed trait Tree[A]

    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    case class Leaf[A](value: A) extends Tree[A]


    val shapes: List[Shape] = List(
      Rectange(3.0, 4.0),
      Circle(1.0)
    )
  }
