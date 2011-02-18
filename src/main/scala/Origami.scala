package origami

object Origami {

  case class Fix[F[_, _], A](out: F[A, Fix[F, A]])

  trait BiFunctor[F[_, _]] {
    def bimap[A, B, C, D]: (A => B) => (C => D) => F[A, C] => F[B, D]
    def fmap2[A, B, C]: (B => C) => F[A, B] => F[A, C] = bimap(identity[A])
  }

  def cata[A, B, F[_, _]](f: F[A, B] => B)(t: Fix[F, A])(implicit ft: BiFunctor[F]): B =
    f(ft.fmap2(cata[A, B, F](f))(t.out))

  def ana[A, B, F[_, _]](f: B => F[A, B])(x: B)(implicit ft: BiFunctor[F]): Fix[F, A] =
    Fix[F, A](ft.fmap2(ana[A, B, F](f))(f(x)))

  def hylo[A, B, C, F[_, _]](f: A => F[C, A])(g: F[C, B] => B)(x: A)(implicit ft: BiFunctor[F]): B =
    g(ft.fmap2(hylo[A, B, C, F](f)(g))(f(x)))

  def build[A, F[_, _]](f: { def apply[B]: (F[A, B] => B) => B }) =
    f.apply(Fix[F, A])
}

object Shapes {
  import Origami._

  trait ListF[A, B]
  case class Nil[A, B]() extends ListF[A, B]
  case class Cons[A, B](hd: A, tl: B) extends ListF[A, B]

  // typeclass implementation for ListF
  implicit object biList extends BiFunctor[ListF] {
    def bimap[A, B, C, D] = f => g => {
      case Nil() => Nil()
      case Cons(x, xs) => Cons(f(x), g(xs))
    }
  }

  type List[A] = Fix[ListF, A]
  def nil[A]: List[A] = Fix[ListF, A](Nil())
  def cons[A] = (x: A) =>
    (xs: List[A]) =>
      Fix[ListF, A](Cons(x, xs))

  trait BtreeF[A, B]
  case class Tip[A, B](data: A) extends BtreeF[A, B]
  case class Bin[A, B](left: B, right: B) extends BtreeF[A, B]

  // typeclass implementation for BtreeF
  implicit object biBtree extends BiFunctor[BtreeF] {
    def bimap[A, B, C, D] = f => g => {
      case Tip(d) => Tip(f(d))
      case Bin(l, r) => Bin(g(l), g(r))
    }
  }

  type Btree[A] = Fix[BtreeF, A]
  def tip[A] = (d: A) => Fix[BtreeF, A](Tip(d))
  def bin[A] = (l: Btree[A]) => (r: Btree[A]) => Fix[BtreeF, A](Bin(l, r))
}
