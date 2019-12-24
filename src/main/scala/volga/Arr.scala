package volga
import cats.arrow.{Arrow, ArrowChoice}

trait ArrLike[->[_, _]]

object ArrLike extends ArrInstanceChain

trait Arr[->[_, _]] extends Cat[->] with ArrLike[->] 
  def lift[A, B](f: A => B): A -> B

  def [A, B, C, D](f: A -> C) split (g: B -> D): (A, B) -> (C, D)
  final def [A, B, C, D](f: A -> C) *** (g: B -> D): (A, B) -> (C, D) = f split g

  def proj1[A, B]: (A, B) -> A = lift(_._1)
  def proj2[A, B]: (A, B) -> B = lift(_._2)

  def [A, B, C](f: A -> B) product (g: A -> C): A -> (B, C) = (f split g) compose lift(a => (a, a))
  final def [A, B, C](f: A -> B) &&& (g: A -> C): A -> (B, C) = f product g

  def term[A]: A -> Unit = lift(_ => ())
  def id[A]: A -> A      = lift(identity)

  def [A, B, C](a1: A -> B) rmap(f: B => C): A -> C = lift(f) compose a1
  def [A, B, C](a1: A -> B) lmap(f: C => A): C -> B = a1 compose  lift(f)

  def first[A, B, C](f: A -> B): (A, C) -> (B, C)  = f split id
  def second[A, B, C](f: A -> B): (C, A) -> (C, B) = id split f

  def [A, B1, B2, B3](a1: A -> B1) product3 (a2: A -> B2, a3: A -> B3): A -> (B1, B2, B3) =
    a1 &&& a2 &&& a3 rmap { case ((b1, b2), b3) => (b1, b2, b3) }
    
  def [A, B1, B2, B3, B4](a1: A -> B1) product4 (a2: A -> B2, a3: A -> B3, a4: A -> B4): A -> (B1, B2, B3, B4) =
    a1 &&& a2 &&& (a3 product a4) rmap { case ((b1, b2), (b3, b4)) => (b1, b2, b3, b4) }
    
  def [A, B1, B2, B3, B4, B5](a1: A -> B1) product5(
      a2: A -> B2,
      a3: A -> B3,
      a4: A -> B4,
      a5: A -> B5
  ): A -> (B1, B2, B3, B4, B5) =
    a1 &&& a2 &&& (a3 &&& a4 &&& a5) rmap {case ((b1, b2), ((b3, b4), b5)) => (b1, b2, b3, b4, b5)}

  def [A, B1, B2, C](a1: A -> B1) mergeMap2 (a2: A -> B2)(f: (B1, B2) => C) =
    a1 &&& a2 rmap { case (b1, b2) => f(b1, b2) }

  def [A, B1, B2, B3, C](a1: A -> B1) mergeMap3 (a2: A -> B2, a3: A -> B3)(f: (B1, B2, B3) => C) =
    a1 &&& a2 &&& a3 rmap { case ((b1, b2), b3) => f(b1, b2, b3) }

  def [A, B1, B2, B3, B4, C](a1: A -> B1) mergeMap4 (a2: A -> B2, a3: A -> B3, a4: A -> B4)(f: (B1, B2, B3, B4) => C) =
    a1 &&& a2 &&& (a3 &&& a4) rmap { case ((b1, b2), (b3, b4)) => f(b1, b2, b3, b4) }

  def [A, B1, B2, B3, B4, B5, C](a1: A -> B1) mergeMap5 (a2: A -> B2, a3: A -> B3, a4: A -> B4, a5: A -> B5)(
      f: (B1, B2, B3, B4, B5) => C
  ) = a1 &&& a2 &&& (a3 &&& a4 &&& a5) rmap {case ((b1, b2), ((b3, b4), b5)) => f(b1, b2, b3, b4, b5)}


object Arr extends ArrInstanceChain

trait ArrChoice[->[_, _]] extends Arr[->] 
  def[A, B, C, D](f: A -> C) choose (g: B -> D): Either[A, B] -> Either[C, D]
  final def[A, B, C, D](f: A -> C) +++ (g: B -> D): Either[A, B] -> Either[C, D] = f choose g

  def [A, B, C](f: A -> C) choice (g: B -> C): Either[A, B] -> C = (f choose g) >>> lift(_.merge)
  final def [A, B, C](f: A -> C) ||| (g: B -> C): Either[A, B] -> C = f choice g

  def [A, B, C](fab: A -> B) left: Either[A, C] -> Either[B, C] = fab choose lift(identity)

  def [A, B, C](fab: A -> B) right: Either[C, A] -> Either[C, B] = lift(identity[C]) choose fab


object ArrChoice extends ArrChoiceInstanceChain

trait ArrPlus[->[_, _]] extends Arr[->] 
  def [A, B](f: A -> B) plus (g: A -> B): A -> B
  final def [A, B](f: A -> B) >+< (g: A -> B): A -> B = f plus g


trait ArrApply[->[_, _]] extends ArrChoice[->] 
  def app[A, B]: (A, A -> B) -> B

  override def[A, B, C, D](f: A -> C) choose(g: B -> D): Either[A, B] -> Either[C, D] =
    app <<< lift {
      case Left(a)  => ((), f lmap(_ => a) rmap (Left(_)))
      case Right(b) => ((), g lmap(_ => b) rmap (Right(_)))
    }


trait ArrInstanceChain 
  final given [->[_, _]]: (arr: Arrow[->]) => Arr[->]
      def lift[A, B](f: A => B): A -> B                               = arr.lift(f)
      def [A, B, C, D](f: A -> C) split (g: B -> D): (A, B) -> (C, D) = arr.split(f, g)
      def [A, B, C](f: B -> C) compose (g: A -> B): A -> C            = arr.compose(f, g)

trait ArrChoiceInstanceChain 
  final given arrowChoiceFromCats[->[_, _]]: (arr: ArrowChoice[->]) => ArrChoice[->] 
      def lift[A, B](f: A => B): A -> B                                            = arr.lift(f)
      def [A, B, C, D](f: A -> C) split (g: B -> D): (A, B) -> (C, D)              = arr.split(f, g)
      def [A, B, C](f: B -> C) compose (g: A -> B): A -> C                         = arr.compose(f, g)
      def [A, B, C, D](f: A -> C) choose (g: B -> D): Either[A, B] -> Either[C, D] = arr.choose(f)(g)
    

