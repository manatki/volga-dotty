package volga

trait SemCatLike[->[_, _], x[_, _]]

trait SemigropalCat[->[_, _], x[_, _]] extends Cat[->] with SemCatLike[->, x] 
  def assocl[A, B, C]: (A x (B x C)) -> ((A x B) x C)
  def assocr[A, B, C]: ((A x B) x C) -> (A x (B x C))

  def [A, B, C, D](f: A -> B) tensor (g: C -> D): (A x C) -> (B x D)
  def [A, B, C, D](f: A -> B) x (g: C -> D): (A x C) -> (B x D) = f tensor g


trait MonCatLike[->[_, _], x[_, _], I] extends SemCatLike[->, x]

trait MonoidalCat[->[_, _], x[_, _], I] extends SemigropalCat[->, x] with MonCatLike[->, x, I] 
  def lunit[A]: (I x A) -> A
  def unitl[A]: A -> (I x A)
  def runit[A]: (A x I) -> A
  def unitr[A]: A -> (A x I)


trait Sym[->[_, _], x[_, _]] extends SemigropalCat[->, x] 
  def swap[A, B]: (A x B) -> (B x A)

  override def assocr[A, B, C] = swap >>> (id x swap) >>> assocl >>> (swap x id) >>> swap      
    

trait Symon[->[_, _], x[_, _], I] extends Sym[->, x] with MonoidalCat[->, x, I] 
  override def runit[A] = lunit <<< swap
  override def unitr[A] = unitl >>> swap


trait SemiClosed[->[_, _], x[_, _], ==>[_, _]] extends Sym[->, x] 
  def [A, B, C](p: (A x B) -> C) lcurry: A -> (B ==> C)
  def [A, B, C](p: A -> (B ==> C))luncurry: (A x B) -> C

  def [A, B, C](p: (A x B) -> C) rcurry: B -> (A ==> C)  = lcurry(p <<< swap)
  def [A, B, C](p: B -> (A ==> C))runcurry: (A x B) -> C = p.luncurry <<< swap

  def lapply[A, B]: ((A ==> B) x A) -> B = id.luncurry
  def rapply[A, B]: (A x (A ==> B)) -> B = id.runcurry

  def lunapply[A, B]: A -> (B ==> (A x B)) = id.lcurry
  def runapply[A, B]: B -> (A ==> (A x B)) = id.rcurry


trait Closed[->[_, _], x[_, _], ==>[_, _], I] extends SemiClosed[->, x, ==>] with MonoidalCat[->, x, I] 
  def ident[A]: I -> (A ==> A)  = lunit.lcurry
  def point[A]: A -> (I ==> A) = runit.lcurry

  def unpoint[A]: (I ==> A) -> A = lapply <<< unitr

  def [A, B](f: A -> B) abstraction: I -> (A ==> B) = lcurry(f <<< lunit)

  def [A, B, C](f: A -> B) precmp: (C ==> A) -> (C ==> B) = lcurry(f <<< lapply)

  def [A, B, C](f: A -> B) postcmp: (B ==> C) -> (A ==> C) = lcurry(lapply <<< (id x f))

  //bifunctoriality of closure
  def promap[A, B, C, D](f: A -> B, g: C -> D): (D ==> A) -> (C ==> B) = f.precmp <<< g.postcmp


trait Cartesian[->[_, _], x[_, _], I] extends Symon[->, x, I] 
  def proj1[A, B]: (A x B) -> A
  def proj2[A, B]: (A x B) -> B

  def [A, B, C](f: A -> B) product (g: A -> C): A -> (B x C)
  def [A, B, C](f: A -> B) <> (g: A -> C): A -> (B x C) = f product g

  def term[A]: A -> I

  override def lunit[A] = proj2
  override def unitl[A] = term <> id
  override def runit[A] = proj1
  override def unitr[A] = id <> term
  override def swap[A, B] =  proj2 <> proj1

  override def assocl[A, B, C] = (proj1 <> (proj1 <<< proj2)) <> (proj2 <<< proj2)
  override def assocr[A, B, C] = (proj1 <<< proj1) <> ((proj2 <<< proj1) <> proj2)

  override def[A, B, C, D](f: A -> B) tensor (g: C -> D) = (f <<< proj1) <> (g <<< proj2)


trait Bicartesian[->[_, _], x[_, _], I, :+[_, _], O] extends Cartesian[->, x, I] 
  def inj1[A, B]: A -> (A :+ B)
  def inj2[A, B]: B -> (A :+ B)

  def [A, B, C](f: A -> C) sum (g: B -> C): (A :+ B) -> C
  def [A, B, C](f: A -> C) ><  (g: B -> C): (A :+ B) -> C


  def choose[A, B, C, D](f: A -> C, g: B -> D): (A :+ B) -> (C :+ D) =  (f >>> inj1) >< (g >>> inj2)

  def init[A]: O -> A

  def distribr[A, B, C]: ((A x B) :+ (A x C)) -> (A x (B :+ C)) = (id x inj1) >< (id x inj2)


trait DistributiveCat[->[_, _], x[_, _], I, :+[_, _], O] extends Bicartesian[->, x, I, :+, O] 
  def distribl[A, B, C]: (A x (B :+ C)) -> ((A x B) :+ (A x C))


trait CartesianClosed[->[_, _], x[_, _], ==>[_, _], I] extends Cartesian[->, x, I] with Closed[->, x, ==>, I]

trait BicartesianClosed[->[_, _], x[_, _], ==>[_, _], I, :+[_, _], O]
    extends DistributiveCat[->, x, I, :+, O] with CartesianClosed[->, x, ==>, I] 
  def distribl[A, B, C]: (A x (B :+ C)) -> ((A x B) :+ (A x C)) =
    runcurry((lunapply >>> precmp(swap >>> inj1)) >< (lunapply >>> precmp(swap >>> inj2)))