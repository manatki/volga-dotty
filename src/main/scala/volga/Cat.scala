package volga

import cats.arrow.Category

trait Identity[->[_, _]] 
  def id[A]: A -> A


object Identity extends CatInstanceChain

trait Cat[->[_, _]] extends Identity[->] 
    def id[A]: A -> A
    
    def [A, B, C](f: B -> C) compose (g: A -> B): A -> C
    final def [A, B, C](f: B -> C) <<< (g: A -> B): A -> C = f compose g

    def [A, B, C](f: A -> B) andThen(g: B -> C): A -> C = g compose f
    final def [A, B, C](f: A -> B) >>> (g: B -> C): A -> C = f andThen g

object Cat extends CatInstanceChain

trait CatInstanceChain
  final given [->[_, _]]: (cat: Category[->]) => Cat[->]
      def id[A]: A -> A                                    = cat.id
      def [A, B, C](f: B -> C) compose (g: A -> B): A -> C = cat.compose(f, g)
    
