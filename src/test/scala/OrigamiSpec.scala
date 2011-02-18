package origami

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import Origami._
import Shapes._

@RunWith(classOf[JUnitRunner])
class OrigamiSpec extends Spec with ShouldMatchers {

  describe("origami with list") {
    it ("should sum a list with cata") {
      def sumList = cata[Int, Int, ListF] {
        case Nil() => 0
        case Cons(x, xs) => x + xs
      } _

      sumList(cons(1)(cons(2)(nil))) should equal(3)
      sumList(cons(1)(cons(2)(cons(3)(nil)))) should equal(6)
    }

    it ("should form a list with ana") {
      def sequence = ana[Int, Int, ListF] {i: Int => i match {
        case x if x <= 5 => Cons(x, x+1)
        case x => Nil()
      }} _

      sequence(0) should equal(cons(0)(cons(1)(cons(2)(cons(3)(cons(4)(cons(5)(nil)))))))
    }
  }

  describe("origami with btree") {
    it ("should sum a tree") {
      def sumTree = cata[Int, Int, BtreeF] {
        case Tip(d) => d
        case Bin(l, r) => l + r
      } _

      sumTree(bin(bin(tip(1))(tip(3)))(bin(tip(5))(tip(2)))) should equal(11)
      sumTree(
        bin 
          (bin 
            (bin(tip(1))(tip(3))) 
            (bin(tip(12))(tip(13))) 
          ) 
          (bin(tip(5))(tip(2)))) should equal(36) 
    }

    it ("should visit leaves from left to right") {
      import scala.collection.immutable.{List => IL}
      def combine(t: BtreeF[Int, IL[Int]]): IL[Int] = t match {
        case Tip(d) => IL(d)
        case Bin(l, r) => l ::: r
      }

      def visitTree = cata[Int, IL[Int], BtreeF] (combine) _

      visitTree(
        bin
          (bin(tip(1))(tip(3)))
          (bin(tip(5))(tip(2)))) should equal(IL(1, 3, 5, 2))

      visitTree(
        bin 
          (bin 
            (bin(tip(1))(tip(3))) 
            (bin(tip(12))(tip(13))) 
          ) 
          (bin(tip(5))(tip(2)))) should equal(IL(1, 3, 12, 13, 5, 2)) 
    }
  }
}
