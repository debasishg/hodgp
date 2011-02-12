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
    it ("should sum a list") {
      def sumList = cata[Int, Int, ListF] {
        case Nil() => 0
        case Cons(x, xs) => x + xs
      } _

      sumList(cons(1)(cons(2)(nil))) should equal(3)
    }
  }

  describe("origami with btree") {
    it ("should sum a tree") {
      def sumTree = cata[Int, Int, BtreeF] {
        case Tip(d) => d
        case Bin(l, r) => l + r
      } _

      sumTree(bin(bin(tip(1))(tip(3)))(bin(tip(5))(tip(2)))) should equal(11)
    }
  }
}
