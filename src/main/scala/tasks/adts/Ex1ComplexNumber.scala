package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */
import u02.Tuples.Tup2
import u03.Sequences.*
import u03.Sequences.Sequence.*

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex
    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double, Double)
    def complex(re: Double, im: Double): Complex = (re, im)
    extension (complex: Complex)
      def re(): Double = complex match
        case (re, im) => re
      def im(): Double = complex match
        case (re, im) => im
      def sum(other: Complex): Complex = (complex, other) match
        case ((re1, im1),(re2, im2)) => (re1 + re2, im1 + im2)

      def subtract(other: Complex): Complex = (complex, other) match
        case ((re1, im1),(re2, im2)) => (re1 - re2, im1 - im2)
      def asString(): String = 
        var reString = ""
        complex match
          case (re, im) if (im < 0) => "" + re + " - " + im.abs + "i"
          case (re, im) if (im > 0) => "" + re + " + " + im + "i"
          case (re, im) => "" + re
