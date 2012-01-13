/**
 * Activate Library for Scala - Concurrency-Oriented Programming in the
 * Activity Activation Style.
 *
 * Copyright (c)2009-2012 by Corbin "Kip" Kohn
 * All Rights Reserved.
 *
 *  Released under the terms of the Artistic License 2.0; see:
 *   http://www.opensource.org/licenses/artistic-license-2.0.php
 *
*/
package k_k_.concurrent.activate


package object util {

  def opt_iterate[T](v: T)(f: T => Option[T]): Iterator[T] =
    Iterator.iterate(Option(v)) {
      _.flatMap(f)
    }.takeWhile { _.isDefined }.map { _.get }


  // micro-benchmark to determine whether faster than:
  // xs.reverse.map f
  // xs.view.reverse.map f
  def reverse_foreach[T](f: T => Unit)(xs: List[T]) {
    xs match {
      case Nil => ()
      case x :: xs => reverse_foreach(f)(xs); f(x)
    }
  }
}
