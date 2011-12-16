/**
 * Activate Library for Scala - Concurrency-Oriented Programming in the
 * Activity Activation Style.
 *
 * Copyright (c)2009-2011 by Corbin "Kip" Kohn
 * All Rights Reserved.
 *
 *  Released under the terms of the Artistic License 2.0; see:
 *   http://www.opensource.org/licenses/artistic-license-2.0.php
 *
*/
package k_k_.test.concurrent.activate

import k_k_.concurrent.activate.core._
import k_k_.concurrent.activate.core.Activarium._
import k_k_.concurrent.activate.loiter._
import k_k_.concurrent.activate.loiter.Guard._

import org.junit._
import Assert._


@Test
class TestActivate {

  // val env = new Activarium
  val @@ = new Activarium

  val e1 = new Event
  val e2 = new Event

  val guard = e1 && !e2

  val work1 = new Activatom(guard, Nil, Nil, () => println("Wee! This is fun!"))
  val work2 = new Activatom(guard, () => println("Tell me about it!"))

//  @@.submit(work1)
//  @@.submit(work2)

  println("affirm in 2s!")
  Thread.sleep(2000)
//  @@.affirm(e1);
  println("affirmed!")

  def pythag(a: Int, b: Int): Promissory_Event[Int] = {
    val (a_result, b_result, answer) = (Val_Event[Int], Val_Event[Int],
                                        Val_Event[Int])
    @@.submit(+> { @@.affirm(a_result(a*a)) } ::
               +> { @@.affirm(b_result(b*b)) } ::
               (a_result && b_result) ?+> (
                  @@.affirm(answer(Math.sqrt(?(a_result) + ?(b_result))))
                 ) :: Nil)
    answer
  }

  def factorial_rec(n: Int): Promissory_Event[Int] = {
    if (n < 0) {
      throw new IllegalArgumentException("'" + n + "' < 0")
    } else {
      val result = Val_Event[Int]
      val inner = factorial(n-1)
      @@.submit(inner ?+> @@.affirm(result(n * ?(factorial(n-1)))))
      result
    }
  }

  def factorial(n: Int): Promissory_Event[Int] = {
    if (n < 0) {
      throw new IllegalArgumentException("'" + n + "' < 0")
    } else {
      val result = Val_Event[Int]
      if (n == 0) {
        @@.affirm(result(1))
      } else {
        @@.submit(~> { () =>
                       val inner = factorial(n-1)
                       @@.submit(inner ?+> { () =>
                                  @@.affirm(result(n * ?(inner))) })
                     })
      }
      result
    }
  }


/*
  def pythag(cont: (Int) => Unit)(a: Int, b: Int) = {
    val (a_result, b_result) = (Val_Event[Int], Val_Event[Int])
    @@.submit(+> { @@.affirm(a_result(a*a)) } ::
               +> { @@.affirm(b_result(b*b)) } ::
               (a_result && b_result) ?+> { () =>
                 val result = Math.sqrt(get(a_result) + get(b_result))
                 cont(result)
               }:: Nil)
  }
*/

/*
  def pythag(cont: (Int) => Unit)(a: Int, b: Int) = {
    val (a_computed, b_computed) = (new Event, new Event)
    var a_result: Int = 0
    var b_result: Int = 0
    @@.submit(+> { a_result = a*a; @@.affirm(a_computed)} ::
               +> { b_result = b*b; @@.affirm(b_computed)} ::
               (a_computed && b_computed) ?+> {
                 val result = Math.sqrt(a_result + b_result)
                 cont(result)
               } :: Nil)
  }
*/


// `~!@#$%^&*()_-+={[}]|\:;"'<,>.?/
// `~!@#$%^&*  _-+=    | :  < > ?/
// `~!@#$%  *   -+=         < > ?

//  val f = a_computed && b_computed ?~>
//              { val result = Math.sqrt(a_result + b_result)



/*
  def pythag(cont: (Int) => Unit)(a: Int, b: Int) = {
    val (a_computed, b_computed) = (new Event, new Event)
    var a_result: Int = 0
    var b_result: Int = 0
    @@.submit(new Activatom({ a_result = a*a; @@.affirm(a_computed)}) ::
               new Activatom({ b_result = b*b; @@.affirm(b_computed)}) ::
               new Activatom(a_computed && b_computed, {
                               val result = Math.sqrt(a_result + b_result)
                               cont(result)
                             }) :: Nil)
  }

  def pythag(cont: (Int) => Unit)(a: Int, b: Int) = {
    val (a_computed, b_computed) = (new Event, new Event)
    var a_result: Int = 0
    var b_result: Int = 0
    @@.submit(new Activatom({ a_result = a*a; @@.affirm(a_computed)}))
    @@.submit(new Activatom({ b_result = b*b; @@.affirm(b_computed)}))
    @@.submit(new Activatom(a_computed && b_computed,
                             { val result = Math.sqrt(a_result + b_result)
                               cont(result)}))
  }
*/

  // pythag((c: Int) => println("(cont) pythag(3,4) == (" + c + ")"))(3, 4)

//  val result = pythag(3, 4)
//  @@.submit(result ?+>
//             println("(promised) pythag(3,4) == (" + ?(result) + ")"))


 val pythag_result = pythag(3, 4)
    @@.submit(pythag_result ?+>
               println("pythag(3, 4) == (" + ?(pythag_result) + ")"))


  def print_factorial(n: Int) {
    val fac_result = factorial(n)
    @@.submit(fac_result ?+>
               println("factorial(" + n + ") == (" + ?(fac_result) + ")"))
  }

  print_factorial(5)
  print_factorial(15)


  import scala.util.Random

  def async_daemon(stop: Event): Event = {
    val failed = new Event
    val rand = new Random
    def loop {
      @@.submit(!stop ?+> { () =>
                   try {
                     val v = rand.nextInt(10)
                     if (v % 5 == 0) {
                       println("dying on " + v)
                       throw new Exception("aargh!")
                     } else {
                       println("generated " + v)
                     }
                     Thread.sleep(500)
                     loop
                   } catch {
                     case e: Exception =>  @@.affirm(failed)
                   }
                 })
    }
    loop
    failed
  }

  def daemon(ignore: Event) {
    import scala.util.Random
    val rand = new Random
    while (true) {
      val v = rand.nextInt(10)
      if (v % 5 == 0) {
        println("dying on " + v)
        throw new Exception("aargh!")
      } else {
        println("generated " + v)
      }
      Thread.sleep(500)
    }
  }

  object Exec_Policy {
    def eternally(task: (Event) => Unit): Event = {
      val stop = new Event
      def rerun(n: Int) {
        val failed = new Event
        println("rerun")
        @@.submit(~> { () =>
                       try {
                         task(stop)
                       } finally {
                         @@.affirm(failed)
                       }
                     } ::
                  (failed && !stop) ?+> rerun(n+1) :: Nil)
      }
      rerun(0)
      stop
    }

    def async_eternally(task: (Event) => Event): Event = {
      val stop = new Event
      def rerun {
        val failed = task(stop)
        @@.submit((failed && !stop) ?+> rerun)
      }
      rerun
      stop
    }
  }

  // val stop = Exec_Policy.eternally(daemon)
  val stop = Exec_Policy.async_eternally(async_daemon)

  Thread.sleep(5000)
  println("=====affirming stop=====")
  @@.affirm(stop)
  Thread.sleep(5000)

  def parallel_map[T](arr: Array[T])(f: T => T): Promissory_Event[Array[T]] = {
    val result_event = Val_Event[Array[T]]
    val result = new Array[T](arr.length)
    var result_ready: Guard = Non_Guard
    for (val i <- 0 until arr.length) {
      val index_updated = new Event
      result_ready &&= index_updated
      @@.submit(~> { () =>
                     result(i) = f(arr(i)); @@.affirm(index_updated)
                   })
    }
    // loiter on result_ready, now that it's fully described
    @@.submit(result_ready ?+> @@.affirm(result_event(result)))
    result_event
  }

  val array = new Array[Int](8)
  for (val i <- 0 until 8) {
    array(i) = i
  }
  println("mapping")
  val map_result = parallel_map(array)((x) => x + x)
  @@.submit(map_result ?+>
             println("map(" + array.toString + ") == (" + ?(map_result) + ")"))


  object defer {
    def apply[P1,R](f: P1 => R)(a1: P1): (Event, Promissory_Event[R]) = {
    // def apply[P1,R](f: P1 => R, a1: P1): (Event, Promissory_Event[R]) = {
      val (start, result) = (new Event, Val_Event[R])
      @@.submit(start ?+> @@.affirm(result(f(a1))))
      (start, result)
    }

    def apply[P1,P2,R](f: (P1, P2) => R)(a1: P1, a2: P2):
        (Event, Promissory_Event[R]) = {
      val (start, result) = (new Event, Val_Event[R])
      @@.submit(start ?+> @@.affirm(result(f(a1, a2))))
      (start, result)
    }
  }

  object async {
    def apply[P1,R](f: P1 => R)(a1: P1): Promissory_Event[R] = {
      val (start, result) = defer(f)(a1)
      @@.affirm(start)
      result
    }

    def apply[P1,P2,R](f: (P1, P2) => R)(a1: P1, a2: P2):
        Promissory_Event[R] = {
      val (start, result) = defer(f)(a1, a2)
      @@.affirm(start)
      result
    }
  }


  def identity[T](x: T) = x

  val (start, defer_result) = defer((x: Int) => x)(42)
  // val (start, defer_result) = defer((x: Int) => x, 42)
  // val (start, defer_result) = defer(identity[Int] _)(42)
  @@.submit(defer_result ?+>
             println("defer(1) result is (" + ?(defer_result) + ")"))
  println("waiting .5s before func apply")
  Thread.sleep(500)
  @@.affirm(start)

  val (start2, defer_result2) = defer((x: Int, y: Int) => x + y)(42, -42)
  @@.submit(defer_result2 ?+>
             println("defer(2) result is (" + ?(defer_result2) + ")"))
  println("waiting .5s before func apply")
  Thread.sleep(500)
  @@.affirm(start2)


  val async_result = async((x: Int) => x)(42)
  @@.submit(async_result ?+>
             println("async(1) result is (" + ?(async_result) + ")"))

  val async_result2 = async((x: Int, y: Int) => x + y)(42, -42)
  @@.submit(async_result2 ?+>
             println("async(2) result is (" + ?(async_result2) + ")"))


  @Test
  def testOK() = assertTrue(true)

//  @Test
//  def testKO() = assertTrue(false)

}
