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
package k_k_.concurrent.activate.loiter

package eval {

import k_k_.concurrent.activate.core.Event

import scala.util.control.Exception.{catching, handling}


// for synchronous evaluation
trait Event_History {
  def is_confirmed(event: Event): Boolean
}

object Event_History {

  def apply(is_conf: Event => Boolean) =
    new Event_History {
      def is_confirmed(event: Event) = is_conf(event)
    }
}


// for asynchronous evaluation
trait Event_Observatory {
  /** registers `event` with `observer`
   * @return true iff observer notified during registration, hence never again
   */
  def register(event: Event, observer: Event_Observer): Boolean
  val origin_tx: Transaction
}

object Event_Observatory {

  def apply(orig_tx: Transaction)(reg: (Event, Event_Observer) => Boolean) =
    new Event_Observatory {
      def register(event: Event, obs: Event_Observer): Boolean = reg(event, obs)
      val origin_tx = orig_tx
    }
}


object Synchronous {

  // internal multi-level 'return'; stacktrace unimportant, so 'object' fine
  class Contradiction extends RuntimeException
  case object Contradiction extends Contradiction
}

class Synchronous(history: Event_History) {

  /**
   * @return simplified `Guard` of what still undetermined, None iff fully det.
   * @throws Contradiction iff determined `Guard` impossible to satisfy
   */
  def calc_undetermined(guard: Guard): Option[Guard] = {
    import Synchronous.Contradiction

    def preorder(guard: Guard, negate: Boolean): Option[Guard] =
      (guard, negate) match {
        case (Null_Guard, false) =>
          None

        case (Null_Guard, true) =>
          throw Contradiction

        case (g @ Existential_Guard(event), false) =>
          if (history.is_confirmed(event)) None
          else                             Some(g)

        case (Existential_Guard(evnt), true) =>
          if (history.is_confirmed(evnt)) throw Contradiction
          else                            Some(Negated_Existential_Guard(evnt))

        case (g @ Negated_Existential_Guard(event), false) =>
          if (history.is_confirmed(event)) throw Contradiction
          else                             Some(g)

        case (Negated_Existential_Guard(event), true) =>
          if (history.is_confirmed(event)) None
          else                             Some(Existential_Guard(event))


        // rewrite in Negated Normal Form by application of DeMorgan's Law:
        case (Negated_Guard(Conjoined_Guard(l, r)), _) =>
          preorder(Disjoined_Guard(l, r), !negate)

        case (Negated_Guard(Disjoined_Guard(l, r)), _) =>
          preorder(Conjoined_Guard(l, r), !negate)

        case (Negated_Guard(expr), _) =>
          preorder(expr, !negate)


        case (Conjoined_Guard(l, r), _) =>
          val lhs_undetermined = preorder(l, negate)
          val rhs_undetermined = preorder(r, negate)
          (lhs_undetermined, rhs_undetermined) match {
            case (None, None)           => None
            case (Some(lhs), Some(rhs)) => Some(Conjoined_Guard(lhs, rhs))
            case (Some(_), None)        => lhs_undetermined
            case (None, Some(_))        => rhs_undetermined
          }

        case (Disjoined_Guard(l, r), _) =>
          // short-circuit: disjunction fully determined once either side is
          catching(classOf[Contradiction]) either preorder(l, negate) match {
            case Left(contradiction) =>
              preorder(r, negate) // r Guard shall now determine all
            case Right(lhs_undetermined) =>
              handling(classOf[Contradiction]) by { _ =>
                lhs_undetermined // l Guard would remain to determine all
              } apply {
                for {
                  lhs <- lhs_undetermined
                  rhs <- preorder(r, negate)
                } yield {
                  Disjoined_Guard(lhs, rhs)
                }
              }
          }
        /******** equivalent formulation:
          try {
            // map past None since disjunction fully deter. once either side is
            preorder(l, negate) flatMap { lhs =>
              try {
                preorder(r, negate) map { rhs =>
                  Disjoined_Guard(lhs, rhs)
                }
              } catch {
                case Contradiction => Some(lhs)
              }
            }
          } catch {
            case Contradiction => preorder(r, negate)
          }
         */
      }

    preorder(guard, false)
  }
}


object Asynchronous {

  private class Existential_Eval(parent: Guard_Observer) {
    val observer = Event_Observer { (tx: Transaction) =>
      parent.indelibly_true(tx)
    }
  }

  private class Negated_Existential_Eval(parent: Guard_Observer) {
    val observer = Event_Observer { (tx: Transaction) =>
      parent.indelibly_false(tx)
    }

    def announce_non_existent_event(tx: Transaction): Boolean = {
      parent.tentatively_true(tx)
      false
    }
  }

  private abstract class Binary_Eval(
    sm: Dual_Guard_Observer
    ) {

    def is_indelibly_determined: Boolean

    def lhs_observer =
      new Guard_Observer {
        def indelibly_true  (tx: Transaction) { sm.a__indelibly_true(tx)
                                                lhs_indelibly = Some(true) }
        def indelibly_false (tx: Transaction) { sm.a__indelibly_false(tx)
                                                lhs_indelibly = Some(false)}
        def tentatively_true(tx: Transaction) { sm.a__tentatively_true(tx) }
        def indeterminate   (tx: Transaction) { sm.a__indeterminate(tx)    }
      }

    def rhs_observer =
      new Guard_Observer {
        def indelibly_true  (tx: Transaction) { sm.b__indelibly_true(tx)
                                                rhs_indelibly = Some(true) }
        def indelibly_false (tx: Transaction) { sm.b__indelibly_false(tx)
                                                rhs_indelibly = Some(false)}
        def tentatively_true(tx: Transaction) { sm.b__tentatively_true(tx) }
        def indeterminate   (tx: Transaction) { sm.b__indeterminate(tx)    }
      }

    protected def count_indelible_where(f: Boolean => Boolean): (Int, Int) =
      Seq(lhs_indelibly, rhs_indelibly).map { opt =>
        if (opt.isEmpty) (0, 0)
        else             (1, opt.count(f))
      }.reduce { (x, y) =>
        (x._1 + y._1, x._2 + y._2)
      }

    @volatile private[this] var lhs_indelibly: Option[Boolean] = None
    @volatile private[this] var rhs_indelibly: Option[Boolean] = None
  }

  private class Conjoined_Eval(parent: Guard_Observer)
      extends Binary_Eval(new Conjoined_SM(parent)) {

    override def is_indelibly_determined: Boolean = {
      val (n_determined, n_false) = count_indelible_where { _ == false }
      n_determined == 2 || n_false > 0
    }
  }

  private class Disjoined_Eval(parent: Guard_Observer)
      extends Binary_Eval(new Disjoined_SM(parent)) {

    override def is_indelibly_determined: Boolean = {
      val (n_determined, n_true) = count_indelible_where { _ == true }
      n_determined == 2 || n_true > 0
    }
  }
}

class Asynchronous(observatory: Event_Observatory) {

  def track(guard: Guard, observer: Guard_Observer) {
    import Asynchronous._
    import observatory.origin_tx

    /** @return true/false whether fully determined */
    def preorder(guard: Guard, observer: Guard_Observer, negate: Boolean):
        Boolean =
      (guard, negate) match {
        case (Null_Guard, false) =>
          observer.indelibly_true(origin_tx)
          true

        case (Null_Guard, true) =>
          observer.indelibly_false(origin_tx)
          true

        case (Existential_Guard(event), false) =>
          val evaluator = new Existential_Eval(observer)
          observatory.register(event, evaluator.observer)

        case (Existential_Guard(event), true) =>
          val evaluator = new Negated_Existential_Eval(observer)
          observatory.register(event, evaluator.observer) ||
            evaluator.announce_non_existent_event(origin_tx)

        case (Negated_Existential_Guard(event), false) =>
          val evaluator = new Negated_Existential_Eval(observer)
          observatory.register(event, evaluator.observer) ||
            evaluator.announce_non_existent_event(origin_tx)

        case (Negated_Existential_Guard(event), true) =>
          val evaluator = new Existential_Eval(observer)
          observatory.register(event, evaluator.observer)


        // rewrite in Negated Normal Form by application of DeMorgan's Law:
        case (Negated_Guard(Conjoined_Guard(l, r)), _) =>
          preorder(Disjoined_Guard(l, r), observer, !negate)

        case (Negated_Guard(Disjoined_Guard(l, r)), _) =>
          preorder(Conjoined_Guard(l, r), observer, !negate)

        case (Negated_Guard(expr), _) =>
          preorder(expr, observer, !negate)


        case (Conjoined_Guard(l, r), _) =>
          val evaluator = new Conjoined_Eval(observer)
          val is_determined = preorder(l, evaluator.lhs_observer, negate)
          if (is_determined && evaluator.is_indelibly_determined)
            true
          else
            preorder(r, evaluator.rhs_observer, negate)

        case (Disjoined_Guard(l, r), _) =>
          val evaluator = new Disjoined_Eval(observer)
          val is_determined = preorder(l, evaluator.lhs_observer, negate)
          if (is_determined && evaluator.is_indelibly_determined)
            true
          else
            preorder(r, evaluator.rhs_observer, negate)
      }

    preorder(guard, observer, false)
  }
}

}
