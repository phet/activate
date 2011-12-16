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
package k_k_.concurrent.activate.loiter.eval

import k_k_.concurrent.activate.core.Event


// for synchronous evaluation
trait Event_Record {
  def confirmed_?(event: Event): Boolean
}


// for asynchronous evaluation
trait Event_Observatory {
  def monitor(event: Event, observer: Event_Observer): Boolean

  val creation_tx: Transaction
}


// internal multi-level return; stacktrace unimportant, therefore 'object' ok
case object Disproven_Exception extends RuntimeException


class Sync(
  record: Event_Record
  ) {

  def calc_undetermined(guard: Guard): Option[Guard] = {
    def preorder(guard: Guard, negate_? : Boolean): Option[Guard] = {
      (guard, negate_?) match {
        case (Non_Guard, true) =>
          throw Disproven_Exception

        case (Non_Guard, false) =>
          None

        case (Existential_Guard(event), true) =>
          if (record.confirmed_?(event)) {
            throw Disproven_Exception
          } else {
            Some(Negated_Existential_Guard(event))
          }

        case (Existential_Guard(event), false) =>
          if (record.confirmed_?(event)) {
            None
          } else {
            Some(Existential_Guard(event))
          }

        case (Negated_Existential_Guard(event), true) =>
          if (record.confirmed_?(event)) {
            None
          } else {
            Some(Existential_Guard(event))
          }

        case (Negated_Existential_Guard(event), false) =>
          if (record.confirmed_?(event)) {
            throw Disproven_Exception
          } else {
            Some(Negated_Existential_Guard(event))
          }

        case (Conjoined_Guard(left, right), _) =>
          val lhs_undetermined = preorder(left, negate_?)
          val rhs_undetermined = preorder(right, negate_?)
          (lhs_undetermined, rhs_undetermined) match {
            case (Some(lhs), Some(rhs)) => Some(Conjoined_Guard(lhs, rhs))
            case (Some(_), None)        => lhs_undetermined
            case (None, Some(_))        => rhs_undetermined
            case (None, None)           => None
          }

        case (Disjoined_Guard(left, right), _) =>
          try {
            val lhs_undetermined = preorder(left, negate_?)
            lhs_undetermined match {
              case None => None // disjunction fully deter. once either side is
              case Some(lhs) =>
                try {
                  preorder(right, negate_?) match {
                    case None => None // disjunction deter. once either side is
                    case Some(rhs) => Some(Disjoined_Guard(lhs, rhs))
                  }
                } catch {
                  case Disproven_Exception => lhs_undetermined
                }
            }
          } catch {
            case Disproven_Exception => preorder(right, negate_?)
          }

        // convert to Negated Normal Form by application of DeMorgan's Law:
        case (Negated_Guard(Conjoined_Guard(left, right)), _) =>
          preorder(Disjoined_Guard(left, right), !negate_?)

        case (Negated_Guard(Disjoined_Guard(left, right)), _) =>
          preorder(Conjoined_Guard(left, right), !negate_?)

        case (Negated_Guard(expr), _) =>
          preorder(expr, !negate_?)
      }
    }
    preorder(guard, false)
  }
}


class Async(
  observatory: Event_Observatory
  ) {

  def evaluate(guard: Guard, observer: Guard_Observer) {
    def preorder(guard: Guard, observer: Guard_Observer, negate_? : Boolean):
        Boolean = {
      (guard, negate_?) match {
        case (Non_Guard, true) =>
          observer.indelibly_false(observatory.creation_tx)
          true

        case (Non_Guard, false) =>
          observer.indelibly_true(observatory.creation_tx)
          true

        case (Existential_Guard(event), true) =>
          val evaluator = new Negated_Existential_Eval(observer)
          observatory.monitor(event, evaluator.observer) ||
          (evaluator.announce_non_existence && false)

        case (Existential_Guard(event), false) =>
          val evaluator = new Existential_Eval(observer)
          observatory.monitor(event, evaluator.observer)

        case (Negated_Existential_Guard(event), true) =>
          val evaluator = new Existential_Eval(observer)
          observatory.monitor(event, evaluator.observer)

        case (Negated_Existential_Guard(event), false) =>
          val evaluator = new Negated_Existential_Eval(observer)
          observatory.monitor(event, evaluator.observer) ||
          (evaluator.announce_non_existence && false)

        case (Conjoined_Guard(left, right), _) =>
          val evaluator = new Conjoined_Eval(observer)
          val determination_made_? =
            preorder(left, evaluator.lhs_observer, negate_?)
          if (determination_made_? && evaluator.indelibly_decided_?)
            true
          else
            preorder(right, evaluator.rhs_observer, negate_?)

        case (Disjoined_Guard(left, right), _) =>
          val evaluator = new Disjoined_Eval(observer)
          val determination_made_? =
            preorder(left, evaluator.lhs_observer, negate_?)
          if (determination_made_? && evaluator.indelibly_decided_?)
            true
          else
            preorder(right, evaluator.rhs_observer, negate_?)

        // convert to Negated Normal Form by application of DeMorgan's Law:
        case (Negated_Guard(Conjoined_Guard(left, right)), _) =>
          preorder(Disjoined_Guard(left, right), observer, !negate_?)

        case (Negated_Guard(Disjoined_Guard(left, right)), _) =>
          preorder(Conjoined_Guard(left, right), observer, !negate_?)

        case (Negated_Guard(expr), _) =>
          preorder(expr, observer, !negate_?)
      }
    }
    preorder(guard, observer, false)
  }

  protected class Existential_Eval(
    parent: Guard_Observer
    ) {
    def observer =
      new Event_Observer {
        def exists(tx: Transaction) {
          parent.indelibly_true(tx)
        }
      }
  }

  protected class Negated_Existential_Eval(
    parent: Guard_Observer
    ) {
    def observer =
      new Event_Observer {
        def exists(tx: Transaction) {
          parent.indelibly_false(tx)
        }
      }
    def announce_non_existence: Boolean = {
      parent.tentatively_true(observatory.creation_tx)
      false
    }
  }

  protected abstract class Binary_Eval(
    parent: Guard_Observer
    ) {

    protected val sm: Dual_Guard_Observer

    def indelibly_decided_? : Boolean =
      false

    def lhs_observer =
      new Guard_Observer {
        def indelibly_true  (tx: Transaction) { sm.a__indelibly_true(tx)   }
        def indelibly_false (tx: Transaction) { sm.a__indelibly_false(tx)  }
        def tentatively_true(tx: Transaction) { sm.a__tentatively_true(tx) }
        def indeterminate   (tx: Transaction) { sm.a__indeterminate(tx)    }
      }

    def rhs_observer =
      new Guard_Observer {
        def indelibly_true  (tx: Transaction) { sm.b__indelibly_true(tx)   }
        def indelibly_false (tx: Transaction) { sm.b__indelibly_false(tx)  }
        def tentatively_true(tx: Transaction) { sm.b__tentatively_true(tx) }
        def indeterminate   (tx: Transaction) { sm.b__indeterminate(tx)    }
      }
  }

  protected class Conjoined_Eval(
    parent: Guard_Observer
    ) extends Binary_Eval(parent) {

    protected val sm = new Conjoined_SM(parent)

    override def indelibly_decided_? : Boolean =
      false
  }

  protected class Disjoined_Eval(
    parent: Guard_Observer
    ) extends Binary_Eval(parent) {

    protected val sm = new Disjoined_SM(parent)

    override def indelibly_decided_? : Boolean =
      false
  }
}
