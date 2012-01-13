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
package k_k_.concurrent.activate.loiter.eval


protected object Binary_SM {

  type Thunk = () => Unit

  val Noop: Thunk = () => { }


  trait State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk
    def dispatch_a__indelibly_false (tx: Transaction): Thunk
    def dispatch_a__tentatively_true(tx: Transaction): Thunk
    def dispatch_a__indeterminate   (tx: Transaction): Thunk

    def dispatch_b__indelibly_true  (tx: Transaction): Thunk
    def dispatch_b__indelibly_false (tx: Transaction): Thunk
    def dispatch_b__tentatively_true(tx: Transaction): Thunk
    def dispatch_b__indeterminate   (tx: Transaction): Thunk
  }

  object Final_State extends State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = Noop
    def dispatch_a__indelibly_false (tx: Transaction): Thunk = Noop
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = Noop
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = Noop

    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = Noop
    def dispatch_b__indelibly_false (tx: Transaction): Thunk = Noop
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = Noop
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = Noop
  }
}

protected abstract class Binary_SM
    extends Dual_Guard_Observer {

  import Binary_SM._

  protected var state = START_STATE


  final def a__indelibly_true  (tx: Transaction) =
    exec_event_dispatch { state.dispatch_a__indelibly_true(tx) }

  final def a__indelibly_false (tx: Transaction) =
    exec_event_dispatch { state.dispatch_a__indelibly_false(tx) }

  final def a__tentatively_true(tx: Transaction) =
    exec_event_dispatch { state.dispatch_a__tentatively_true(tx) }

  final def a__indeterminate   (tx: Transaction) =
    exec_event_dispatch { state.dispatch_a__indeterminate(tx) }


  final def b__indelibly_true  (tx: Transaction) =
    exec_event_dispatch { state.dispatch_b__indelibly_true(tx) }

  final def b__indelibly_false (tx: Transaction) =
    exec_event_dispatch { state.dispatch_b__indelibly_false(tx) }

  final def b__tentatively_true(tx: Transaction) =
    exec_event_dispatch { state.dispatch_b__tentatively_true(tx) }

  final def b__indeterminate   (tx: Transaction) =
    exec_event_dispatch { state.dispatch_b__indeterminate(tx) }


  protected def START_STATE: State


  protected final def fail() =
    throw new IllegalStateException()


  private def exec_event_dispatch(event_dispatch: => Thunk) =
    // NOTE: no need to block other threads to exec thunk / subsequent dispatch
    synchronized { event_dispatch } apply()
}
