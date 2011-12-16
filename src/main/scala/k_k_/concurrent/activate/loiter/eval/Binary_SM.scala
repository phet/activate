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


protected abstract class Binary_SM
    extends Dual_Guard_Observer {

  protected type Thunk = () => Unit

  protected trait State {
    def dispatch_a__indelibly_true  (tx: Transaction): Option[Thunk]
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk]
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk]
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk]

    def dispatch_b__indelibly_true  (tx: Transaction): Option[Thunk]
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk]
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk]
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk]
  }

  protected object Final_State extends State {
    def dispatch_a__indelibly_true  (tx: Transaction): Option[Thunk] = None
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = None
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = None
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = None

    def dispatch_b__indelibly_true  (tx: Transaction): Option[Thunk] = None
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = None
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = None
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = None
  }

  protected def START_STATE: State

  protected var state = START_STATE

  protected def error(): Option[Thunk] =
    throw new IllegalStateException()

  private def exec_event_dispatch(event_dispatch: => Option[Thunk]) {
    event_dispatch match {
      case Some(notify) => notify()
      case None =>
    }
  }

  def a__indelibly_true  (tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_a__indelibly_true(tx) }
  }
  def a__indelibly_false (tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_a__indelibly_false(tx) }
  }
  def a__tentatively_true(tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_a__tentatively_true(tx) }
  }
  def a__indeterminate   (tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_a__indeterminate(tx) }
  }

  def b__indelibly_true  (tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_b__indelibly_true(tx) }
  }
  def b__indelibly_false (tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_b__indelibly_false(tx) }
  }
  def b__tentatively_true(tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_b__tentatively_true(tx) }
  }
  def b__indeterminate   (tx: Transaction) = synchronized {
    exec_event_dispatch { state.dispatch_b__indeterminate(tx) }
  }
}
