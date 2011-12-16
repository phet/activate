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


class Disjoined_SM(
  observer: Guard_Observer
  ) extends Binary_SM {

  protected def START_STATE = A_Ind__B_Ind

  // NOTE: protected, so A_Ind__B_Ind may escape to Binary_SM as START_STATE
  protected abstract class Base_State extends State {
    def dispatch_a__indelibly_true  (tx: Transaction): Option[Thunk] = {
      state = Final_State
      Some(() => observer.indelibly_true(tx))
    }
    def dispatch_b__indelibly_true  (tx: Transaction): Option[Thunk] = {
      state = Final_State
      Some(() => observer.indelibly_true(tx))
    }
  }

  // NOTE: protected, so object may escape to Binary_SM (base) as START_STATE
  protected object A_Ind__B_Ind extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_F__B_Ind
      None
    }
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_Ind
      Some(() => observer.tentatively_true(tx))
    }
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_F
      None
    }
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_Ten
      Some(() => observer.tentatively_true(tx))
    }
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = error()
  }

  private object A_F__B_Ind extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = error()
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = Final_State
      Some(() => observer.indelibly_false(tx))
    }
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = {
      state = A_F__B_Ten
      Some(() => observer.tentatively_true(tx))
    }
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = error()
  }

  private object A_Ten__B_Ind extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_F__B_Ind
      Some(() => observer.indeterminate(tx))
    }
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_Ind
      Some(() => observer.indeterminate(tx))
    }
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_F
      None
    }
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_Ten
      None
    }
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = error()
  }

  private object A_F__B_Ten extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = error()
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = Final_State
      Some(() => observer.indelibly_false(tx))
    }
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = {
      state = A_F__B_Ind
      Some(() => observer.indeterminate(tx))
    }
  }

  private object A_Ten__B_Ten extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_F__B_Ten
      None
    }
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_Ten
      None
    }
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_F
      None
    }
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_Ind
      None
    }
  }

  private object A_Ind__B_F extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = Final_State
      Some(() => observer.indelibly_false(tx))
    }
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_F
      Some(() => observer.tentatively_true(tx))
    }
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = error()
  }

  private object A_Ind__B_Ten extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_F__B_Ten
      None
    }
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = {
      state = A_Ten__B_Ten
      None
    }
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_F
      Some(() => observer.indeterminate(tx))
    }
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_Ind
      Some(() => observer.indeterminate(tx))
    }
  }

  private object A_Ten__B_F extends Base_State {
    def dispatch_a__indelibly_false (tx: Transaction): Option[Thunk] = {
      state = Final_State
      Some(() => observer.indelibly_false(tx))
    }
    def dispatch_a__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_a__indeterminate   (tx: Transaction): Option[Thunk] = {
      state = A_Ind__B_F
      Some(() => observer.indeterminate(tx))
    }
    def dispatch_b__indelibly_false (tx: Transaction): Option[Thunk] = error()
    def dispatch_b__tentatively_true(tx: Transaction): Option[Thunk] = error()
    def dispatch_b__indeterminate   (tx: Transaction): Option[Thunk] = error()
  }
}
