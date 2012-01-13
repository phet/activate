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


class Conjoined_SM(
  observer: Guard_Observer
  ) extends Binary_SM {

  import Binary_SM._

  protected def START_STATE: State = A_Ind__B_Ind


  private abstract class Base_State extends State {
    def dispatch_a__indelibly_false  (tx: Transaction): Thunk = {
      state = Final_State
      () => observer.indelibly_false(tx)
    }
    def dispatch_b__indelibly_false  (tx: Transaction): Thunk = {
      state = Final_State
      () => observer.indelibly_false(tx)
    }
  }

  /* Notational Key (to state names):
   *   A - designates the 'first' child
   *   B - designates the 'second' child
   *
   *   Ind - "Indeterminate"
   *   Ten - "Tentative(ly True)"
   *   T   - "True"
   *   F   - "False"
   *
   * examples:
   *   A_Ind__B_Ten - 'first' child "Indeterminate", 'second' child "Tentative"
   *   A_T__B_Ind   - 'first' child "True", 'second' child "Indeterminate"
   */

  private object A_Ind__B_Ind extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = {
      state = A_T__B_Ind
      Noop
    }
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = {
      state = A_Ten__B_Ind
      Noop
    }
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = fail()
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = {
      state = A_Ind__B_T
      Noop
    }
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = {
      state = A_Ind__B_Ten
      Noop
    }
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = fail()
  }

  private object A_T__B_Ind extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = fail()
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = fail()
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = {
      state = Final_State
      () => observer.indelibly_true(tx)
    }
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = {
      state = A_T__B_Ten
      () => observer.tentatively_true(tx)
    }
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = fail()
  }

  private object A_Ten__B_Ind extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = {
      state = A_T__B_Ind
      Noop
    }
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = {
      state = A_Ind__B_Ind
      Noop
    }
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = {
      state = A_Ten__B_T
      () => observer.tentatively_true(tx)
    }
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = {
      state = A_Ten__B_Ten
      () => observer.tentatively_true(tx)
    }
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = fail()
  }

  private object A_T__B_Ten extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = fail()
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = fail()
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = {
      state = Final_State
      () => observer.indelibly_true(tx)
    }
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = {
      state = A_T__B_Ind
      () => observer.indeterminate(tx)
    }
  }

  private object A_Ten__B_Ten extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = {
      state = A_T__B_Ten
      Noop
    }
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = {
      state = A_Ind__B_Ten
      () => observer.indeterminate(tx)
    }
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = {
      state = A_Ten__B_T
      Noop
    }
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = {
      state = A_Ten__B_Ind
      () => observer.indeterminate(tx)
    }
  }

  private object A_Ind__B_T extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = {
      state = Final_State
      () => observer.indelibly_true(tx)
    }
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = {
      state = A_Ten__B_T
      () => observer.tentatively_true(tx)
    }
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = fail()
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = fail()
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = fail()
  }

  private object A_Ten__B_T extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = {
      state = Final_State
      () => observer.indelibly_true(tx)
    }
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = {
      state = A_Ind__B_T
      () => observer.indeterminate(tx)
    }
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = fail()
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = fail()
  }

  private object A_Ind__B_Ten extends Base_State {
    def dispatch_a__indelibly_true  (tx: Transaction): Thunk = {
      state = A_T__B_Ten
      () => observer.tentatively_true(tx)
    }
    def dispatch_a__tentatively_true(tx: Transaction): Thunk = {
      state = A_Ten__B_Ten
      () => observer.tentatively_true(tx)
    }
    def dispatch_a__indeterminate   (tx: Transaction): Thunk = fail()
    def dispatch_b__indelibly_true  (tx: Transaction): Thunk = {
      state = A_Ind__B_T
      Noop
    }
    def dispatch_b__tentatively_true(tx: Transaction): Thunk = fail()
    def dispatch_b__indeterminate   (tx: Transaction): Thunk = {
      state = A_Ind__B_Ind
      Noop
    }
  }
}
