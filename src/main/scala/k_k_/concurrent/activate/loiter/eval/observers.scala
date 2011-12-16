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


trait Event_Observer {
  def exists(tx: Transaction)
}

trait Guard_Observer {
  def indelibly_true  (tx: Transaction)
  def indelibly_false (tx: Transaction)
  def tentatively_true(tx: Transaction)
  def indeterminate   (tx: Transaction)
}

trait Dual_Guard_Observer {
  def a__indelibly_true  (tx: Transaction)
  def a__indelibly_false (tx: Transaction)
  def a__tentatively_true(tx: Transaction)
  def a__indeterminate   (tx: Transaction)

  def b__indelibly_true  (tx: Transaction)
  def b__indelibly_false (tx: Transaction)
  def b__tentatively_true(tx: Transaction)
  def b__indeterminate   (tx: Transaction)
}
