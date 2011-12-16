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
package k_k_.concurrent.activate.core

import k_k_.concurrent.activate.core.eval._
import k_k_.concurrent.activate.loiter._
import k_k_.concurrent.activate.loiter.eval._

import java.util.concurrent.{ConcurrentHashMap, Executors, ExecutorService}


class Event {
  def unary_! : Guard = Negated_Existential_Guard(this)
}

sealed abstract class Promissory_Event[T]
    extends Event {

  @volatile private var result: Option[T] = None

  def apply(value: T): Event

  def get: Option[T] =
    result

/*???
  error: a_result.? of type Int does not take parameters
                    env.affirm(answer(Math.sqrt(a_result?() + b_result?())))
                                                         ^
                    env.affirm(answer(Math.sqrt(a_result? + b_result?)))
                                                         ^
*/
  def ? : T =
    get match {
      case Some(v) => v
      case None => throw new Exception("promise has not been fulfilled!")
    }

  protected def set(v: T): Boolean = synchronized {
    result match {
      case Some(_) => false
      case None => result = Some(v); true
    }
  }
}


class Would_Deadlock extends Exception

object Would_Deadlock extends Would_Deadlock


object Activarium {

  def Val_Event[T]: Promissory_Event[T] =
    new Protected_Promissory_Event[T]

  def get[T](event: Promissory_Event[T]): T =
    event.get match {
      case Some(v) => v
      case None => throw new Exception("be patient!")
    }

  def ?[T](event: Promissory_Event[T]): T =
    get(event)

  private class Protected_Promissory_Event[T]
      extends Promissory_Event[T] {
    def apply(value: T): Event =
      new Fulfillable_Promissory_Event[T](this, value)

    def fulfill(value: T): Boolean =
      set(value)
  }

  private class Fulfillable_Promissory_Event[T](
    event: Protected_Promissory_Event[T],
    fulfillment: T
    ) extends Event {

    def fulfill: Boolean =
      event.fulfill(fulfillment)

    def get_event: Event =
      event
  }

  private abstract class Triggering_Event extends Event {
    def trigger: Unit
  }
}

class Activarium {

  import Activarium.Fulfillable_Promissory_Event

  private val tx_sequencer = new Transaction_Sequencer {
    def next: Transaction =
      new Transaction(next_id) {
        protected val execute = elaborate _ // Activarium.elaborate def'd below

        protected def handle_add_events_after_complete(events: List[Event]) {
          val shadow = create_shadow
          shadow.add_events(events)
          evaluation_executor.execute(new Runnable {
            def run() {
              try {
                elaborate(shadow)
              } catch {
                case e: Throwable => println("Throwable: " + e)
              }
            }
          })
        }

        protected def handle_add_pending_after_complete(pending: Pending) {
          val shadow = create_shadow
          shadow.add_pending(pending)
          evaluation_executor.execute(new Runnable {
            def run() {
              try {
                elaborate(shadow)
              } catch {
                case e: Throwable => println("Throwable: " + e)
              }
            }
          })
        }
      }
  }

  private val init_tx = tx_sequencer.next
  init_tx.complete

  private val async =
    new Async(new Event_Observatory {
      def monitor(event: Event, observer: Event_Observer): Boolean =
        Activarium.this.monitor(event, observer)

      val creation_tx = init_tx
    })

  private val sync =
    new Sync(new Event_Record {
      def is_confirmed(event: Event): Boolean =
        Activarium.this.is_confirmed(true)(event)
    })


  private val event_register = new ConcurrentHashMap[Event, Event_Exposition]

  // Executor for user-defined activities
  private val activity_executor = create_activity_executor
  // Executor for internal management of events, guards, etc.
  private val evaluation_executor = create_evaluation_executor

  final def submit(as: List[Activatom]) {
    as foreach (do_submit _)
  }

  final def submit(a: Activatom) {
    do_submit(a)
  }

/**???????
 * [26:  env.submit(work1) ]
 * testOK(k_k_.AppTest)  Time elapsed: 0.04 sec  <<< ERROR!
java.lang.NoSuchMethodError: submit
        at k_k_.AppTest.<init>(activate.scala:26)
        at sun.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)
        at sun.reflect.NativeConstructorAccessorImpl.newInstance(NativeConstructorAccessorImpl.java:39)
        at sun.reflect.DelegatingConstructorAccessorImpl.newInstance(DelegatingConstructorAccessorImpl.java:27)
        at java.lang.reflect.Constructor.newInstance(Constructor.java:513)
        at org.junit.internal.runners.JUnit4ClassRunner.createTest(JUnit4ClassRunner.java:72)

  def submit(as: List[Activatom]): this.type = {
    as foreach (do_submit _)
    this
  }

  def submit(a: Activatom): Activarium = {
    do_submit(a)
    return this
  }
*/
  private def do_submit(a: Activatom) {
    val observ =
      new Activatable(a.activate_events, a.drop_events, a.activities).observer
    install(a.guard, observ)
  }

  final
  def affirm(events: List[Event]) {
    events foreach { atomic_affirm(_) }
  }

  final
  def affirm(event: Event) {
    atomic_affirm(List(event))
  }

  final
  def atomic_affirm(events: List[Event]) {
    val scrubbed_events = events map { _ match {
        case e: Fulfillable_Promissory_Event[_] => e.fulfill; e.get_event
        case e: Event => e
      }
    }
    evaluation_executor.execute(new Runnable {
      def run() {
        try {
          val tx = tx_sequencer.next
          tx.add_events(scrubbed_events)
          elaborate(tx)
        } catch {
          case e: Throwable => println("Throwable: " + e)
        }
      }
    })
  }

  // for completeness only: (silly function name+signature combo)
  final
  def atomic_affirm(event: Event) {
    atomic_affirm(List(event))
  }

  @throws(classOf[Would_Deadlock])
  final
  def await(guard: Guard) {
    await(0)(guard)
  }

  @throws(classOf[Would_Deadlock])
  final
  def await(max_ms: Long)(guard: Guard): Boolean = {
    try {
      sync.calc_undetermined(guard) match {
	case None => true // guard has been fully satisfied
	case Some(undetermined_guard) => {
	  val wait_obj = new Object
	  val satisfied = new Activarium.Triggering_Event {
	    def trigger = wait_obj synchronized { wait_obj notify }
	  }
  	  val not_satisfiable = new Activarium.Triggering_Event {
	    def trigger = wait_obj synchronized { wait_obj notify }
          }
	  wait_obj synchronized {
	    while (!is_confirmed(false)(satisfied) &&
		   !is_confirmed(false)(not_satisfiable)) {
	      wait_obj.wait(max_ms) //??????
	    }
	  }
	  is_confirmed(false)(satisfied) || { 
            throw Would_Deadlock
          }
	}
      }
    } catch {
      case Disproven_Exception => throw Would_Deadlock
    }
  }

  protected def create_activity_executor: ExecutorService = {
    Executors.newCachedThreadPool()
  }

  protected def create_evaluation_executor: ExecutorService = {
    Executors.newCachedThreadPool()
  }

  private def elaborate(tx: Transaction) {
    val confirmation = Event_Confirmation(tx) // reuse for all Event`s
    while (!tx.complete) {
      val event = tx.next
      event_register.putIfAbsent(event, confirmation) match {
        // confirmed by another transaction
        case Event_Confirmation(other_tx) =>
        // NOTE: hopefully !(tx < other_tx), but too late to change now!
        case expect @ Event_Expectation(_) =>
          fulfill_expectation(event, expect, confirmation)
        case null => // done! (event affirmed before it was ever monitored)
      }
      event match {
	case triggering : Activarium.Triggering_Event => triggering.trigger
	case _ => ()
      }
    }
    tx.close
  }

  private def fulfill_expectation(event: Event, expect: Event_Expectation,
                                  confirm: Event_Confirmation) {
    if (event_register.replace(event, expect, confirm))
      expect.fulfill(confirm.tx)
    else
      event_register.get(event) match {
        // event confirmed in meanwhile; notification handled then
        case Event_Confirmation(tx) =>
          // NOTE: hopefully !(confirm.tx < tx), but too late to change now!
        case updated_expect @ Event_Expectation(_) =>
          // expectation updated in meanwhile; try to fullfill that instead
          fulfill_expectation(event, updated_expect, confirm)
      }
  }

  protected def install(guard: Guard, observer: Guard_Observer) {
    async.evaluate(guard, observer)
  }

  // req_tx_complete_? ensures isolation by only considering an event
  // `is_confirmed` once its transaction has completed
  private def is_confirmed(req_tx_complete_? : Boolean)(event: Event): Boolean =
    event_register.get(event) match {
      case Event_Confirmation(tx) if (!req_tx_complete_? || tx.has_completed) =>
                true
      case _ => false
    }

  private def monitor(event: Event, observer: Event_Observer): Boolean = {
    event_register.get(event) match {
      case Event_Confirmation(tx) =>
        observer.exists(tx); true
      case expect @ Event_Expectation(_) =>
        update_expectation(event, Some(expect), observer)
      case null =>
        update_expectation(event, None, observer)
    }
  }

  private def update_expectation(event: Event,
                                 curr_expect: Option[Event_Expectation],
                                 observer: Event_Observer):
      Boolean = {
    curr_expect match {
      case Some(expect) =>
        if (!event_register.replace(event, expect,
                                    expect.add_observer(observer)))
          event_register.get(event) match {
            // event confirmed in meanwhile; handle notification
            case Event_Confirmation(tx) =>
              observer.exists(tx)
              true
            case expect @ Event_Expectation(_) =>
              update_expectation(event, Some(expect), observer)
          }
        else
          false
      case None =>
        event_register.putIfAbsent(event, new Event_Expectation(observer))
          match {
            // event confirmed in meanwhile; handle notification
            case Event_Confirmation(tx) =>
              observer.exists(tx)
              true
            case expect @ Event_Expectation(_) =>
              update_expectation(event, Some(expect), observer)
            case null => // successfully added
              false
          }
    }
  }

  private def activate_!(activities: List[Activity]) {
    for { activity <- activities } {
      activity_executor.execute(new Runnable {
        def run() {
          try {
            activity()
          } catch {
            case e: Throwable => println("Throwable: " + e)
          }
        }
      })
    }
  }


/**
  1. implement conjoined and disjoined statecharts
     !!!!these statecharts must be locked since they have two children notifying them at same time!!!!!!!!!
     !!!!!use the later of two Transaction`s when they both figure in together!!!!!
       ?????how to get around the problem of only being able to have a single transaction, when the other tentative may be the one involved in the crucial atomic affirming??????
     !!!!finish implementing indelibly_decided_? !!!!
  2. return boolean to indicate whether Promissory_Event was actually fulfilled by this affirmation?????
  3. implement Timer_Event
  4. implement shutdown/stop
  4. determine whether to make 'observer' member val or def   
  5. decide whether to be less eager in activating a tentative immediately after transaction close.  ???what if it gets queued and does not run for some time, but in the meanwhile, would have been deemed eternally_false?????  in addition, would there be any guarantee not upheld if it were allowed to become eternally_true, instead of merely proceeding from tentative????
  6. ???what if a Promissory_Event is affirmed without a value???
  7. create a type in which Val_Event may be wrapped so those holding it may only wait, but not actually invoke it!!!
*/

  protected
  class Activatable(
    act_evts: List[Event],
    drop_evts: List[Event],
    activities: List[Activity]
    ) {
    private var determination_claimed = false
    private var valid_tentative_tx: Transaction = Nil_Transaction

    val observer =
      new Guard_Observer {
        def indelibly_true(tx: Transaction) {
          if (claim_singular_determination_?)
            do_activation(tx)
        }
        def indelibly_false(tx: Transaction) {
          if (claim_singular_determination_?)
            do_drop(tx)
        }
        def tentatively_true(tx: Transaction) {
          if (tx == init_tx) {
            // no concept of transactionality involved in case of invocation
            // immediately upon install (from negation of non-existent event)
            if (claim_singular_determination_?) {
              do_activation(tx)
	    }
          } else {
            set_tentative_tx(tx)
            tx.add_pending(new Pending {
              def release(tx: Transaction) {
                Activatable.this.attempt_activation(tx)
              }
            })
          }
        }
        def indeterminate(tx: Transaction) {
          clear_tentative_tx
        }
      }

    def attempt_activation(orig_tx: Transaction) {
      if (proceed_with_activation_?(orig_tx))
        do_activation(orig_tx)
    }

    private def do_activation(tx: Transaction) {
      if (!act_evts.isEmpty)
        tx.add_events(act_evts)
      activate_!(activities)
    }

    private def do_drop(tx: Transaction) {
      if (!drop_evts.isEmpty)
        tx.add_events(drop_evts)
    }

    private def proceed_with_activation_?(tx: Transaction): Boolean =
      synchronized {
        valid_tentative_tx != Nil_Transaction &&
        valid_tentative_tx.is_ancestor_of(tx) &&
        claim_singular_determination_?
    }

    // used to ensure that determination made at most once
    private def claim_singular_determination_? : Boolean = synchronized {
      if (!determination_claimed) {
        determination_claimed = true
        true
      } else {
        false
      }
    }

    private def set_tentative_tx(tx: Transaction) = synchronized {
      valid_tentative_tx = tx
    }

    private def clear_tentative_tx = synchronized {
      valid_tentative_tx = Nil_Transaction
    }
  }
}

