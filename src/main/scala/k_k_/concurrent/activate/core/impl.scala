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
package k_k_.concurrent.activate.core

import k_k_.concurrent.activate.core.eval._
import k_k_.concurrent.activate.loiter._
import k_k_.concurrent.activate.loiter.eval._

import org.slf4j.LoggerFactory

import scala.util.control.Exception.handling

import java.util.concurrent.{ConcurrentHashMap, TimeUnit, Executors,
                             ExecutorService, ScheduledExecutorService}


class Event {
  def unary_! = Negated_Existential_Guard(this)
}


case class Timing_Event private[core](ms: Long)
    extends Event


object Timing {
  /** Convert `Timing` to `Existential_Guard` based on a fresh 'timing event' */
  implicit def Timing2Guard(t: Timing): Existential_Guard = t.new_event
}

/**
 * A `Timing` represents a duration in the abstract, which may render any number
 * of (distinct) concrete 'timing events' to realize that duration.  These take
 * the form of a specially-handled `Event` that is automatically `affirm`ed at
 * the conclusion of the duration's measure.  An `Event's duration commences the
 * moment the first `Guard` in which it is present is `submit`ted to a given
 * `Activarium` (the affirmation of a 'timing event` is, as with that of every
 * `Event`, scoped per `Activarium`).  The `Event`s timing lifecycle is thus
 * common across all `Guard`s in which it participates; to achieve independence
 * between `Guard`s, use mutiple events of the same (or equivalent) `Timing`.
 */
case class Timing(ms: Long) {
  def this(n: Long, unit: TimeUnit) = this(unit.toMillis(n))

  // Timing_Event`s are always singularly atomic.
  // Even when submitted all for the first time as part of the same Guard,
  // affirmation for multiple Timing_Event`s may happen out of temporal order
  // (i.e. not necessarily in order of increasing `ms` values), as a result of
  // small values being dwarfed by Guard evaluation overhead (itself small).
  def new_event: Event = new Timing_Event(ms)
}


sealed trait Promise[T]
    extends Event {

  @volatile private[this] var result: Option[T] = None

  final def get: T =
    result.getOrElse(sys.error("promise has not been fulfilled!"))

  final def getOrElse[U >: T](default: => U): U = result.getOrElse(default)

  final def opt: Option[T] = result

  /** alias for `get` */
  final def ? : T = get


  /** alias for `fulfill(value)` */
  final def apply(value: T): Event = fulfill(value)

  /**
   *  fulfill the promise by providing its value, returning an Event, which
   *  must be `affirm`ed for a given `Activarium` to recognize the fulfillment
   */
  def fulfill(value: T): Event


  protected final def set_if_unset(v: T): Boolean = synchronized {
    result.isEmpty && {
      result = Some(v)
      true
    }
  }
}


object Activarium {

  final val num_timing_threads = 2

  // internal multi-level 'return'; stacktrace unimportant, so 'object' fine
  class Would_Deadlock extends Exception
  case object Would_Deadlock extends Would_Deadlock

  object Promise {
    def apply[T]: Promise[T] = new Promissory_Event[T]
  }

  def ?[T](promise: Promise[T]): T = promise.?


  private implicit def expr2Runnable[T](f: => T): Runnable =
    new Runnable {
      def run() = f
    }


  private class Promissory_Event[T]
      extends Promise[T] {

    def fulfill(value: T): Event = new Fulfillment_Event[T](this, value)

    private[Activarium] def private_update(value: T): Boolean =
      set_if_unset(value)
  }

  private object Fulfillment_Event {
    case class Conflict[T](existing: T, attempted: T)
  }

  private class Fulfillment_Event[T](
    private[Activarium] val child_event: Promissory_Event[T],
    fulfillment: T
    ) extends Event {

    import Fulfillment_Event.Conflict

    private[Activarium] def attempt(): Option[Conflict[T]] =
      if (child_event.private_update(fulfillment)) None
      else Some(Conflict(child_event.get, fulfillment))
  }


  private abstract class Triggering_Event extends Event {
    def pull(): Unit
  }
}

class Activarium {

  import Activarium._

  private val log = LoggerFactory.getLogger(classOf[Activarium])

  private val create_transaction: () => Transaction = {
    val tx_iterator = Transaction.create_id_iterator map { id =>
      new Transaction(id) {
        protected val execute = Activarium.this.elaborate _

        protected def handle_add_events_after_complete(events: List[Event]) {
          handle_after_complete_update(_.add_events(events), "Events")
        }

        protected def handle_add_deferred_after_complete(deferred: Deferred) {
          handle_after_complete_update(_.add_deferred(deferred), "Deferred")
        }

        private def handle_after_complete_update(
            update: Transaction => Unit,
            entity: String) {
          val shadow = create_shadow()
          update(shadow)
          // unclear why, but either `expr2Runnable` must be called explicitly,
          // `new Runnable` must be constructed directly, or apply[Unit] type
          // param specified, otherwise expr will not run.  FSR `expr2Runnable`
          // is used twice with different type params Any and Unit, instead of
          // simply once with Unit!
          evaluation_executor.execute {
            handling(classOf[Throwable]).by { e =>
              log.warn("(Post-Complete %s Elaboration): ".format(entity), e)
            }.apply[Unit](elaborate(shadow))
          }
        }
      }
    }

    () => tx_iterator.next()
  }

  private val initial_tx = create_transaction()

  private val async =
    //????should all async evaluation use the same initial tx???
    new Asynchronous(Event_Observatory(initial_tx) { Activarium.this.link _ })

  private val sync =
    new Synchronous(Event_History { Activarium.this.is_confirmed(true) _ })

  private val event_registry = new ConcurrentHashMap[Event, Event_Exposition]

  // Executor for user-defined activities
  private val activity_executor = create_activity_executor()
  // Executor for internal management of events, guards, etc.
  private val evaluation_executor = create_evaluation_executor()
  // (Scheduled) Executor dedicated to timing events
  private val timing_executor = create_timing_executor()


  /**
   * submit `Activatom`s for evaluation.
   * @return the *modified* `Activarium`
   */
  final def submit(as: Seq[Activatom]): Activarium = {
    as.foreach { install _ }
    this
  }

  /**
   * submit an `Activatom` for evaluation.
   * @return the *modified* `Activarium`
   */
  final def submit(a: Activatom): Activarium = {
    install(a)()
    this
  }


  /**
   * synonym for `affirm_atomic(event)`
   * @return the *modified* `Activarium`
   */
  final def affirm(event: Event): Activarium = {
    affirm_atomic(event)
  }

  /**
   * affirm all events, non-atomically
   * @return the *modified* `Activarium`
   */
  final def affirm(events: List[Event]): Activarium = {
    events.foreach { affirm_atomic _ }
    this
  }

  /**
   * affirm the event; since there's only one, it is, obviously, atomic
   * @return the *modified* `Activarium`
   */
  final def affirm_atomic(event: Event): Activarium = {
    affirm_atomic(List(event))
  }

  /**
   * affirm all events, atomically
   * @return the *modified* `Activarium`
   */
  final def affirm_atomic(events: List[Event]): Activarium = {
    val fulfilled_events = events map {
      case fulfillment: Fulfillment_Event[_] => // Promise[T] fulfillment
        import Fulfillment_Event.Conflict
        for {
          Conflict(existing, attempted) <- fulfillment.attempt()
        } {
          log.warn(("promise unfulfillable with [%s], since already " +
                    "fulfilled with [%s]").format(attempted, existing))
        }
        fulfillment.child_event
      case e: Event => e
    }
    // unclear why, but either `expr2Runnable` must be called explicitly,
    // `new Runnable` must be constructed directly, or apply[Unit] type param
    // specified, otherwise expr will not run.  FSR, implicit `expr2Runnable`
    // is used twice with different type params Any and Unit, instead of simply
    // once with Unit!
    evaluation_executor.execute {
      handling(classOf[Throwable]).by { e =>
        log.warn("(Elaborate) Throwable: ", e)
      }.apply[Unit] {
        val tx = create_transaction()
        tx.add_events(fulfilled_events)
        elaborate(tx)
      }
    }
    this
  }


  /**
   *  awaits determination of `guard`, aborting after `max_ms` millis
   *  @return
   *    - true iff `guard` satisfied
   *    - false iff `guard` undetermined (after waiting `max_ms`)
   *    - throw `Would_Deadlock` iff `guard` determined never to be satisfiable
   *  NOTE: (default) `max_ms` of 0L will wait indefinitely for determination
   */
  @throws(classOf[Would_Deadlock])
  final def await(guard: Guard, max_ms: Long = 0L): Boolean = {
    val start_ms = System.currentTimeMillis
    try {
      sync.calc_undetermined(guard) match {
        case None => true // guard has been fully satisfied
        case Some(undetermined_guard) => {
          val wait_obj = new Object
          val satisfied, not_satisfiable = new Triggering_Event {
            def pull() { wait_obj synchronized { wait_obj notify } }
          }
          val is_non_tx_event_confirmed = is_confirmed(false) _
          // NOTE: deadlock-safe as long as event confirmation precedes
          // Triggering_Event.pull()!
          install {
            Activatom.upon(undetermined_guard, satisfied, not_satisfiable)
          }(start_ms)
          wait_obj synchronized {
            while (!is_non_tx_event_confirmed(satisfied) &&
                   !is_non_tx_event_confirmed(not_satisfiable)) {
              wait_obj.wait(max_ms)
            }
          }
          is_non_tx_event_confirmed(satisfied) ||
          is_non_tx_event_confirmed(not_satisfiable) && {
            throw Would_Deadlock
          }
        }
      }
    } catch {
      case Synchronous.Contradiction => throw Would_Deadlock // unsatisfiable
    }
  }

  /** alternative to `await(guard, max_ms)` that allows specifying `TimeUnit` */
  @throws(classOf[Would_Deadlock])
  final def await(guard: Guard, n: Long, unit: TimeUnit): Boolean =
    await(guard, unit.toMillis(n))


  /** overridable executor factory for user-defined Activity`s */
  protected def create_activity_executor(): ExecutorService =
    Executors.newCachedThreadPool()

  /** overridable executor factory for internal mgmt. of Event`s/Guard`s/etc. */
  protected def create_evaluation_executor(): ExecutorService =
    Executors.newCachedThreadPool()

  /**
   * overridable executor factory for internal mgmt. of Timing_Event`s
   * A dedicated executor ensures execution even against evaluation backlog.
   */
  protected def create_timing_executor(): ScheduledExecutorService =
    Executors.newScheduledThreadPool(num_timing_threads)


  private def elaborate(tx: Transaction) {
    val confirmation = Event_Confirmation(tx) // use for all Event`s of tx
    while (tx.has_more_events_or_else_complete()) {
      val event = tx.take_event()
      event_registry.putIfAbsent(event, confirmation) match {
        // confirmed by another transaction
        case Event_Confirmation(other_tx) =>
          // NOTE: hopefully !(tx < other_tx), but too late to change now!
          if (tx < other_tx)
            log.warn(
                ("yikes! %s found elaborate event [%s] previously confirmed" +
                 " by (younger!) %s").format(tx, event, other_tx))
        case expect: Event_Expectation =>
          fulfill_expectation(event, expect, confirmation)
        case null => // done! (event affirmed before it was ever link`ed)
      }
      event match {
        case trigger: Triggering_Event => trigger.pull()
        case _ => ()
      }
    }
    tx.fulfill_deferred()
  }

  private def fulfill_expectation(
    event: Event,
    expect: Event_Expectation,
    confirm: Event_Confirmation
    ) {
    if (event_registry.replace(event, expect, confirm))
      expect.fulfill(confirm.tx)
    else
      event_registry.get(event) match {
        // event confirmed in meanwhile; notification handled at that time
        case Event_Confirmation(other_tx) =>
          // NOTE: hopefully !(confirm.tx < other_tx), but too late now!
          if (confirm.tx < other_tx)
            log.warn(
                ("yikes! %s found expectation event [%s] previously confirmed" +
                 " by (younger!) %s").format(confirm.tx, event, other_tx))
        case updated_expect: Event_Expectation =>
          // expectation updated in meanwhile; try to fulfill that instead
          fulfill_expectation(event, updated_expect, confirm)
      }
  }

  private def install(a: Activatom)(start_ms: Long = System.currentTimeMillis) {
    async.track(
        a.guard,
        new Activatable(a).observer,
        start_ms)
  }

  // `require_completed_tx` ensures isolation by only considering an event
  // `is_confirmed` once its transaction has completed
  private def is_confirmed(require_completed_tx: Boolean)(ev: Event): Boolean =
    event_registry.get(ev) match {
      case Event_Confirmation(tx) =>
        !require_completed_tx || !tx.has_more_events_or_else_complete()
      case _ => false
    }

  /** @return true iff `event` has been confirmed */
  private def link(event: Event, observer: Event_Observer, at_start_ms: Long):
      Boolean = {
    event_registry.get(event) match {
      case Event_Confirmation(tx) =>
        observer.exists(tx)
        true
      case expect: Event_Expectation =>
        update_expectation(event, Some(expect), observer)
      case null =>
        PartialFunction.condOpt(event) { // conditional handling of Timing`s
          case Timing_Event(ms) => ms
        } foreach { ms =>
          val remaining_ms = ms - (System.currentTimeMillis - at_start_ms)
          if (remaining_ms <= 0) {
            affirm_atomic(event)
          } else {
            timing_executor.schedule(
                affirm_atomic(event),
                remaining_ms, TimeUnit.MILLISECONDS)
          }
        }
        update_expectation(event, None, observer)
    }
  }

  /** @return true iff `event` found to have been confirmed in meanwhile */
  @scala.annotation.tailrec
  private def update_expectation(
    ev: Event,
    curr_expect: Option[Event_Expectation],
    observer: Event_Observer
    ): Boolean =
    curr_expect match {
      case Some(expect) =>
        if (!event_registry.replace(ev, expect, expect.add_observer(observer))){
          event_registry.get(ev) match {
            // event confirmed in meanwhile; handle notification
            case Event_Confirmation(tx) =>
              observer.exists(tx)
              true
            case expect: Event_Expectation =>
              update_expectation(ev, Some(expect), observer)
          }
        }
        else {
          false
        }
      case None =>
        event_registry.putIfAbsent(ev, new Event_Expectation(observer)) match {
          // event confirmed in meanwhile; handle notification
          case Event_Confirmation(tx) =>
            observer.exists(tx)
            true
          case expect: Event_Expectation =>
            update_expectation(ev, Some(expect), observer)
          case null => // successfully added
            false
        }
    }

  private def activate(activities: List[Activity]) {
    activities.foreach { activity =>
      // also unclear, but `expr2Runnable` behaves as intended here (see above)
      activity_executor.execute {
        try   { activity() }
        catch { case e: Throwable => log.warn("(Activity) Throwable: ", e) }
      }
    }
  }


/**
  1. finish conjoined and disjoined statecharts
     !!!!!use the later of two Transaction`s when they both figure in together!!!!!
       ?????how to get around the problem of only being able to have a single transaction, when the other tentative may be the one involved in the crucial atomic affirming??????
  2. add companion object methods to Activarium, which taken an implicit Activarium
  3. implement shutdown/stop
  4. decide whether to be less eager in activating a tentative immediately after transaction close.  ???what if it gets queued and does not run for some time, but in the meanwhile, would have been deemed eternally_false?????  in addition, would there be any guarantee not upheld if it were allowed to become eternally_true, instead of merely proceeding from tentative????
  5. ???what if a Promise is affirmed without a value???
     !!!!must have a type of Loiterable which may be used in a Guard, but is not an Event, and may not be affirmed
  6. create a type in which Event may be wrapped so those holding it may only wait, but not actually affirm it... perhaps this would be sub-type fo Loiterable, which is also not an Event!!!
*/

  private class Activatable private (
    activate_events: List[Event],
    abandon_events: List[Event],
    activities: List[Activity]
    ) {

    def this(a: Activatom) =
      this(a.activate_events, a.abandon_events, a.activities)


    // NOTE: these two check-and-set-oriented vars both require synchronization!
    private[this] var is_determined = false // determination *at most* once
    // the tentative tx (when needed) must remain stable until completion, or
    // this Activatable shall be stoped from going on to activate
    private[this] var tentative_tx: Transaction = Nil_Transaction // sentinel

    final val observer = new Guard_Observer {

      def indelibly_true(tx: Transaction) {
        if (attempt_singular_determination()) do_activation(tx)
      }

      def indelibly_false(tx: Transaction) {
        if (attempt_singular_determination()) do_abandon(tx)
      }

      def tentatively_true(tx: Transaction) {
        if (tx == initial_tx) {
          // no concept of transactionality in case of invocation immediately
          // upon install (viz. due to negation of non-existent event)
          if (attempt_singular_determination()) do_activation(tx)
        } else {
          set_tentative_tx(tx)
          tx.add_deferred(new Deferred {
            def fulfill(tx: Transaction) {
              Activatable.this.attempt_activation(tx)
            }
          })
        }
      }

      def indeterminate(tx: Transaction) {
        clear_tentative_tx()
      }
    }


    // NOTE: synchronized methods below are all invoked from `this.observer`,
    // yet defined here, in order to use `this` as the monitor

    /** true iff determination made (for only time!); false iff already made */
    private def attempt_singular_determination(): Boolean =
      synchronized {
        if (!is_determined) {
          is_determined = true
          true
        } else {
          false
        }
      }

    private def do_activation(tx: Transaction) {
      if (!activate_events.isEmpty) tx.add_events(activate_events)
      activate(activities)
    }

    private def do_abandon(tx: Transaction) {
      if (!abandon_events.isEmpty) tx.add_events(abandon_events)
    }

    private def attempt_activation(orig_tx: Transaction) {
      if (is_valid_tentative_tx(orig_tx))
        do_activation(orig_tx)
    }

    private def is_valid_tentative_tx(tx: Transaction): Boolean =
      synchronized {
        // verify tentative_tx has remained stable with no determination yet
        tentative_tx != Nil_Transaction &&
        tentative_tx.is_ancestor_of(tx) &&
        attempt_singular_determination()
      }

    private def set_tentative_tx(tx: Transaction) =
      synchronized { tentative_tx = tx }

    private def clear_tentative_tx() =
      synchronized { tentative_tx = Nil_Transaction }
  }
}
