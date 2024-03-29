/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.time;

import static java.util.Objects.*;

import java.time.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import javax.annotation.*;

/**
 * Tracks time that continually elapses to a certain point and then resets to start elapsing again. Detecting the recurrence requires polling the instance—this
 * lightweight class, although safe to be called by multiple threads, itself uses no additional threads and does not emit events concurrently.
 * <p>
 * Because this class performs no concurrent processing, detecting that a recurrence has passed—and indeed, invoking the recurrence to be registered—is
 * performed by a query to this class, e.g. {@link #getElapsingTime()} or {@link #pollRecurred()}. Each of the queries that will can register an recurrence will
 * "claim" the recurrence, causing the recurrence not to be detected directly by the other threads. In other words, if one thread calls
 * {@link #getElapsingTime()} after a recurrence duration, the returned time will reflect the new elapsing time since the new recurrence, but another thread
 * calling {@link #pollRecurred()} will not see that a recurrence has passed because it was "claimed" by the first thread calling {@link #getElapsingTime()}.
 * </p>
 * @apiNote A typical use case is to detect, in the middle of some processing, whether some time repeating interval has passed so that some other action may
 *          take place—logging a status update every second, for example. <pre><code>RecurringTime logStatusTime = RecurringTime.fromNowByInterval(1, SECONDS);
 * for(Bar bar:foo.bars()) { //there are a lot of bars; this may take a while
 *   process(bar);
 *   if(logStatusTime.pollRecurred()) {
 *     getLogger().atInfo().log("... still processing ...");
 *   }
 * }</code></pre>
 * @implSpec By default this class uses {@link System#nanoTime()} as its time supplier, but any time supplier can be used with any {@link TimeUnit}.
 * @implNote This class is a think wrapper around {@link ElapsingTime}. An instance uses no CPU resources when its methods are not called, and only performs
 *           calculations when queried, without blocking using <code>synchronize</code>..
 * @author Garret Wilson
 * @see <a href="https://www.writingtips.cc/intermittent-vs-recurrent-vs-periodic-vs-alternate/">Intermittent vs Recurrent vs Periodic vs Alternate</a>
 */
public final class RecurringTime {

	/** The reference to the elapsing time since the last marked recurrence. */
	private AtomicReference<ElapsingTime> elapsingTimeReference;

	private final RecurStrategy recurStrategy;

	/**
	 * Elapsing time and recur strategy constructor.
	 * @apiNote This constructor is provided for highly customized use cases. More common use cases are covered by static factory methods such as
	 *          {@link #fromNowByInterval(Duration)} and {@link #fromNowByPeriod(Duration)}.
	 * @param elapsingTime The already elapsing time that should recur.
	 * @param recurStrategy The strategy for determining when and how to recur.
	 */
	public RecurringTime(@Nonnull final ElapsingTime elapsingTime, @Nonnull final RecurStrategy recurStrategy) {
		this.elapsingTimeReference = new AtomicReference<ElapsingTime>(requireNonNull(elapsingTime));
		this.recurStrategy = requireNonNull(recurStrategy);
	}

	/**
	 * Produces a recurring time that registers each next recurrence exactly one interval later than each current registration.
	 * @apiNote The produced recurring time is sensitive to the time between polling, and may not result in equally-spaced intervals.
	 * @implSpec This is a convenience method that delegates to {@link #fromNowByInterval(Duration)}.
	 * @param interval At each recurrence, the amount of the interval to add to determine the time of the next recurrence.
	 * @param timeUnit The unit the interval is measured in.
	 * @return A new interval-based recurring time.
	 * @throws DateTimeException if the period unit has an estimated duration
	 * @throws ArithmeticException if a numeric overflow occurs
	 * 
	 */
	public static RecurringTime fromNowByInterval(final long interval, @Nonnull final TimeUnit timeUnit) {
		return fromNowByInterval(Duration.of(interval, timeUnit.toChronoUnit()));
	}

	/**
	 * Produces a recurring time that registers each next recurrence exactly one interval later than each current registration.
	 * @apiNote The produced recurring time is sensitive to the time between polling, and may not result in equally-spaced intervals.
	 * @implSpec This implementation delegates to {@link #fromElapsingByInterval(ElapsingTime, Duration)}.
	 * @param interval At each recurrence, the interval to add to determine the time of the next recurrence.
	 * @return A new interval-based recurring time.
	 */
	public static RecurringTime fromNowByInterval(@Nonnull final Duration interval) {
		return fromElapsingByInterval(ElapsingTime.fromNow(), interval);
	}

	/**
	 * Produces a recurring time that registers each next recurrence exactly one interval later than each current registration.
	 * @apiNote The produced recurring time is sensitive to the time between polling, and may not result in equally-spaced intervals.
	 * @implSpec This implementation delegates to {@link #fromElapsingByInterval(ElapsingTime, long)}.
	 * @param elapsingTime The already elapsing time that should recur.
	 * @param interval At each recurrence, the interval to add to determine the time of the next recurrence.
	 * @return A new interval-based recurring time.
	 */
	public static RecurringTime fromElapsingByInterval(@Nonnull final ElapsingTime elapsingTime, @Nonnull final Duration interval) {
		return fromElapsingByInterval(elapsingTime, elapsingTime.convertToTimeUnit(interval));
	}

	/**
	 * Produces a recurring time that registers each next recurrence exactly one interval later than each current registration.
	 * @apiNote The produced recurring time is sensitive to the time between polling, and may not result in equally-spaced intervals.
	 * @implSpec This implementation uses the strategy produced by {@link AbstractElapsedTimeRecurStrategy#byInterval(long)}.
	 * @param elapsingTime The already elapsing time that should recur.
	 * @param interval At each recurrence, the amount of the interval to add to determine the time of the next recurrence, using the time unit of the elapsing
	 *          time.
	 * @return A new interval-based recurring time.
	 * @throws DateTimeException if the period unit has an estimated duration
	 * @throws ArithmeticException if a numeric overflow occurs
	 * @see ElapsingTime#getTimeUnit()
	 */
	static RecurringTime fromElapsingByInterval(@Nonnull final ElapsingTime elapsingTime, final long interval) {
		return new RecurringTime(elapsingTime, AbstractElapsedTimeRecurStrategy.byInterval(interval));
	}

	/**
	 * Produces a recurring time that registers all recurrences at equal time durations, like a periodic function.
	 * @apiNote Here the use of term "period" refers to evenly spaced time points, not the calendar time class of the same name. The produced recurring time is
	 *          not sensitive to the time between polling; if polling is delayed beyond the end of period, the next recurrence duration will be shortened so that
	 *          the next recurrence registered (assuming it is polled in time) exactly one period from the time the last recurrence <em>should</em> have been
	 *          registered.
	 * @implSpec This is a convenience method that delegates to {@link #fromNowByPeriod(Duration)}.
	 * @param period The evenly-spaced periods between recurrences.
	 * @param timeUnit The unit the period is measured in.
	 * @return A new period-based recur recurring time.
	 * @throws DateTimeException if the period unit has an estimated duration
	 * @throws ArithmeticException if a numeric overflow occurs
	 */
	public static RecurringTime fromNowByPeriod(final long period, @Nonnull final TimeUnit timeUnit) {
		return fromNowByPeriod(Duration.of(period, timeUnit.toChronoUnit()));
	}

	/**
	 * Produces a recurring time that registers all recurrences at equal time durations, like a periodic function.
	 * @apiNote Here the use of term "period" refers to evenly spaced time points, not the calendar time class of the same name. The produced recurring time is
	 *          not sensitive to the time between polling; if polling is delayed beyond the end of period, the next recurrence duration will be shortened so that
	 *          the next recurrence registered (assuming it is polled in time) exactly one period from the time the last recurrence <em>should</em> have been
	 *          registered.
	 * @implSpec This implementation delegates to {@link #fromElapsingByPeriod(ElapsingTime, Duration)}.
	 * @param period The evenly-spaced periods between recurrences.
	 * @return A new period-based recur recurring time.
	 */
	public static RecurringTime fromNowByPeriod(@Nonnull final Duration period) {
		return fromElapsingByPeriod(ElapsingTime.fromNow(), period);
	}

	/**
	 * Produces a recurring time that registers all recurrences at equal time durations, like a periodic function.
	 * @apiNote Here the use of term "period" refers to evenly spaced time points, not the calendar time class of the same name. The produced recurring time is
	 *          not sensitive to the time between polling; if polling is delayed beyond the end of period, the next recurrence duration will be shortened so that
	 *          the next recurrence registered (assuming it is polled in time) exactly one period from the time the last recurrence <em>should</em> have been
	 *          registered.
	 * @implSpec This implementation delegates to {@link #fromElapsingByPeriod(ElapsingTime, long)}.
	 * @param elapsingTime The already elapsing time that should recur.
	 * @param period The evenly-spaced periods between recurrences.
	 * @return A new period-based recur recurring time.
	 */
	public static RecurringTime fromElapsingByPeriod(@Nonnull final ElapsingTime elapsingTime, @Nonnull final Duration period) {
		return fromElapsingByPeriod(elapsingTime, elapsingTime.convertToTimeUnit(period));
	}

	/**
	 * Produces a recurring time that registers all recurrences at equal time durations, like a periodic function.
	 * @apiNote Here the use of term "period" refers to evenly spaced time points, not the calendar time class of the same name. The produced recurring time is
	 *          not sensitive to the time between polling; if polling is delayed beyond the end of period, the next recurrence duration will be shortened so that
	 *          the next recurrence registered (assuming it is polled in time) exactly one period from the time the last recurrence <em>should</em> have been
	 *          registered.
	 * @implSpec This implementation uses the strategy produced by {@link AbstractElapsedTimeRecurStrategy#byPeriod(long)}.
	 * @param elapsingTime The already elapsing time that should recur.
	 * @param period The evenly-spaced periods between recurrences, using the time unit of the elapsing time.
	 * @return A new period-based recur recurring time.
	 * @throws DateTimeException if the period unit has an estimated duration
	 * @throws ArithmeticException if a numeric overflow occurs
	 * @see ElapsingTime#getTimeUnit()
	 */
	static RecurringTime fromElapsingByPeriod(@Nonnull final ElapsingTime elapsingTime, final long period) {
		return new RecurringTime(elapsingTime, AbstractElapsedTimeRecurStrategy.byPeriod(period));
	}

	/**
	 * Returns the elapsing time since the last recurrence. Because of the nature of elapsing time, the returned value may have passed the recurrence interval by
	 * the time it is checked. Likewise the subsequent calls to this method will return different elapsing time instance if the recurrence interval passes. A
	 * given return elapsing time will continue to show time elapsed from the same occurrence, so this method should be called again rather than reusing the
	 * previous instance if determining the elapsed time from the last recurrence is desired.
	 * @apiNote This thread registers recurrences. One thread calling this method may "claim" the recurrence, preventing another thread from seeing it using
	 *          {@link #pollRecurred()}.
	 * @implSpec This implementation is guaranteed to return the latest elapsing time since the last recurrence, even if recurrence is claimed by another thread,
	 *           unless the interval time is shorter than the time it takes to process this method.
	 * @return The continual elapsing time since the last recurrence
	 */
	public ElapsingTime getElapsingTime() {
		final ElapsingTime nullableRecurredElapsingTime = updateRecurrence();
		if(nullableRecurredElapsingTime != null) { //if the time elapsed
			return nullableRecurredElapsingTime; //the recurred elapsing time is the latest
		}
		return elapsingTimeReference.get(); //no recurrence, so return the latest elapsing time
	}

	/**
	 * Polls to determine if the elapsing time has recurred. Once the elapsing time reaches the appropriate time to recur, this method will return
	 * <code>true</code> a single time, and henceforth the elapsing time will be relative to this new recurrence. If this method is not called
	 * <p>
	 * For example, with a periodic recurrence of one second, repeatedly calling this method will result in a return value of <code>true</code> once per second.
	 * If polling is delayed, all the past recurrences will be registered a single time by a single result of <code>true</code> once from this method until the
	 * subsequent recurrence.
	 * </p>
	 * @apiNote This thread registers recurrences. One thread calling this method or {@link #getElapsingTime()} method may "claim" the recurrence, preventing
	 *          another thread from seeing it.
	 * @return <code>true</code> if one or more recurrences have been reached or have past since the last registered recurrence.
	 */
	public boolean pollRecurred() {
		return updateRecurrence() != null; //update the recurrence and report whether recurrence occurred
	}

	/**
	 * Updates the elapsing time, recurring and updating the elapsing time if needed.
	 * @apiNote To maintain consistency, no other method must update the elapsing time reference.
	 * @implSpec This method uses the recurring time's {@link RecurStrategy} to determine when and how to recur.
	 * @implNote This implementation is atomic and thread-safe.
	 * @return The new elapsing time if past the interval, or <code>null</code> if the current elapsing time is still valid.
	 */
	private ElapsingTime updateRecurrence() {
		final ElapsingTime oldElapsingTime = elapsingTimeReference.get();
		if(recurStrategy.shouldRecur(oldElapsingTime)) {
			final ElapsingTime newElapsingTime = recurStrategy.recur(oldElapsingTime);
			if(elapsingTimeReference.compareAndSet(oldElapsingTime, newElapsingTime)) { //update the elapsing time, making sure there were no concurrent updates
				return newElapsingTime;
			}
		}
		return null; //the time has not recurred or another thread claimed the recurrence
	}

	/**
	 * A strategy for determining when and how recurring time should recur.
	 * @author Garret Wilson
	 */
	public interface RecurStrategy {

		/**
		 * Indicates if the elapsing time has reached the point at which it should recur. Note that this method must not actually cause any recurrence to take
		 * place.
		 * @param elapsingTime The current elapsing time since the last registered recurrence.
		 * @return <code>true</code> if sufficient time has elapsed so that recurrence should take place.
		 */
		public boolean shouldRecur(@Nonnull ElapsingTime elapsingTime);

		/**
		 * Registers a recurrence. The new elapsing time will be relative to this recurrence registration, but specifically what the new elapsing time will be will
		 * depend upon the rules implemented by this strategy.
		 * @param elapsingTime The current elapsing time since the last registered recurrence.
		 * @return The new elapsing time relative to this registration of a recurrence.
		 */
		public ElapsingTime recur(@Nonnull ElapsingTime elapsingTime);

	}

	/**
	 * An abstract implementation of a recur strategy based upon some elapsed time between recurrences, in the time unit of the elapsing time.
	 * @apiNote Beware that this strategy uses an absolute elapsed time comparison, which is assumed to be in the same unit of the elapsing time. Thus this
	 *          strategy may be used with multiple elapsing time instances, but if those instances have distinct time units, the effective absolute time durations
	 *          will be different.
	 * @author Garret Wilson
	 * @see ElapsingTime#getElapsedTime()
	 */
	static abstract class AbstractElapsedTimeRecurStrategy implements RecurStrategy {

		private final long elapsedTime;

		/**
		 * @return The elapsed time for calculating time between recurrences, in the time unit of the elapsing time.
		 * @see ElapsingTime#getElapsedTime()
		 */
		protected long getElapsedTime() {
			return elapsedTime;
		}

		/**
		 * Elapsed time constructor.
		 * @param elapsedTime The elapsed time to be used to calculate time between recurrences, in the time unit of the elapsing time.
		 * @see ElapsingTime#getElapsedTime()
		 */
		protected AbstractElapsedTimeRecurStrategy(final long elapsedTime) {
			this.elapsedTime = elapsedTime;
		}

		@Override
		public boolean shouldRecur(final ElapsingTime elapsingTime) {
			return elapsingTime.getElapsedTime() >= getElapsedTime(); //determine if sufficient time has passed
		}

		/**
		 * Simple recur strategy that schedules the next recurrence exactly one interval later than the current elapsing time.
		 * @apiNote This strategy produces recurrences that are sensitive to the time between polling, and may not result in equally-spaced intervals.
		 * @param interval At each recurrence, the interval to add to determine the time of the next recurrence, in terms of the recurring time unit.
		 * @return A new interval-based recur strategy.
		 */
		static RecurStrategy byInterval(final long interval) {
			return new AbstractElapsedTimeRecurStrategy(interval) {
				@Override
				public ElapsingTime recur(final ElapsingTime elapsingTime) {
					return elapsingTime.withElapsingFromNow();
				}
			};
		}

		/**
		 * Recur strategy that schedules all recurrences to occur equidistant, like a periodic function.
		 * @apiNote Here the use of term "period" refers to evenly spaced time points, not the calendar time class of the same name. This strategy produces
		 *          recurrences that are not sensitive to the time between polling; if polling is delayed beyond the end of period, the next recurrence duration
		 *          will be shortened so that the next recurrence registered (assuming it is polled in time) exactly one period from the time the last recurrence
		 *          <em>should</em> have been registered.
		 * @param period The evenly-spaced periods between recurrences, in terms of the recurring time unit.
		 * @return A new period-based recur strategy.
		 */
		static RecurStrategy byPeriod(final long period) {
			return new AbstractElapsedTimeRecurStrategy(period) {
				@Override
				public ElapsingTime recur(final ElapsingTime elapsingTime) {
					final long totalElapsedTime = elapsingTime.getElapsedTime();
					final long periodElapsedTime = totalElapsedTime % period; //see how much time has elapsed after the end of the last period
					return elapsingTime.withElapsed(periodElapsedTime);
				}
			};
		}

	}

}
