/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

var com = com || {}; //create the com.globalmentor.time package
com.globalmentor = com.globalmentor || {};
com.globalmentor.time = com.globalmentor.time || {};

/**
 * A class implementing a timer.
 * 
 * The timer can count up or down. The timer can be started, stopped, and reset. The timer has the following states:
 * <dl>
 * <dt><dfn>active</dfn></dt>
 * <dd>The timer is running.</dd>
 * <dt><dfn>paused</dfn></dt>
 * <dd>The timer is temporarily not running; when started it continue where it left off.</dd>
 * <dt><dfn>stopped</dfn></dt>
 * <dd>The timer is terminally not running, but it has not reached its target. When started it will start from the
 * beginning, effective to being reset.</dd>
 * <dt><dfn>finished</dfn></dt>
 * <dd>The timer is terminally not running because it reached its target. When started it will start from the
 * beginning, effective to being reset.</dd>
 * </dl>
 * 
 * <p>
 * The above states are logical conditions, which can be stored independent of whether the timer is actually executing
 * in memory. A JavaScript timer is maintained internally to actually make the timer run.
 * </p>
 * 
 * <p>
 * Internally the timer keeps track of its value using one of two properties. If the timer is active, the timer's value
 * is calculated from a <dfn>base time</dfn> (the standard milliseconds since the epoch), which allows the elapsed time
 * to be easily calculated at any time, even if the program is suspended. If the timer is not active, its value is
 * stored as the actual milliseconds elapsed since the base time, allowing the timer to be restarted at exactly the time
 * it left off, regardless of the time that has passed during its inactive state.
 * </p>
 * 
 * <p>
 * Loading and saving the timer across time zones is not supported.
 * </p>
 * 
 * @param targetElapsedTime The relative target time, in milliseconds.
 * @param ascending <code>true</code> if the timer should count up, or <code>false</code> if the timer should count
 *          down.
 * @param updateFrequency The number of milliseconds between updates (default 1000).
 * @property id The optional id of the timer; used when saving/loading values.
 * @property onUpdate The function, if any, that is called each time the timer updates (which is usually but not
 *           necessarily associated with a value change).
 * @property onStateChange The function, if any, that is called when the timer's state changes.
 * 
 * Dependencies: javascript.js
 */
com.globalmentor.time.Timer = function(targetElapsedTime, ascending, updateFrequency)
{
	this._targetElapsedTime = targetElapsedTime;
	this._ascending = !!ascending; //make sure we get an actual Boolean value
	this._updateFrequency = updateFrequency || 1000; //default to a one-second update frequency
	this._timerID = null;
	/** The time, in milliseconds past the epoch, at which the timer was first started or reset. */
	this._baseTime = 0;
	/**
	 * The elapsed time, in milliseconds, from the time at which the timer was paused, stopped, or ended, or -1 if the
	 * timer is active.
	 */
	this._elapsedTime = 0;
	/**
	 * Whether the timer is stopped/finished. Stopped and finished can be distinguished by whether the timer's elapsed
	 * time is greater to or equal the target elapsed time.
	 */
	this._terminated = true;
	/** The callback method, if any, to be called when the value is updated. */
	this.onUpdate = null;
	/** The callback method, if any, to be called when the state changes. */
	this.onStateChange = null;

	var proto = com.globalmentor.time.Timer.prototype;
	if(!proto._initialized)
	{
		proto._initialized = true;

		/** Internal callback method, called when the timer updates or when its configuration changes. */
		proto._update = function()
		{
			if(this.isActive()) //if the timer is running
			{
				var elapsedTime = this.getElapsedTime(); //get the current elapsed time
				if(elapsedTime >= this._targetElapsedTime) //if the timer hit its target
				{
					this._elapsedTime = this._targetElapsedTime; //set our elapsed time variable; this will deactivate the timer; use the exact target time in case the timer went over
					this._terminated = true; //reaching the target terminates the timer; starting the timer again will reset it
					this._synchronizeState(); //start or stop a JavaScript timeout as needed
					this._stateChange(); //indicate that the state has changed
				}
			}
			if(this.onUpdate) //if the caller has provided a callback method
			{
				this.onUpdate(); //call the caller's callback method
			}
		};

		/** Internal method, called when the state has changed. */
		proto._stateChange = function()
		{
			if(this.onStateChange) //if the caller has provided a callback method
			{
				this.onStateChange(); //call the caller's callback method
			}
		};

		/**
		 * Calls the update function asynchronously.
		 * @see #_update
		 */
		proto._scheduleUpdate = function()
		{
			setTimeout(this._update.bind(this), 1); //wait a millisecond or so and call the update function
		};

		/**
		 * Calls the state changed function asynchronously.
		 * @see #_stateChange
		 */
		proto._scheduleStateChange = function()
		{
			setTimeout(this._stateChange.bind(this), 1); //wait a millisecond or so and call the state changed function
		};

		/** @return <code>true</code> if the counter is counting up. */
		proto.isAscending = function()
		{
			return this._ascending;
		};

		/** @return <code>true</code> if the timer is not paused, stopped, or finished. */
		proto.isActive = function()
		{
			return this._elapsedTime < 0; //if there is no record of elapsed time, we're running off a base time, which means we are active
		};

		/** @return <code>true</code> if the timer is stopped or finished. */
		proto.isTerminated = function()
		{
			return this._terminated;
		};

		/**
		 * @return <code>true</code> if the timer is stopped, or <code>false</code> if the timer is active, paused, or
		 *         finished.
		 */
		proto.isStopped = function()
		{
			return this._terminated && this.getElapsedTime() < this._targetElapsedTime;
		};

		/** @return <code>true</code> if the timer has reached its target and is no longer active. */
		proto.isFinished = function()
		{
			return this._terminated && this.getElapsedTime() >= this._targetElapsedTime;
		};

		/** @return <code>true</code> if the timer has been paused. */
		proto.isPaused = function()
		{
			return !this.isActive() && !this._terminated; //if the timer isn't active, but isn't terminated, it must just be paused
		};

		/**
		 * Returns the absolute elapsed time of the timer, without regard to whether it is counting up or down. This value
		 * will never be larger than the target elapsed time.
		 * @return The absolute elapsed time of the timer, without regard to whether it is counting up or down.
		 */
		proto.getElapsedTime = function()
		{
			var elapsedTime = this.isActive() ? new Date().getTime() - this._baseTime : this._elapsedTime; //calculate the difference in time since we started, based upon our state
			return Math.min(elapsedTime, this._targetElapsedTime); //don't let the elapsed time be greater than the target
		};

		/** @return The configured target elapsed time as a relative number of milliseconds from the start base time. */
		proto.getTargetElapsedTime = function()
		{
			return this._targetElapsedTime;
		};

		/**
		 * Sets the target elapsed time. This method does not directly change the state of the timer.
		 * @param targetElapsedTime The target elapsed time as a relative number of milliseconds from the start base time.
		 */
		proto.setTargetElapsedTime = function(targetElapsedTime)
		{
			var finished = this.isFinished(); //see if we're finished so we can update our elapsed time appropriately
			this._targetElapsedTime = targetElapsedTime; //update the target elapsed time
			if(finished) //if we were finished
			{
				this._elapsedTime = targetElapsedTime; //update our current elapsed time so we'll still be finished
			}
			this._scheduleStateChange(); //indicate that the state has changed
			this._scheduleUpdate(); //call any callbacks
		};

		/** @return The current value of the timer. */
		proto.getValue = function()
		{
			var elapsedTime = this.getElapsedTime(); //get the absolute time elapsed, whether we are stopped or going
			return this.isAscending() ? elapsedTime : this._targetElapsedTime - elapsedTime; //if we are counting down, subtract from our target time
		};

		/**
		 * Resets the timer. This method does not change the running state of the timer.
		 * @return The new timer value.
		 */
		proto.reset = function(baseTime)
		{
			if(this.isActive()) //if the timer is running
			{
				this._baseTime = new Date().getTime(); //reset its base time to now
			}
			else
			//if the timer is not running
			{
				this._elapsedTime = 0; //reset its elapsed time back to the start
			}
			this._scheduleStateChange(); //indicate that the state has changed
			this._scheduleUpdate(); //call any callbacks
			return this.getValue(); //return the new current value
		};

		/**
		 * Starts the timer if it is stopped or paused. If the timer is stopped/finished, it will first be reset. If the
		 * timer is already started, no action occurs.
		 */
		proto.start = function()
		{
			if(!this.isActive()) //if the timer isn't already active
			{
				var newElapsedTime = this._terminated ? 0 : this._elapsedTime; //if the timer is terminated, reset the elapsed time; otherwise, start where we left off
				this._baseTime = new Date().getTime() - newElapsedTime; //set the base time
				this._elapsedTime = -1; //set the elapsed time to an invalid value; this indicates that we are now active
				this._terminated = false; //the timer is started, so it certainly isn't terminated
				this._synchronizeState(); //start or stop a JavaScript timeout as needed
				this._scheduleStateChange(); //indicate that the state has changed
				this._scheduleUpdate(); //proactively call any callbacks
			}
		};

		/**
		 * Stops the timer. If the timer is already stopped or finished, no action occurs. If only paused, the timer is
		 * fully stopped.
		 */
		proto.stop = function()
		{
			if(!this.isTerminated()) //if the timer is not already stopped or finished
			{
				if(this.isActive()) //if the timer is active
				{
					this._elapsedTime = this.getElapsedTime(); //store the elapsed time locally; providing a non-negative value deactivates the timer
				}
				this._terminated = true; //show that the timer is not only paused, but completely stopped
				this._synchronizeState(); //start or stop a JavaScript timeout as needed
				this._scheduleStateChange(); //indicate that the state has changed
				this._scheduleUpdate(); //proactively call any callbacks
			}
		};

		/** Pauses the timer. If the timer is already paused, no action occurs. */
		proto.pause = function()
		{
			if(this.isActive()) //if the timer is active
			{
				this._elapsedTime = this.getElapsedTime(); //store the elapsed time locally; providing a non-negative value deactivates the timer
				this._synchronizeState(); //start or stop a JavaScript timeout as needed
				this._scheduleStateChange(); //indicate that the state has changed
				this._scheduleUpdate(); //proactively call any callbacks
			}
		};

		/** Starts or stops a JavaScript timeout bring the actual executing state in line with the logical timer state. */
		proto._synchronizeState = function()
		{
			if(this.isActive()) //if the timer is logically active
			{
				if(!this._timerID) //if the timeout isn't active
				{
					this._timerID = setInterval(this._update.bind(this), this._updateFrequency); //start a new timer; bind the timer to the update method to get the correct this
				}
			}
			else
			//if the timer isn't logically active
			{
				if(this._timerID) //if we have a timer running
				{
					clearTimeout(this._timerID); //stop the timer
					this._timerID = null; //clear our timer ID
				}
			}
		};

		/**
		 * Saves the state of the timer as strings in the given map. If a timer ID is defined, it is prepended to each
		 * property in "id-propertyName" format.
		 * @param map The map to receive the state of the timer; if no map is given, a new map will be created.
		 * @returns The map into which properties were stored.
		 */
		proto.save = function(map)
		{
			if(!map) //create a new map if needed
			{
				map = {};
			}
			var prefix = this.id ? this.id + "-" : ""; //use the ID in the property names, if present
			map[prefix + "targetElapsedTime"] = this._targetElapsedTime.toString();
			map[prefix + "ascending"] = this._ascending.toString();
			map[prefix + "updateFrequency"] = this._updateFrequency.toString();
			map[prefix + "baseTime"] = this._baseTime.toString();
			map[prefix + "elapsedTime"] = this._elapsedTime.toString();
			map[prefix + "terminated"] = this._terminated.toString();
			return map;
		};

		/**
		 * Loads the state of the timer from strings in the given map. If this timer's state is not stored in the
		 * properties, no action occurs. If a timer ID is defined, it is prepended to each property in "id-propertyName"
		 * format.
		 * @param map The map with string values representing the new state of the timer.
		 */
		proto.load = function(map)
		{
			var prefix = this.id ? this.id + "-" : ""; //use the ID in the property names, if present
			if(map[prefix + "targetElapsedTime"]) //make sure we have properties stored
			{
				if(this._timerID) //get rid of any timer we may have while we load new values
				{
					clearTimeout(this._timerID);
					this._timerID = null;
				}
				this._targetElapsedTime = Number(map[prefix + "targetElapsedTime"]);
				this._ascending = (map[prefix + "ascending"] == "true");
				this._updateFrequency = Number(map[prefix + "updateFrequency"]);
				this._baseTime = Number(map[prefix + "baseTime"]);
				this._elapsedTime = Number(map[prefix + "elapsedTime"]);
				this._terminated = (map[prefix + "terminated"] == "true");
				this._synchronizeState(); //start or stop a JavaScript timeout as needed
				this._scheduleStateChange(); //indicate that the state has changed
				this._scheduleUpdate(); //proactively call any callbacks
			}
		};
	}

};
