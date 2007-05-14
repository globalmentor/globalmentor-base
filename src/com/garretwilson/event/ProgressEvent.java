package com.garretwilson.event;

import java.util.EventObject;

/**An event used to notify interested parties that progress has been made.
@author Garret Wilson
@see ProgressListener
*/
public class ProgressEvent extends EventObject
{

	/**+The amount of recent progress, or <code>-1</code> if not known.*/
	private long delta=-1;

		/**@return The amount of recent progress, or <code>-1</code> if not known.*/
		public long getDelta() {return delta;}

	/**The total progress to this point, or <code>-1</code> if not known.*/
	private long value=-1;

		/**@return The total progress to this point, or <code>-1</code> if not known.*/
		public long getValue() {return value;}

	/**The goal, or <code>-1</code> if not known.*/
	private long maximum=-1;

		/**@return The goal, or <code>-1</code> if not known.*/
		public long getMaximum() {return maximum;}

	/**Delta, value, and maximum constructor.
	@param source The object on which the event initially occurred.
	@param delta The amount of recent progress, or <code>-1</code> if not known.
	@param value The total progress to this point, or <code>-1</code> if not known.
	@param maximum The goal, or <code>-1</code> if not known.
	*/
	public ProgressEvent(final Object source, final long delta, final long value, final long maximum)
	{
		super(source);	//let the parent class initialize
		this.delta=delta;
		this.value=value;
		this.maximum=maximum;
	}

}
