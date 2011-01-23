/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.model;

import static com.globalmentor.java.Integers.*;

import java.beans.PropertyVetoException;

/**Abstract implementation of a task performed in a sequence.
@author Garret Wilson
*/
public abstract class AbstractSequenceTask extends AbstractTask implements SequenceTask
{

	/**The index in the sequence, or -1 if the sequence has not started.*/
	private int sequenceIndex=-1;

		/**Returns the index in the sequence.
		This is a constrained property of type {@link Integer}.
		@return The index in the sequence, or -1 if the sequence has not started.
		*/
		public int getSequenceIndex() {return sequenceIndex;}

		/**Goes to the indicated index in the sequence.
		This is a bound property of type {@link Integer}.
		@param newSequenceIndex The new index in the sequence.
		@throws IllegalArgumentException if the given sequence index is not within the allowed range.
		@throws PropertyVetoException if the sequence index change has been vetoed.
		@see SequenceTask#SEQUENCE_INDEX_PROPERTY
		@see #getMinSequenceIndex()
		@see #getMaxSequenceIndex()
		*/
		protected void setSequenceIndex(final int newSequenceIndex) throws PropertyVetoException
		{
			final int oldSequenceIndex=sequenceIndex; //get the current index
			if(checkRange(newSequenceIndex, getMinSequenceIndex(), getMaxSequenceIndex())!=oldSequenceIndex)  //if the index is really changing
			{
				fireVetoableChange(SEQUENCE_INDEX_PROPERTY, oldSequenceIndex, newSequenceIndex);	//notify vetoable change listeners of the impending change
				sequenceIndex=newSequenceIndex; //change the index
				firePropertyChange(SEQUENCE_INDEX_PROPERTY, oldSequenceIndex, newSequenceIndex);	//show that the index has changed
			}
		}

		/**Determines the current lowest valid sequence index.
		This method is called to validate the allowed range of the sequence index.
		This version returns 0.
		@return The current lowest valid sequence index, inclusive.
		*/
		protected int getMinSequenceIndex()
		{
			return 0;
		}

		/**Determines the current highest valid sequence index.
		This method is called to validate the allowed range of the sequence index.
		This version returns {@link Integer#MAX_VALUE}.
		@return The current highest valid sequence index, inclusive.
		*/
		protected int getMaxSequenceIndex()
		{
			return Integer.MAX_VALUE;
		}

	/**Starts the sequence annd goes to the first step in the sequence.
	If the sequence is already started, no action occurs.
	This implementation delegates to {@link #start()}, which derived classes should override instead of this method.
	@see #start()
	*/
	public void goStart()
	{
		if(getState()==TaskState.UNSTARTED)	//if we haven't yet started the sequence
		{
			start();	//start
		}
	}

	/**Starts the sequence by going to the first step in the sequence.
	This implementation sets the task state to {@link TaskState#INCOMPLETE}.
	This implementation calls {@link #goFirst()}.
	@throws IllegalStateException if the task has already been started.
	@see #goFirst()
	*/
	protected void start()
	{
		if(getState()!=TaskState.UNSTARTED)	//if we've already started the sequence
		{
		  throw new IllegalStateException("Sequence already started; cannot start sequence in state "+getState());
		}
		setState(TaskState.INCOMPLETE);  //show that we're now interacting
		try
		{
			goFirst();	//go to the first step in the sequence
		}
		catch(final PropertyVetoException propertyVetoException)
		{
			throw new IllegalStateException(propertyVetoException);
		}
	}

	/**Goes to the first step in the sequence.
	This implementation delegates to {@link #first()}, which derived classes should override instead of this method.
	@throws PropertyVetoException if the sequence index change has been vetoed.
	@see #first()
	*/
	public void goFirst() throws PropertyVetoException
	{
		first();	//go the first
	}
	
	/**Goes to the first step in the sequence.
	@throws PropertyVetoException if the sequence index change has been vetoed.
	*/
	protected abstract void first() throws PropertyVetoException;

	/**Determines if there is a previous step in the sequence.
	@return <code>true</code> if there is a previous step before the current one.
	*/
	public abstract boolean hasPrevious();

	/**Goes to the previous step in the sequence. If there is no previous component, no action occurs.
	This implementation delegates to {@link #previous()}, which derived classes should override instead of this method.
	@throws PropertyVetoException if the sequence index change has been vetoed.
	@see #hasPrevious()
	@see #previous()
	*/
	public void goPrevious() throws PropertyVetoException
	{
		if(hasPrevious())	//if there is a previous step
		{
			previous();	//go the previous step
		}
	}
	
	/**Goes to the previous step in the sequence.
	If there is no previous step, no action occurs.
	@throws PropertyVetoException if the sequence index change has been vetoed.
	*/
	protected abstract void previous() throws PropertyVetoException;

	/**Determines if there is a next step in the sequence.
	@return <code>true</code> if there is a next step after the current one.
	*/
	public abstract boolean hasNext();

	/**Goes to the next step in the sequence. If there is no next step, no action occurs.
	This implementation delegates to {@link #next()}, which derived classes should override instead of this method.
	@throws PropertyVetoException if the sequence index change has been vetoed.
	@see #hasNext()
	@see #next()
	*/
	public void goNext() throws PropertyVetoException
	{
		if(hasNext())  //if there is a next step
		{
			next();	//go the next step
		}
	}

	/**Goes to the next step in the sequence.
	If there is no next step, no action occurs.
	@throws PropertyVetoException if the sequence index change has been vetoed.
	*/
	protected abstract void next() throws PropertyVetoException;
	
	/**Verifies the contents and finishes the sequence.
	This implementation delegates to {@link #finish()}, which derived classes should override instead of this method.
	@see #finish()
	*/
	public void goFinish()
	{
		finish();	//actually finish
	}
	
	/**Finishes the sequence.
	This implementation sets the task state to {@link TaskState#INCOMPLETE}.
	*/
	protected void finish()
	{
		setState(TaskState.COMPLETE);  //show that the sequence is complete
	}

}
