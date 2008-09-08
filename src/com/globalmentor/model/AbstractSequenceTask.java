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

/**Abstract implementation of a task performed in a sequence.
@author Garret Wilson
*/
public abstract class AbstractSequenceTask extends AbstractTask implements SequenceTask
{

	/**Starts the sequence by going to the first step in the sequence.
	This implementation delegates to {@link #start()}, which derived classes should override instead of this method.
	@see #start()
	*/
	public void goStart()
	{
		start();	//go the start
	}
	
	/**Starts the sequence by going to the first step in the sequence.
	This implementation calls {@link #goFirst()}.
	@see #goFirst()
	*/
	protected void start()
	{
		goFirst();	//go to the first step in the sequence
	}

	/**Goes to the first step in the sequence.
	This implementation delegates to {@link #first()}, which derived classes should override instead of this method.
	@see #first()
	*/
	public void goFirst()
	{
		first();	//go the first
	}
	
	/**Goes to the first step in the sequence.*/
	protected abstract void first();

	/**Determines if there is a previous step in the sequence.
	@return <code>true</code> if there is a previous step before the current one.
	*/
	public abstract boolean hasPrevious();

	/**Goes to the previous step in the sequence. If there is no previous component, no action occurs.
	This implementation delegates to {@link #previous()}, which derived classes should override instead of this method.
	@see #hasPrevious()
	@see #previous()
	*/
	public void goPrevious()
	{
		if(hasPrevious())	//if there is a previous step
		{
			previous();	//go the previous step
		}
	}
	
	/**Goes to the previous step in the sequence.
	If there is no previous step, no action occurs.
	*/
	protected abstract void previous();

	/**Determines if there is a next step in the sequence.
	@return <code>true</code> if there is a next step after the current one.
	*/
	public abstract boolean hasNext();

	/**Goes to the next step in the sequence. If there is no next step, no action occurs.
	This implementation delegates to {@link #next()}, which derived classes should override instead of this method.
	@see #hasNext()
	@see #next()
	*/
	public void goNext()
	{
		if(hasNext())  //if there is a next step
		{
			next();	//go the next step
		}
	}

	/**Goes to the next step in the sequence.
	If there is no next step, no action occurs.
	*/
	protected abstract void next();
	
	/**Verifies the contents and finishes the sequence.
	This implementation delegates to {@link #finish()}, which derived classes should override instead of this method.
	@see #finish()
	*/
	public void goFinish()
	{
		finish();	//actually finish
	}
	
	/**Finishes the sequence.
	This version does nothing.
	*/
	protected void finish()
	{
	}

}
