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

/**A task performed in a sequence.
@author Garret Wilson
*/
public interface SequenceTask extends Task
{

	/**Starts the sequence by going to the first step in the sequence.*/
	public void goStart();
	
	/**Goes to the first step in the sequence.*/
	public void goFirst();
	
	/**Determines if there is a previous step in the sequence.
	@return <code>true</code> if there is a previous step before the current one.
	*/
	public boolean hasPrevious();

	/**Goes to the previous step in the sequence. If there is no previous component, no action occurs.
	@see #hasPrevious()
	*/
	public void goPrevious();
	
	/**Determines if there is a next step in the sequence.
	@return <code>true</code> if there is a next step after the current one.
	*/
	public boolean hasNext();

	/**Goes to the next step in the sequence. If there is no next step, no action occurs.
	@see #hasNext()
	*/
	public void goNext();

	/**Verifies the contents and finishes the sequence.*/
	public void goFinish();
	
}
