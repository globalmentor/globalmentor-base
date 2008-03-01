/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.util;

import java.util.Iterator;

/**An object that can appear in a sequence, able to provide the next object in
	the sequence.
<p>This interface differs from {@link Iterator} in the semantics of the
	"get next" functionality. An {@link Iterator} is an independent object
	that returns different next objects after successive calls. A
	{@link Sequenceable} is an object that is part of a sequence, which will
	always return the successive object after <var>this</var> (which will
	seldom change).</p>
@param <E> The type of element in the sequence.
@author Garret Wilson
@see Iterator
*/
public interface Sequenceable<E>	//TODO rename to Sequence
{

	/**@return <code>true</code> if there is an object after this one in the sequence.*/
	public boolean hasNext();	

	/**@return The next object in the sequence, which need not be constant.*/
	public E getNext();	
	
}
