/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections;

import java.util.*;

import com.globalmentor.model.Modifiable;

/**
 * A set that implements <code>Modifiable</code> so that it can keep track of whether it has been modified.
 * @apiNote This class is meant as a wrapper to an existing set.
 * @param <E> the type of elements maintained by this set.
 * @author Garret Wilson
 * @see Modifiable
 * @deprecated
 */
@Deprecated
public class ModifiableSet<E> extends ModifiableCollection<E> implements Set<E> {

	/**
	 * Set constructor.
	 * @param set The set this set should wrap.
	 */
	public ModifiableSet(final Set<E> set) {
		super(set); //construct the parent class with the set
	}

}
