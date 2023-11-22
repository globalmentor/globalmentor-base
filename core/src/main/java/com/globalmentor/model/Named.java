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

package com.globalmentor.model;

/**
 * An object that has a name.
 * <p>
 * A name is considered to be semantically similar to an id, which may or may not be for human consumption, but has weaker unique constraints.
 * </p>
 * @param <N> The type of name.
 * @author Garret Wilson
 * @see Labeled
 */
public interface Named<N> {

	/**
	 * Returns the name of the object.
	 * @return The name of the object, or <code>null</code> if the object has no name.
	 */
	public N getName();

}
