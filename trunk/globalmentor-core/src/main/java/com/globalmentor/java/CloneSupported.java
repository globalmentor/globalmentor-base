/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

/**
 * Indicates that a class implements cloning.
 * 
 * <p>
 * This interface is needed because of the deficiencies of the standard Java <code>Object.clone()</code> framework. This interface makes {@link #clone()} public
 * and guarantees that {@link CloneNotSupportedException} will not be thrown, method signature notwithstanding.
 * </p>
 * 
 * @author Garret Wilson
 * 
 * @see <a href="http://www.artima.com/intv/issues3.html">The clone Dilemma</a>
 */
public interface CloneSupported extends Cloneable
{

	/**
	 * {@inheritDoc} This version is guaranteed not to throw {@link CloneNotSupportedException}.
	 */
	public Object clone() throws CloneNotSupportedException;

}
