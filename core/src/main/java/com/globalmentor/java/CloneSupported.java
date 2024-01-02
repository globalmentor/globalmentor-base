/*
 * Copyright © 2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

/**
 * Indicates that a class implements cloning.
 * @apiNote This interface is needed because of the deficiencies of the standard Java <code>Object.clone()</code> framework. This interface makes
 *          {@link #clone()} public and guarantees via the API contract that {@link CloneNotSupportedException} will not be thrown, method signature
 *          notwithstanding.
 * @author Garret Wilson
 * @see <a href="https://www.artima.com/articles/java-design-issues#part3">Java Design Issues: A Conversation with Ken Arnold, Part VI: The clone Dilemma</a>
 */
public interface CloneSupported extends Cloneable {

	/**
	 * Creates and returns a copy of this object.
	 * @implSpec This version is guaranteed not to throw {@link CloneNotSupportedException}.
	 * @return A clone of this instance.
	 * @throws CloneNotSupportedException if the object's class does not support the {@link Cloneable} interface. As this interface extends {@link Cloneable},
	 *           this exception will never by thrown by the JVM for implementations of this method.
	 */
	public Object clone() throws CloneNotSupportedException;

}
