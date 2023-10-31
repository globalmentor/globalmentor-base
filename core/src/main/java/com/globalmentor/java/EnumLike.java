/*
 * Copyright © 2023 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
 * Interface for requiring an object to methods similar to those in {@link Enum}, which is not an interface.
 * @apiNote This interface provides a convenience for other interfaces to restrict the classes implementing them to classes that resemble an {@link Enum}. The
 *          easiest way for a class fulfill the requirements of this interface is to simply be an {@link Enum}.
 * @author Garret Wilson
 */
public interface EnumLike {

	/**
	 * @return The name associated with this object.
	 * @see Enum#name()
	 */
	String name();

	/**
	 * @return The ordinal associated with this object.
	 * @see Enum#ordinal()
	 */
	int ordinal();

}
