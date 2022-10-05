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
 * Indicates a class that determines whether a given object will pass through the filter or be filtered out.
 * @author Garret Wilson
 */
public interface Filter<T> {

	/**
	 * Determines whether a given object should pass through the filter or be filtered out.
	 * @param object The object to filter.
	 * @return <code>true</code> if the object should pass through the filter, else <code>false</code> if the object should be filtered out.
	 */
	public boolean isPass(final T object);
}
