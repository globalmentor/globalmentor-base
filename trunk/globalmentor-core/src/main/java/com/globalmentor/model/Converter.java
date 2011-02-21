/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**
 * A class that knows how to convert one type of object to another.
 * @author Garret Wilson
 * @param <I> The input type.
 * @param <O> The output type.
 */
public interface Converter<I, O>
{

	/**
	 * Converts the input to the output.
	 * @param input The input value to convert.
	 * @return The output value.
	 * @throws IllegalArgumentException If there was an error converting the value.
	 */
	public O convert(final I input) throws IllegalArgumentException;

}
