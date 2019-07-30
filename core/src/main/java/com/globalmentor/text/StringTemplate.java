/*
 * Copyright © 1996-2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text;

import static java.util.Objects.*;

/**
 * An efficient class for creating templates which can be repeatedly applied with string arguments. Non-string parameter components will be added to the string
 * using their {@link Object#toString()} methods.
 * <p>
 * Example: <code>new StringTemplate("ex", StringTemplate.STRING_PARAMETER, "le").apply("amp")</code> yields "example".
 * </p>
 * @author Garret Wilson
 * @see java.text.MessageFormat
 */
public class StringTemplate {

	/** The parameter indicating that a string argument should be expected. */
	public static final Parameter STRING_PARAMETER = new Parameter() {};

	/** The array of template components. */
	private final Object[] components;

	/**
	 * Component constructor.
	 * @param components The components that make up the template
	 * @throws NullPointerException if the given components array or one of the components is <code>null</code>.
	 */
	public StringTemplate(final Object... components) {
		this.components = components.clone(); //clone the compnents so that they won't be modified later
		for(final Object component : components) { //for each component
			requireNonNull(component, "Component cannot be null.");
		}
	}

	/**
	 * Applies the given template using the given arguments.
	 * @param arguments The arguments to be used when applying the template.
	 * @return A string indicating the applied template with the given arguments.
	 * @throws NullPointerException if the given arguments array or one of the arguments is <code>null</code>.
	 * @throws ArrayIndexOutOfBoundsException if there are insufficient arguments for the parameters of the given template.
	 * @throws IllegalArgumentException if there are more arguments than parameters of the given template.
	 */
	public String apply(final Object... arguments) {
		int parameterCount = 0; //keep track of the number of parameters used
		int argumentIndex = 0; //start at the first argument
		final StringBuilder stringBuilder = new StringBuilder(); //create a new string builder
		for(final Object component : components) { //for each component
			if(component instanceof Parameter) { //if this is a parameter
				++parameterCount; //we found another parameter
				stringBuilder.append(arguments[argumentIndex]); //append an argument in its place
				++argumentIndex; //advance to the next argument
			} else { //if this is not a parameter
				stringBuilder.append(component); //append this component as-is
			}
		}
		if(argumentIndex < arguments.length) { //if we didn't use all the arguments
			throw new IllegalArgumentException("Too many arguments; " + arguments.length + " provided and only " + parameterCount + " needed.");
		}
		return stringBuilder.toString(); //return the constructed string
	}

	/**
	 * A parameter for a string argument.
	 * @author Garret Wilson
	 */
	public static interface Parameter //TODO later add other types of parameters with more options
	{
	}

}
