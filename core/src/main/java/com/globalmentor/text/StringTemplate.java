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

import static com.globalmentor.java.Conditions.*;
import static java.util.Objects.*;

import java.util.*;

import javax.annotation.*;

/**
 * An efficient class for creating templates which can be repeatedly applied with string arguments. Non-string parameter components will be added to the string
 * using their {@link Object#toString()} methods.
 * <p>
 * Example: <code>new StringTemplate("ex", StringTemplate.STRING_PARAMETER, "le").apply("amp")</code> yields "example".
 * </p>
 * @author Garret Wilson
 * @see String#format(String, Object...)
 * @see java.text.MessageFormat
 */
public final class StringTemplate {

	/** The parameter indicating that a string argument should be expected. */
	public static final Parameter STRING_PARAMETER = new Parameter() {};

	/**
	 * A generator for a newline appropriate for this system.
	 * @see System#lineSeparator()
	 */
	public static final Generator NEWLINE = new Generator() {};

	/** The array of template components. */
	private final Object[] components;

	/**
	 * Component constructor.
	 * @param components The components that make up the template; any {@link Replacement} instance will result in replaced content, while any other object will
	 *          be included verbatim using {@link Object#toString()} when the template is applied.
	 * @throws NullPointerException if the given components array or one of the components is <code>null</code>.
	 * @deprecated in favor of the static factory method {@link #of(Object...)}.
	 */
	@Deprecated
	public StringTemplate(final Object... components) {
		this.components = components.clone(); //clone the components so that they won't be modified later
		for(final Object component : components) { //for each component
			requireNonNull(component, "Component cannot be null.");
		}
	}

	/**
	 * Creates a string template from the given components.
	 * @param components The components that make up the template; any {@link Replacement} instance will result in replaced content, while any other object will
	 *          be included verbatim using {@link Object#toString()} when the template is applied.
	 * @return A new string template from the given components.
	 * @throws NullPointerException if the given components array or one of the components is <code>null</code>.
	 */
	public static StringTemplate of(final Object... components) {
		return new StringTemplate(components);
	}

	/**
	 * Builder factory.
	 * @return A new builder for a string template.
	 */
	public static Builder builder() {
		return new Builder();
	}

	/**
	 * Applies the given template using the given arguments.
	 * @param arguments The arguments to be used when applying the template.
	 * @return A string indicating the applied template with the given arguments.
	 * @throws NullPointerException if the given arguments array or one of the arguments is <code>null</code>.
	 * @throws ArrayIndexOutOfBoundsException if there are insufficient arguments for the parameters of the given template.
	 * @throws IllegalArgumentException if there are too few arguments or more arguments than parameters of the given template.
	 */
	public String apply(final Object... arguments) {
		int parameterCount = 0; //keep track of the number of parameters used
		int argumentIndex = 0; //start at the first argument
		final StringBuilder stringBuilder = new StringBuilder(); //create a new string builder
		for(final Object component : components) { //for each component
			if(component instanceof Replacement) {
				if(component instanceof Parameter) { //if this is a parameter
					if(component == STRING_PARAMETER) {
						++parameterCount; //we found another parameter
						checkArgument(argumentIndex < arguments.length, "Not enough many arguments; %s needed and only %s provided.", parameterCount, arguments.length);
						stringBuilder.append(arguments[argumentIndex]); //append an argument in its place
						++argumentIndex; //advance to the next argument
					} else {
						throw new IllegalStateException(String.format("Unknown parameter %s.", component));
					}
				} else if(component instanceof Generator) {
					if(component == NEWLINE) {
						stringBuilder.append(System.lineSeparator());
					} else {
						throw new IllegalStateException(String.format("Unknown generator %s.", component));
					}
				}
			} else { //if this is not a replacement component
				stringBuilder.append(component); //append this component as-is
			}
		}
		checkArgument(argumentIndex == arguments.length, "Too many arguments; %s provided and only %s needed.", arguments.length, parameterCount);
		return stringBuilder.toString();
	}

	/**
	 * Builder for a string template.
	 * @author Garret Wilson
	 */
	public static class Builder {

		private final List<Object> components = new ArrayList<>();

		/**
		 * This class cannot be publicly instantiated.
		 * @see StringTemplate#builder()
		 */
		private Builder() {
		}

		/**
		 * Adds literal text to the template.
		 * @param text The literal text to append.
		 * @return A reference to this builder.
		 */
		public Builder text(@Nonnull final String text) {
			object(text);
			return this;
		}

		/**
		 * Adds some object to the template. The object's {@link Object#toString()} method will be used to generate the content.
		 * @param object The object to append.
		 * @return A reference to this builder.
		 */
		public Builder object(@Nonnull final Object object) {
			components.add(requireNonNull(object));
			return this;
		}

		/**
		 * Adds a parameter to the template.
		 * @param parameter The type of parameter to append.
		 * @return A reference to this builder.
		 */
		public Builder parameter(@Nonnull final Parameter parameter) {
			components.add(requireNonNull(parameter));
			return this;
		}

		/**
		 * Appends a system newline generator.
		 * @return A reference to this builder.
		 * @see System#lineSeparator()
		 */
		public Builder newline() {
			components.add(NEWLINE);
			return this;
		}

		/**
		 * Builds the string template from the supplied components.
		 * @return The new string template.
		 */
		public StringTemplate build() {
			return new StringTemplate(components.toArray());
		}
	}

	/**
	 * A component to be replaced in a template.
	 * @author Garret Wilson
	 */
	public static interface Replacement {
	}

	/**
	 * A parameter to be replaced by an argument.
	 * @author Garret Wilson
	 */
	public static interface Parameter extends Replacement { //TODO later add other types of parameters with more options
	}

	/**
	 * Content to be generated.
	 * @author Garret Wilson
	 */
	public static interface Generator extends Replacement {
	}

}
