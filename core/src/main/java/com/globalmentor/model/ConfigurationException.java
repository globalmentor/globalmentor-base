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
 * An unchecked illegal state exception to indicate that the system is not configured correctly for some operation. For example, this exception might be thrown
 * if no appropriate parser is available or a particular character encoding is not supported.
 * @author Garret Wilson
 * @deprecated to be removed in favor of {@link ConfiguredStateException} or Confound's <code>ConfigurationException</code>, depending on the context.
 */
@Deprecated
public class ConfigurationException extends IllegalStateException {

	private static final long serialVersionUID = 1L;

	/**
	 * Message constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 */
	public ConfigurationException(final String message) {
		this(message, null); //construct the class with no cause
	}

	/**
	 * Cause constructor.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public ConfigurationException(final Throwable cause) {
		this(null, cause); //construct the class with no message
	}

	/**
	 * Message and cause constructor.
	 * @param message An explanation of why the input could not be parsed, or <code>null</code> if a default message should be used.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public ConfigurationException(final String message, final Throwable cause) {
		super(message, cause); //construct the class
	}

}
