/*
 * Copyright © 1996-2022 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import org.jspecify.annotations.*;

/**
 * An unchecked illegal state exception to indicate that the system is not configured correctly for some operation. For example, this exception might be thrown
 * if no appropriate parser is available or a particular character encoding is not supported.
 * @apiNote This exception is a low-level system/JVM configuration-related exception which does not rely on any higher-level configuration framework, although
 *          such a framework might use this exception as a base for some higher layer to indicate a missing configuration key for example.
 * @author Garret Wilson
 */
public class ConfiguredStateException extends IllegalStateException {

	private static final long serialVersionUID = 1L;

	/** No-argument constructor. */
	public ConfiguredStateException() {
		this((String)null);
	}

	/**
	 * Message constructor.
	 * @param message An explanation of the problem, or <code>null</code> if a default message should be used.
	 */
	public ConfiguredStateException(@Nullable final String message) {
		this(message, null);
	}

	/**
	 * Cause constructor. The message of the cause will be used if available.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public ConfiguredStateException(@Nullable final Throwable cause) {
		this(cause == null ? null : cause.toString(), cause);
	}

	/**
	 * Message and cause constructor.
	 * @param message An explanation of the problem, or <code>null</code> if a default message should be used.
	 * @param cause The cause error or <code>null</code> if the cause is nonexistent or unknown.
	 */
	public ConfiguredStateException(@Nullable final String message, @Nullable final Throwable cause) {
		super(message, cause); //construct the class
	}

}
