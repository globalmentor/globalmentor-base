/*
 * Copyright © 2016 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static java.util.Objects.*;

import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Supplier;

import javax.annotation.*;

/**
 * Supplies sequential string identifiers via {@link #get()}, each guaranteed to be sequentially unique. The identifiers begin with some prefix and end with a
 * sequential number.
 * <p>
 * This implementation makes no allowance for sequential values past the maximum unsigned value stored in a <code>long</code>.
 * </p>
 * <p>
 * This class is thread safe.
 * </p>
 * @author Garret Wilson
 */
public class SequentialdISupplier implements Supplier<String> {

	private final String prefix;

	private final AtomicLong sequenceGenerator = new AtomicLong();

	/** Constructor with no prefix. */
	public SequentialdISupplier() {
		this("");
	}

	/**
	 * Prefix constructor.
	 * @param prefix The prefix to use in the generated identifiers.
	 */
	public SequentialdISupplier(@Nonnull final String prefix) {
		this.prefix = requireNonNull(prefix);
	}

	@Override
	public String get() {
		final String suffix = Long.toUnsignedString(sequenceGenerator.getAndIncrement());
		return prefix.isEmpty() ? suffix : prefix + suffix;
	}

}
