/*
 * Copyright © 2020 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

import javax.annotation.*;

/**
 * Abstract base class for a text comparator that uses a {@link Collator} for comparison.
 * @implSpec This implementation does not support <code>null</code>s.
 * @author Garret Wilson
 */
public class AbstractCollatingComparator implements Comparator<CharSequence> {

	private final Collator collator;

	/** @return The collator being used by this comparator. */
	public Collator getCollator() {
		return collator;
	}

	/**
	 * Locale constructor.
	 * @implSpec This implementation uses a collator that takes into account differences in case and accents.
	 * @param locale The locale to use for comparison.
	 */
	protected AbstractCollatingComparator(@Nonnull final Locale locale) {
		this(createCollator(locale));
	}

	/**
	 * Collator constructor.
	 * @param collator The collator to use for comparisons.
	 */
	protected AbstractCollatingComparator(@Nonnull final Collator collator) {
		this.collator = requireNonNull(collator);
	}

	@Override
	public int compare(@Nonnull final CharSequence charSequence1, @Nonnull final CharSequence charSequence2) {
		return getCollator().compare(charSequence1.toString(), charSequence2.toString());
	}

	/**
	 * Collator factory.
	 * @implSpec This implementation returns a collator that takes into account accents and case.
	 * @param locale The locale to use for comparison.
	 * @return A collator for the indicated locale.
	 */
	public static Collator createCollator(@Nonnull final Locale locale) {
		final Collator collator = Collator.getInstance(locale);
		collator.setDecomposition(Collator.CANONICAL_DECOMPOSITION);
		collator.setStrength(Collator.TERTIARY); //take into account accents and case
		return collator;
	}

}
