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

package com.globalmentor.model;

import static java.util.Objects.*;

import javax.annotation.*;

import com.globalmentor.text.ASCII;

/**
 * Representation of a language tag as per <cite>RFC 4646</cite>.
 * @author Garret Wilson
 * @see <a href="https://datatracker.ietf.org/doc/html/rfc4646">RFC 4646: Tags for Identifying Languages</a>
 */
public final class LanguageTag {

	/** The string form of the entire language tag. */
	private final String tag;

	/**
	 * Constructor.
	 * @param tag The string form of the entire language tag.
	 */
	private LanguageTag(@Nonnull final String tag) {
		this.tag = requireNonNull(tag);
	}

	/**
	 * Returns a language tag from the text of the tag.
	 * @param text The text of a language tag.
	 * @return A new or existing language tag representing the given text.
	 */
	public static LanguageTag of(@Nonnull final CharSequence text) {
		return new LanguageTag(text.toString());
	}

	@Override
	public int hashCode() {
		return ASCII.hashCodeIgnoreCase(tag);
	}

	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof LanguageTag)) {
			return false;
		}
		return ASCII.equalsIgnoreCase(tag, ((LanguageTag)object).tag);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This version returns the language tag text.
	 */
	@Override
	public String toString() {
		return tag;
	}
}
