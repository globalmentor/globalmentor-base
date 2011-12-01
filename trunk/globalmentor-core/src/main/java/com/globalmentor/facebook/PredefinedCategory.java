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

package com.globalmentor.facebook;

import static com.globalmentor.java.Objects.*;

import com.globalmentor.model.Labeled;

/**
 * A predefined category of Open Graph type.
 * @author Garret Wilson
 * @see <a href="http://ogp.me/">The Open Graph Protocol</a>
 */
public enum PredefinedCategory implements Labeled
{
	ACTIVITIES("Activities"), BUSINESSES("Businesses"), GROUPS("Groups"), ORGANIZATIONS("Organizations"), PEOPLE("People"), PLACES("Places"), PRODUCTS_ENTERTAINMENT(
			"Products and Entertainment"), WEBSITES("Websites");

	private final CharSequence label;

	/** {@inheritDoc} */
	public CharSequence getLabel()
	{
		return label;
	}

	PredefinedCategory(final CharSequence label)
	{
		this.label = checkInstance(label);
	}

}
