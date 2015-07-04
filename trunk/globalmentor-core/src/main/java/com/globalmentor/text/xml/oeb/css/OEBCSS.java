/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.xml.oeb.css;

import java.lang.ref.*;
import java.util.*;

import com.globalmentor.net.ContentType;

import static com.globalmentor.text.css.CSS.*;
import static com.globalmentor.text.xml.oeb.OEB.*;

/**
 * Utilities for working with OEB CSS.
 * @author Garret Wilson
 */
public class OEBCSS {

	/** An OEB 1.0 CSS document. */
	public static final String X_OEB1_CSS_SUBTYPE = ContentType.SUBTYPE_EXTENSION_PREFIX + "oeb1-css";

	/** A reference to a set containing the OEB 1.0 CSS property names. */
	private static Reference<Set<String>> oeb1CSSPropertySetReference = null;

	/**
	 * Retrieves a set containing CSS property names. The set is cached using soft references so that it can be garbage collected if need be.
	 * @return A read-only set containing the OEB 1.0 CSS property names.
	 */
	protected static Set<String> getOEB1CSSPropertySet() {
		//get the set currently being referenced, if we have a reference
		Set<String> oeb1CSSPropertySet = oeb1CSSPropertySetReference != null ? oeb1CSSPropertySetReference.get() : null;
		if(oeb1CSSPropertySet == null) { //if we don't have a set (we never had one, or it's been garbage collected)
			oeb1CSSPropertySet = new HashSet<String>(); //create a new hash set
			oeb1CSSPropertySet.add(CSS_PROP_BACKGROUND_COLOR); //add the supported OEB CSS properties
			oeb1CSSPropertySet.add(CSS_PROP_BORDER);
			oeb1CSSPropertySet.add(CSS_PROP_CLEAR);
			oeb1CSSPropertySet.add(CSS_PROP_COLOR);
			oeb1CSSPropertySet.add(CSS_PROP_DISPLAY);
			oeb1CSSPropertySet.add(CSS_PROP_FLOAT);
			oeb1CSSPropertySet.add(CSS_PROP_FONT_FAMILY);
			oeb1CSSPropertySet.add(CSS_PROP_FONT_SIZE);
			oeb1CSSPropertySet.add(CSS_PROP_FONT_STYLE);
			oeb1CSSPropertySet.add(CSS_PROP_FONT_WEIGHT);
			oeb1CSSPropertySet.add(CSS_PROP_HEIGHT);
			oeb1CSSPropertySet.add(CSS_PROP_LINE_HEIGHT);
			oeb1CSSPropertySet.add(CSS_PROP_LIST_STYLE_TYPE);
			oeb1CSSPropertySet.add(CSS_PROP_MARGIN_BOTTOM);
			oeb1CSSPropertySet.add(CSS_PROP_MARGIN_LEFT);
			oeb1CSSPropertySet.add(CSS_PROP_MARGIN_RIGHT);
			oeb1CSSPropertySet.add(CSS_PROP_MARGIN_TOP);
			oeb1CSSPropertySet.add(CSS_PROP_TEXT_ALIGN);
			oeb1CSSPropertySet.add(CSS_PROP_TEXT_INDENT);
			oeb1CSSPropertySet.add(CSS_PROP_VERTICAL_ALIGN);
			oeb1CSSPropertySet.add(CSS_PROP_WIDTH);
			oeb1CSSPropertySet.add(CSS_PROP_PAGE_BREAK_BEFORE);
			oeb1CSSPropertySet.add(CSS_PROP_PAGE_BREAK_INSIDE);
			oeb1CSSPropertySet.add(CSS_PROP_TEXT_DECORATION);
			oeb1CSSPropertySet.add(OEB_CSS_PROP_OEB_COLUMN_NUMBER);
			oeb1CSSPropertySet = Collections.unmodifiableSet(oeb1CSSPropertySet); //make the set read-only
			oeb1CSSPropertySetReference = new SoftReference<Set<String>>(oeb1CSSPropertySet); //create a soft reference to the property set so that it can be used again
		}
		return oeb1CSSPropertySet; //return the property set we either had already or that we created
	}

	/**
	 * Determines whether the specified property is an OEB 1.0 CSS property.
	 * @param propertyName The name of the property to check.
	 * @return <code>true</code> if the named CSS property is included in OEB 1.0.
	 * @see #getOEB1CSSPropertySet
	 */
	public static boolean isOEB1CSSProperty(final String propertyName) {
		return getOEB1CSSPropertySet().contains(propertyName); //return whether or not the given property name is in our set of OEB 1 CSS property names
	}

}