/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory.vcard;

import java.util.*;

import com.globalmentor.collections.ModifiableSet;
import com.globalmentor.model.LocaledText;

/**
 * A class that maintains a singleton instance of a list of available categories that are available to be chosen for the "CATEGORIES" type of a vCard
 * <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>, "vCard MIME Directory Profile".
 * <p>
 * Each category is an object of type <code>LocaleText</code>.
 * </p>
 * @author Garret Wilson
 * @see LocaledText
 */
@Deprecated
public class Categories {

	/** The set of available categories. */
	private static ModifiableSet availableCategorySet = null;

	/** This class cannot currently be publicly instantiated. */
	private Categories() {
	}

	/** @return The singleton set of available categories. */
	public static ModifiableSet getAvailableCategorySet() {
		if(availableCategorySet == null) { //if we have not yet created our set of available categories
			availableCategorySet = new ModifiableSet(new HashSet()); //create a new set of categories
			availableCategorySet.add(new LocaledText("Business", Locale.ENGLISH)); //add the default categories TODO i18n
			availableCategorySet.add(new LocaledText("Competition", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Favorite", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Familiy", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Friend", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("International", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Miscellaneous", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Personal", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Supplier", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("VIP", Locale.ENGLISH));
			availableCategorySet.setModified(false); //show that the categories have not been modified
		}
		return availableCategorySet; //return the singleton set of categories.
	}

}
