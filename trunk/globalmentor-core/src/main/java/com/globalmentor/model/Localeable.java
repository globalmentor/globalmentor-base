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

package com.globalmentor.model;

import java.util.Locale;

import static com.globalmentor.java.Classes.*;

/**An object that keeps track of a locale.
@author Garret Wilson
*/
public interface Localeable
{

	/**The name of the locale property, if it is bound in any modifiable object.*/
	public final static String LOCALE_PROPERTY_NAME=getPropertyName(Localeable.class, "locale");

	/**@return The locale or <code>null</code>	if no locale is indicated.*/
	public Locale getLocale();

	/**Sets the locale
	@param locale The locale or <code>null</code>	if no locale is indicated.
	*/
	public void setLocale(final Locale locale);

}
