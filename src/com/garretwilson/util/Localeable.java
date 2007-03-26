package com.garretwilson.util;

import java.util.Locale;

/**An object that keeps track of a locale.
@author Garret Wilson
*/
public interface Localeable
{

	/**@return The locale or <code>null</code>	if no locale is indicated.*/
	public Locale getLocale();

	/**Sets the locale
	@param locale The locale or <code>null</code>	if no locale is indicated.
	*/
	public void setLocale(final Locale locale);

}
