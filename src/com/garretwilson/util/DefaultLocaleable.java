package com.garretwilson.util;

import java.util.Locale;

/**A default implementation of an object that keeps track of a locale.
@author Garret Wilson
*/
public class DefaultLocaleable implements Localeable
{
	/**The locale or <code>null</code>	if no locale is indicated.*/
	private Locale locale;
	
		/**@return The locale or <code>null</code>	if no locale is indicated.*/
		public Locale getLocale() {return locale;}
	
		/**Sets the locale
		@param locale The locale or <code>null</code>	if no locale is indicated.
		*/
		public void setLocale(final Locale locale) {this.locale=locale;}

	/**Default constructor.*/
	public DefaultLocaleable()
	{
		this(null);	//construct the object with no locale	
	}
	
	/**Locale constructor.
	@param locale The locale or <code>null</code>	if no locale is indicated.
	*/
	public DefaultLocaleable(final Locale locale)
	{
		this.locale=locale;
	}

}
