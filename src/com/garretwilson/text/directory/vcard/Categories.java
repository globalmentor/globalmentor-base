package com.garretwilson.text.directory.vcard;

import java.util.*;
import com.garretwilson.util.LocaleText;
import com.garretwilson.util.ModifiableSet;

/**A class that maintains a singleton instance of a list of available categories
	that are available to be chosen for the "CATEGORIES" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
<p>Each category is an object of type <code>LocaleText</code>.</p>
@author Garret Wilson
@see LocaleText
*/
public class Categories
{
	
	/**The set of available categories.*/
	private static ModifiableSet availableCategorySet=null;

	/**This class cannot currently be publicly instantiated.*/
	private Categories() {}
	
	/**@return The singleton set of available categories.*/
	public static ModifiableSet getAvailableCategorySet()
	{
		if(availableCategorySet==null)	//if we have not yet created our set of available categories
		{
			availableCategorySet=new ModifiableSet(new HashSet());	//create a new set of categories
			availableCategorySet.add(new LocaleText("Business", Locale.ENGLISH));	//add the default categories G***i18n
			availableCategorySet.add(new LocaleText("Competition", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("Favorite", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("Familiy", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("Friend", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("International", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("Miscellaneous", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("Personal", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("Supplier", Locale.ENGLISH));
			availableCategorySet.add(new LocaleText("VIP", Locale.ENGLISH));
			availableCategorySet.setModified(false);	//show that the categories have not been modified
		}
		return availableCategorySet;	//return the singleton set of categories.
	}
	
}
