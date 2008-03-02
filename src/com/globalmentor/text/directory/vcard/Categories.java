package com.globalmentor.text.directory.vcard;

import java.util.*;

import com.globalmentor.util.LocaledText;
import com.globalmentor.util.ModifiableSet;

/**A class that maintains a singleton instance of a list of available categories
	that are available to be chosen for the "CATEGORIES" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
<p>Each category is an object of type <code>LocaleText</code>.</p>
@author Garret Wilson
@see LocaledText
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
			availableCategorySet.add(new LocaledText("Business", Locale.ENGLISH));	//add the default categories G***i18n
			availableCategorySet.add(new LocaledText("Competition", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Favorite", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Familiy", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Friend", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("International", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Miscellaneous", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Personal", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("Supplier", Locale.ENGLISH));
			availableCategorySet.add(new LocaledText("VIP", Locale.ENGLISH));
			availableCategorySet.setModified(false);	//show that the categories have not been modified
		}
		return availableCategorySet;	//return the singleton set of categories.
	}
	
}
