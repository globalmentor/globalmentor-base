package com.garretwilson.text.directory.vcard;

import java.util.*;
import com.garretwilson.util.ModifiableSet;

/**A class that maintains a singleton instance of a list of available categories
	that are available to be chosen for the "CATEGORIES" type of a vCard <code>text/directory</code>
	profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
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
			availableCategorySet.add("Business");	//add the default categories G***i18n
			availableCategorySet.add("Competition");
			availableCategorySet.add("Favorite");
			availableCategorySet.add("Familiy");
			availableCategorySet.add("Friend");
			availableCategorySet.add("International");
			availableCategorySet.add("Miscellaneous");
			availableCategorySet.add("Personal");
			availableCategorySet.add("Supplier");
			availableCategorySet.add("VIP");
			availableCategorySet.setModified(false);	//show that the categories have not been modified
		}
		return availableCategorySet;	//return the singleton set of categories.
	}
	
}
