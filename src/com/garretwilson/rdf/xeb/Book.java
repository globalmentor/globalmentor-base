package com.garretwilson.rdf.xeb;

import java.net.URI;
import static com.garretwilson.rdf.xeb.XEBConstants.*;

/**Class representing a XEB book.
@author Garret Wilson
*/
public class Book extends Publication
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return BOOK_CLASS_NAME;}

	/**Default constructor.*/
	public Book()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Book(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

}
