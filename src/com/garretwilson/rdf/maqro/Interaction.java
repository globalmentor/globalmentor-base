package com.garretwilson.rdf.maqro;

import java.net.URI;
import java.util.*;
import com.garretwilson.rdf.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

/**Designates an object is an interaction that can be part of a MAQRO activity.
@author Garret Wilson
*/
public abstract class Interaction extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Interaction()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Interaction(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Adds a category to the interaction.
	@param category The category to add.
	@param language The language of the category, or <code>null</code> if
		no language should be specified.
	*/
	public void addCategory(final String category, final Locale language)
	{
		MAQROUtilities.addCategory(this, category, language);	//add the category to the interaction
	}

	/**@return An iterable to categories, if any, of the interaction.*/
	public Iterable<RDFObject> getCategories()
	{
		return MAQROUtilities.getCategories(this);	//return an iterable to the categories
	}

	/**Determines if the interaction has a category in the given category set.
	<p>If the category set contains <code>NO_CATEGORY</code>, this method returns
		<code>true</code> if the interactions has no categories specified.</p>
	@param categorySet The set of categories, any category of which will allow
		the interaction to be selected.
	@return <code>true</code> if the interaction has a category that is
		included in the category set.
	*/
	public boolean hasCategory(final Set categorySet)
	{
		return MAQROUtilities.hasCategory(this, categorySet);	//see whether this category has one of the supplied categories
	}

}
