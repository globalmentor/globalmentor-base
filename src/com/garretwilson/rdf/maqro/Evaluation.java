package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

import static com.garretwilson.rdf.RDFUtilities.*;
import static com.garretwilson.rdf.maqro.MAQROConstants.*;

/**An evaluation of a MAQRO activity.
@author Garret Wilson
*/
public abstract class Evaluation extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**Default constructor.*/
	public Evaluation()
	{
		super();	//construct the parent class
	}
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Evaluation(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The <code>maqro:condition</code> literal value, or <code>null</code> if there is condition no value or it is not a plain literal.*/
	public String getCondition()
	{
		return asString(getPropertyValue(MAQRO_NAMESPACE_URI, CONDITION_PROPERTY_NAME));	//return the maqro:condition property value if it represents a string
	}
	
	/**Sets the <code>maqro:condition</code> property of the evaluation of the dialogue with the new condition string.
	@param condition The new conditoin string.
	*/
	public void setCondition(final String condition)
	{
		setProperty(MAQRO_NAMESPACE_URI, CONDITION_PROPERTY_NAME, condition); //set the maqro:condition property value
	}

}
