package com.globalmentor.rdf.maqro;

import java.net.URI;

import static com.globalmentor.java.Objects.*;
import com.globalmentor.rdf.*;
import static com.globalmentor.rdf.RDF.*;
import static com.globalmentor.rdf.maqro.MAQROConstants.*;
import com.globalmentor.rdf.xmlschema.NumberLiteral;

/**The result of a score evalution.
@author Garret Wilson
*/
public class Score extends Result
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return MAQRO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return SCORE_CLASS_NAME;}

	/**Default constructor.*/
	public Score()
	{
		super();	//construct the parent class
	}

	/**Interaction constructor.
	@param interaction The interaction this result represents.
	*/
/*TODO fix evaluation constructor
	public Score(final Interaction interaction)
	{
		this();	//construct the class
		setInteraction(interaction);	//set the interaction
	}
*/
	
	/**Reference URI constructor.
	@param referenceURI The reference URI for the new resource.
	*/
	public Score(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**Sets the value of the score.
	@param literal The literal number score value.
	*/
	public void setValue(final NumberLiteral literal)
	{
		RDFUtilities.setValue(this, literal);	//set the value of this score resource
	}

	/**Retrieves the literal number value of the score.
	@return The score value, or <code>null</code> if there is no value
		or the value is not a number.
	*/
	public NumberLiteral<?> getValue()
	{
		return asInstance(RDFUtilities.getValue(this), NumberLiteral.class);	//get the value of this score resource
	}

	/**Sets the possible value of the score.
	@param literal The literal score possible value.
	*/
	public void setPossible(final RDFLiteral literal)
	{
		setProperty(RDF_NAMESPACE_URI, POSSIBLE_PROPERTY_NAME, literal); //replace all possible properties with the given literal value
	}

	/**Retrieves the literal number possible value of the score.
	@return The possible score value, or <code>null</code> if there is no possible
		value or the possible value is not a number.
	*/
	public NumberLiteral<?> getPossible()
	{
		return asInstance(getPropertyValue(RDF_NAMESPACE_URI, POSSIBLE_PROPERTY_NAME), NumberLiteral.class); //get the value of the possible property only if it is a number
	}
	
}
