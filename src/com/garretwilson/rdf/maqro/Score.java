package com.garretwilson.rdf.maqro;

import java.net.URI;
import com.garretwilson.rdf.*;

/**The result of a score evalution.
@author Garret Wilson
*/
public class Score extends Result implements MAQROConstants
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
	@param literal The literal score value.
	*/
	public void setValue(final RDFLiteral literal)
	{
		RDFUtilities.setValue(this, literal);	//set the value of this score resource
	}

	/**Retrieves the literal value of the score.
	@return The score value, or <code>null</code> if there is no value
		or the value is not a literal.
	*/
	public RDFLiteral getValue()
	{
		return RDFUtilities.getValue(this);	//get the value of this score resource
	}

	/**Sets the possible value of the score.
	@param literal The literal score possible value.
	*/
	public void setPossible(final RDFLiteral literal)
	{
		setProperty(RDF_NAMESPACE_URI, POSSIBLE_PROPERTY_NAME, literal); //replace all possible properties with the given literal value
	}

	/**Retrieves the literal possible value of the score.
	@return The possible score value, or <code>null</code> if there is no possible
		value or the possible value is not a literal.
	*/
	public RDFLiteral getPossible()
	{
		return RDFUtilities.asLiteral(getPropertyValue(RDF_NAMESPACE_URI, POSSIBLE_PROPERTY_NAME)); //get the value of the possible property only if it is a literal
	}
	
}
