package com.garretwilson.rdf;

import com.garretwilson.util.Debug;

/**Represents a statement in the RDF data model.
@author Garret Wilson
*/
public class Statement
{

	/**The predicate of the triple.*/
	private final RDFResource predicate;

		/**@return The predicate of the triple.*/
		public RDFResource getPredicate() {return predicate;}

	/**The subject of the triple.*/
	private final RDFResource subject;

		/**@return The subject of the triple.*/
		public RDFResource getSubject() {return subject;}

	/**The object of the triple.*/
	private final RDFObject object;

		/**@return The object of the triple.*/
		public RDFObject getObject() {return object;}

	/**Creates a new statement from existing data.
	@param newPredicate The predicate of the statement.
	@param newSubject The subject of the statement.
	@param newObject The object of the statement.
	*/
	public Statement(final RDFResource newPredicate, final RDFResource newSubject, final RDFObject newObject)
	{
		Debug.assert(newObject!=null, "Object of statement with subject "+newSubject+" and predicate "+newPredicate+" is null."); //G***del
		predicate=newPredicate; //set the predicate
		subject=newSubject; //set the subject
		object=newObject; //set the object
	}

	/**@return A string representation of the statement triple in the form:
	"{predicate, subject, object}".
	*/
	public String toString()
	{
		final StringBuffer stringBuffer=new StringBuffer(); //create a new string buffer
		stringBuffer.append('{');
		stringBuffer.append('[').append(getPredicate().getReferenceURI()).append(']');
		stringBuffer.append(',').append(' ');
		stringBuffer.append('[').append(getSubject().getReferenceURI()).append(']');
		stringBuffer.append(',').append(' ');
		final RDFObject object=getObject(); //get the object of the statement
		if(object instanceof RDFResource)  //if the object is a resource
			stringBuffer.append('[').append(((RDFResource)object).getReferenceURI()).append(']');  //[resource]
		else if(object instanceof RDFLiteral)  //if the object is a literal
			stringBuffer.append('"').append(((RDFLiteral)object).getLexicalForm()).append('"'); //"literal"
		else  //if we don't know what type the object is (this should never happen unless RDF is changed in the future)
			stringBuffer.append(getObject().toString());  //let the object produce a string representing itself
		stringBuffer.append('}');
		return stringBuffer.toString(); //return the string we just constructed
	}
}