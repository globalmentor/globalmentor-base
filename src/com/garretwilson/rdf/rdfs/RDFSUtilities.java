package com.garretwilson.rdf.rdfs;

import com.garretwilson.rdf.*;
import com.garretwilson.util.Debug;

/**Various supporting methods for processing RDF schema resources.
@author Garret Wilson
*/
public class RDFSUtilities implements RDFSConstants
{

	/**This class cannot be publicly constructed.*/
	private RDFSUtilities(){}

	/**Adds an <code>rdfs:comment</code> property to the resource.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param rdf The RDF data model.
	@param resource The resource to which the comment should be added.
	@param value A string comment value.
	*/
	public static void addComment(final RDF rdf, final RDFResource resource, final String value)
	{
//G***del when works		resource.addProperty(locateCommentProperty(rdf), new Literal(value));  //add a literal value to the resource as a comment
		RDFUtilities.addProperty(rdf, resource, RDFS_NAMESPACE_URI, COMMENT_PROPERTY_NAME, new Literal(value)); //add a literal value to the resource as a comment
	}

	/**Adds an <code>rdfs:label</code> property to the resource.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param rdf The RDF data model.
	@param resource The resource to which the label should be added.
	@param value A string label value.
	*/
	public static void addLabel(final RDF rdf, final RDFResource resource, final String value)
	{
//G***del when works		resource.addProperty(locateLabelProperty(rdf), new Literal(value));  //add a literal value to the resource as a label
		RDFUtilities.addProperty(rdf, resource, RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME, new Literal(value)); //add a literal value to the resource as a label
	}

	/**Retrieves the comment of the resource. If this resource has more than one
		property of <code>rdfs:comment</code>, it is undefined which of those property
		values will be returned.
		If the property value is a literal, its text will be returned. If the
		property value is a resource, its string value (usually its reference URI)
		will be returned.
	@param resource The resource the type of which will be returned.
	@return The comment of the resource, or <code>null</code> if no comment is
		present.
	*/
	public static String getComment(final RDFResource resource)
	{
		final RDFObject rdfObject=resource.getPropertyValue(RDFS_NAMESPACE_URI, COMMENT_PROPERTY_NAME); //get the value of the comment property
		return rdfObject!=null ? rdfObject.toString() : null; //return the string value of the comment property, or null if is no such property
	}

	/**Retrieves the label of the resource. If this resource has more than one
		property of <code>rdfs:label</code>, it is undefined which of those property
		values will be returned.
		If the property value is a literal, its text will be returned. If the
		property value is a resource, its string value (usually its reference URI)
		will be returned.
	@param resource The resource the type of which will be returned.
	@return The label of the resource, or <code>null</code> if no label is present.
	*/
	public static String getLabel(final RDFResource resource)
	{
//G***del Debug.trace("getting label for resource: ", resource);
//G***del Debug.trace("resource type: ", com.garretwilson.rdf.RDFUtilities.getType(resource)); //G***del
		final RDFObject rdfObject=resource.getPropertyValue(RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME); //get the value of the label property
		return rdfObject!=null ? rdfObject.toString() : null; //return the string value of the label property, or null if is no such property
	}

	/**Gets an <code>rdfs:comment</code> property from RDF. This ensures that an
		existing label property will be used, if present.
	@param rdf The RDF data model.
	@return The <code>rdfs:comment</code> property resource.
	*/
/*G***del if not needed
	public static RDFResource locateCommentProperty(final RDF rdf)
	{
		return rdf.locateResource(RDFS_NAMESPACE_URI, COMMENT_PROPERTY_NAME); //get an rdfs:comment resource
	}
*/

	/**Gets an <code>rdfs:label</code> property from RDF. This ensures that an
		existing label property will be used, if present.
	@param rdf The RDF data model.
	@return The <code>rdfs:label</code> property resource.
	*/
/*G***del if not needed
	public static RDFResource locateLabelProperty(final RDF rdf)
	{
		return rdf.locateResource(RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME); //get an rdfs:label resource
	}
*/

}