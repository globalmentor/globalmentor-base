package com.globalmentor.rdf.rdfs;

import com.globalmentor.rdf.*;

/**Various supporting methods for processing RDF schema resources.
@author Garret Wilson
*/
public class RDFSUtilities extends RDFUtilities implements RDFSConstants
{

	/**Adds an <code>rdfs:comment</code> property to the resource.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the comment should be added.
	@param value A string comment value.
	*/
	public static void addComment(final RDFResource resource, final String value)
	{
		resource.addProperty(RDFS_NAMESPACE_URI, COMMENT_PROPERTY_NAME, value); //add a literal value to the resource as a comment
	}

	/**Adds an <code>rdfs:label</code> literal property to the resource.
	@param resource The resource to which the label should be added.
	@param value A literal label value.
	*/
	public static void addLabel(final RDFResource resource, final RDFLiteral value)
	{
		resource.addProperty(RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME, value); //add a literal value to the resource as a label
	}

	/**Adds an <code>rdfs:label</code> property to the resource.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the label should be added.
	@param value A string label value.
	*/
	public static void addLabel(final RDFResource resource, final String value)
	{
		resource.addProperty(RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME, value); //add a literal value to the resource as a label
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

	/**Retrieves the label value of the resource. If this resource has more than
		one property of <code>rdfs:label</code>, it is undefined which of these
		property values will be returned.
	@param resource The resource the label of which will be returned.
	@return The label of the resource, or <code>null</code> if there is no label
		or the label is not a literal.
	*/
	public static RDFLiteral getLabel(final RDFResource resource)
	{
		final RDFObject rdfObject=resource.getPropertyValue(RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME); //get the value of the label property
		return rdfObject instanceof RDFLiteral ? (RDFLiteral)rdfObject : null; //return the literal value object, or null if is no such property or the property value is not a literal
	}

	/**Replaces all <code>rdfs:label</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the label properties should be replaced.
	@param value A string label value.
	*/
	public static void setLabel(final RDFResource resource, final String value)
	{
		resource.setProperty(RDFS_NAMESPACE_URI, LABEL_PROPERTY_NAME, value); //replace all label properties with a literal label value
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