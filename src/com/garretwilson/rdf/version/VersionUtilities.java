package com.garretwilson.rdf.version;

import com.garretwilson.rdf.*;

/**Utilities for working with version metadata stored in RDF.
@author Garret Wilson
*/
public class VersionUtilities extends RDFUtilities implements VersionConstants
{

	/**Adds a <code>ver:version</code> property with the given value to the
		resource.
	@param resource The resource to which the property should be added.
	@param value The property value to add.
	@return The added literal property value.
	*/
	public static RDFLiteral addVersion(final RDFResource resource, final String value)
	{
		return addProperty(resource, VERSION_NAMESPACE_URI, VERSION_PROPERTY_NAME, value);
	}

	/**Returns the value of the first <code>ver:version</code> property.
	@param resource The resource the property of which should be located.
	@return The value of the first <code>ver:version</code> property, or
		<code>null</code> if no such property exists.
	*/
	public static RDFObject getVersion(final RDFResource resource)
	{
		return resource.getPropertyValue(VERSION_NAMESPACE_URI, VERSION_PROPERTY_NAME);
	}
}