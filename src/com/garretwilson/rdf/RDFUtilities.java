package com.garretwilson.rdf;

import java.io.*;
import java.net.URI;
import java.util.*;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLUtilities;
import org.w3c.dom.*;

/**Various supporting methods for processing RDF.
@author Garret Wilson
*/
public class RDFUtilities implements RDFConstants
{

	/**This class cannot be publicly constructed.*/
	private RDFUtilities(){}

	/**Adds a property by creating a <code>NameValuePair</code> from the given
		property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject
		of an RDF statement with the property as the predicate and the value as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param rdf The RDF data model.
	@param resource The resource to which the type should be added.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public static RDFObject addProperty(final RDF rdf, final RDFResource resource, final URI propertyNamespaceURI, final String propertyLocalName, final RDFObject value)
	{
		return resource.addProperty(rdf.locateResource(propertyNamespaceURI, propertyLocalName), value); //create a property from the namespace URI and local name, then add the actual property value
	}

	/**Adds a literal property from a string by creating a <code>NameValuePair</code>
		from the given property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject of an RDF statement
		with the property as the predicate and the value, stored as a literal, as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param rdf The RDF data model.
	@param resource The resource to which the type should be added.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>Literal</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public static Literal addProperty(final RDF rdf, final RDFResource resource, final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue)
	{
		return resource.addProperty(rdf.locateResource(propertyNamespaceURI, propertyLocalName), literalValue); //create a new literal value and store the property
	}

	/**Adds an <code>rdf:type</code> property to the resource.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param rdf The RDF data model.
	@param resource The resource to which the type should be added.
	@param value A type property value; the object of an RDF statement.
	@return The added property value.
	*/
	public static RDFObject addType(final RDF rdf, final RDFResource resource, final RDFObject value)
	{
		return resource.addProperty(locateTypeProperty(rdf), value);  //add the value to the resource as a type
	}

	/**Adds an <code>rdf:type</code> property to the resource.
		This is a convenience method that allows specification
		of the property value resource through a namespace URI and a local name.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param rdf The RDF data model.
	@param resource The resource to which the type should be added.
	@param valueNamespaceURI The XML namespace URI used in the serialization
		of the property value resource that is the object of an RDF statement.
	@param valueLocalName The XML local name used in the serialization of the
		property value resource that is the object of an RDF statement.
	@return The added property value.
	*/
	public static RDFObject addType(final RDF rdf, final RDFResource resource, final URI valueNamespaceURI, final String valueLocalName)
	{
		return addType(rdf, resource, rdf.locateResource(valueNamespaceURI, valueLocalName));  //get a resource from the namespace and local name and add it as a type
	}

	/**Creates a resource reference URI from an XML namespace URI (which may be
		<code>null</code> and an XML local name.
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@return An RDF reference URI constructed from the given namespace and local name.
//G***del if not needed	@exception URISyntaxException Thrown if a valid URI cannot be created from the given namespace and local name.
	*/
	public static URI createReferenceURI(final URI namespaceURI, final String localName) //G***del if not needed throws URISyntaxException
	{
		final StringBuffer stringBuffer=new StringBuffer();  //create a string buffer to hold the resource URI
		if(namespaceURI!=null)  //if there is a namespace URI	//G***is this right?
		  stringBuffer.append(namespaceURI);  //append the namespace URI
		stringBuffer.append(localName); //append the local name
		return URI.create(stringBuffer.toString()); //return a URI from the the string we constructed; if somehow concatenating the strings does not create a valid URI, a runtime exception will be thrown
	}

	/**Gets an <code>rdf:type</code> property from RDF. This ensures that an
		existing type property will be used, if present.
	@param rdf The RDF data model.
	@return The <code>rdf:type</code> property resource.
	*/
	public static RDFResource locateTypeProperty(final RDF rdf)
	{
		return rdf.locateResource(RDF_NAMESPACE_URI, TYPE); //get an rdf:type resource
	}

	/**Retrieves a label appropriate for the reference URI of the resource. If the
		resource has namespace URI and local name, the XML qualified name will be
		returned in <em>namespaceURI</em>:<em>localName</em> format; otherwise,
		reference URI itself will be returned.
	@param rdf The RDF data model to which the resource belongs.
	@param resource The resource the label of which to return.
	@return A label representing the reference URI of the resource.
	*/
/*G***del if not needed; moved to RDFXMLifier
	public static String getLabel(final RDF rdf, final RDFResource resource)
	{


		RDFUtilities.createReferenceURI()

		final String namespaceURI=resource.getNamespaceURI(); //get the resource namespace URI, if it has one
		final String localName=resource.getLocalName(); //get the resource's local name
		if(namespaceURI!=null && localName!=null) //if this resource has a namespace URI and a local name
		{
				//get the prefix from the RDF data model to be used for this namespace
			final String prefix=XMLSerializer.getNamespacePrefix(rdf.getNamespacePrefixMap(), namespaceURI);
			return XMLUtilities.createQualifiedName(prefix, localName);  //create an XML qualified name for this namespace and local name
		}
		else  //if there is no namespace URI known
			return resource.getReferenceURI(); //just use the reference URI as the label


			if(RDF_NAMESPACE_URI.equals(namespaceURI))  //if this is the RDF namespace
			{
				if(localName.startsWith(CONTAINER_MEMBER_PREFIX)) //if this is one of the rdf:li_XXX members
					localName=ELEMENT_LI; //just use the normal rdf:li property name---the order is implicit in the serialization
			}
			qualifiedName=XMLUtilities.createQualifiedName(prefix, localName);  //create a qualified name for the element

	}
*/

	/**Retrieves the resources in an RDF data model that are of the requested type.
	@param rdf The RDF data model.
	@param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	@param typeLocalName The XML local name that represents part of the reference URI.
	@return A read-only collection of resources that are of the requested type.
	*/
	public static Collection getResourcesByType(final RDF rdf, final URI typeNamespaceURI, final String typeLocalName)
	{
		return getResourcesByType(rdf, RDFUtilities.createReferenceURI(typeNamespaceURI, typeLocalName)); //gather the resources with a type property of the URI from the given namespace and local name
	}

	/**Retrieves the resources in an RDF data model that are of the requested type.
	@param rdf The RDF data model.
	@param typeURI The reference URI of the type resource.
	@return A read-only collection of resources that are of the requested type.
	*/
	public static Collection getResourcesByType(final RDF rdf, final URI typeURI)
	{
		final List resourceList=new ArrayList();  //create a list in which to store the resources
		final Iterator resourceIterator=rdf.getResourceIterator();  //get an iterator to the resources in this data model
		while(resourceIterator.hasNext()) //while there are more resources
		{
		  final RDFResource resource=(RDFResource)resourceIterator.next();  //get the next resource
		  if(isType(resource, typeURI)) //if this resource is of the given type
				resourceList.add(resource); //add this resource to our list
		}
		return Collections.unmodifiableList(resourceList);  //make the list read-only and return it
	}

	/**Retrieves the type of the resource. If this resource has more than one
		property of <code>rdf:type</code>, it is undefined which of those property
		values will be returned.
	@param resource The resource the type of which will be returned.
	@return The type value of the resource, or <code>null</code> if the resource
		has no type specified.
	@exception ClassCastException Thrown if the resource type property value is
		not a <code>RDFResource</code> (such as a <code>Literal</code>), which would
		indicate that an incorrect value has been stored for thee type.
	*/
	public static RDFResource getType(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, TYPE); //return the type property
	}

	/**Retrieves a label appropriate for the type of the resource. If the type
		has namespace URI and local name, the XML qualified name will be returned in
		<em>namespaceURI</em>:<em>localName</em> format; otherwise, the type
		reference URI will be returned. If this resource has no type,
		<code>null</code> will be returned.
	@param rdf The RDF data model to which the resource belongs.
	@param resource The resource the label of which type to return.
	@return A label representing the type of the resource, or <code>null</code>
		if the resource has no type.
	@exception ClassCastException Thrown if the resource type property value is
		not a <code>RDFResource</code> (such as a <code>Literal</code>), which would
		indicate that an incorrect value has been stored for thee type.
	*/
/*G***del if not needed
	public static String getTypeLabel(final RDF rdf, final RDFResource resource) throws ClassCastException
	{
		final RDFResource type=RDFUtilities.getType(resource);  //get the type of the resource
		return type!=null ? getLabel(rdf, type) : null; //return the label of the type, if there is a type
		if(type!=null)  //if this object has a type
		{
			return getLabel(rdf, type); //return the label of the type
			final String typeNamespaceURI=type.getNamespaceURI(); //get the type namespace URI, if it has one
			final String typeLocalName=type.getLocalName(); //get the type's local name
			if(typeNamespaceURI!=null && typeLocalName!=null) //if this type has a namespace URI and a local name
			{
					//get the prefix from the RDF data model to be used for this type namespace
				final String typePrefix=XMLSerializer.getNamespacePrefix(rdf.getNamespacePrefixMap(), typeNamespaceURI);
//G***del					//get the prefix to be used for this type namespace G***share this namespace prefix map between all nodes, somehow
//G***del				final String typePrefix=XMLSerializer.getNamespacePrefix(XMLSerializer.createNamespacePrefixMap(), typeNamespaceURI);
				return XMLUtilities.createQualifiedName(typePrefix, typeLocalName);  //create an XML qualified name for this type
			}
			else  //if there is no namespace URI known
				return type.getReferenceURI(); //just use the reference URI as the type label
		}
		else  //if this object has no type
			return null; //we have no type label
	}
*/

	/**Determines whether a given resource is of a particular type.
		Every type property of the resource is checked.
	@param resource The resource the type type of which to check.
	@param typeURI The reference URI of the type resource.
	@return <code>true</code> if the resource has the indicated type property.
	*/
	public static boolean isType(final RDFResource resource, final URI typeURI)
	{
		return resource.hasPropertyValue(RDF_NAMESPACE_URI, TYPE, typeURI); //determine if the resource has a type property of the given URI
	}

	/**Determines whether a given resource is of a particular type.
		Every type property of the resource is checked.
	@param resource The resource the type type of which to check.
	@param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	@param typeLocalName The XML local name that represents part of the reference URI.
	@return <code>true</code> if the resource has the indicated type property.
	*/
	public static boolean isType(final RDFResource resource, final URI typeNamespaceURI, final String typeLocalName)
	{
		return resource.hasPropertyValue(RDF_NAMESPACE_URI, TYPE, RDFUtilities.createReferenceURI(typeNamespaceURI, typeLocalName)); //determine if the resource has a type property of the URI from the given namespace and local name
	}

	/**Converts an RDF data model to an XML string. If an error occurs converting
		the XML document document to a string, the normal XML object string will be
		returned.
	@param rdf The RDF data model to convert.
	@return A string representation of an XML serialization of the RDF data model.
	*/
	public static String toString(final RDF rdf)
	{
		final ByteArrayOutputStream outputStream=new ByteArrayOutputStream(); //create an output stream of bytes
		final RDFXMLifier rdfXMLifier=new RDFXMLifier();  //create an object to turn the RDF into XML
/*G***setup namespace prefixes if we can
				//setup the Mentoract namespace prefixes
		rdfXMLifier.getNamespacePrefixMap().put(MENTORACT_PROTOCOL_NAMESPACE_URI, MENTORACT_PROTOCOL_DEFAULT_PREFIX);
		rdfXMLifier.getNamespacePrefixMap().put(MentoractResourceURIConstants.MENTORACT_RESOURCE_NAMESPACE_URI, MentoractResourceURIConstants.MENTORACT_RESOURCE_DEFAULT_PREFIX);
*/
			//create an XML document from the RDF
		final Document document=rdfXMLifier.createDocument(rdf, new XMLDOMImplementation());  //G***try to make this XML parser agnostic
		return XMLUtilities.toString(document); //convert the XML document to a string and return it
	}

}