package com.garretwilson.rdf;

import java.io.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.net.URIConstants;
import com.garretwilson.rdf.rdfs.RDFSUtilities;
import com.garretwilson.rdf.xmlschema.URILiteral;

import static com.garretwilson.rdf.RDFConstants.*;
import com.garretwilson.text.xml.XMLDOMImplementation;
import com.garretwilson.text.xml.XMLUtilities;

import org.w3c.dom.*;

/**Various supporting methods for processing RDF.
@author Garret Wilson
*/
public class RDFUtilities
{

	/**The start of a reference URI from the rdf:li element qualified
		name (i.e. "rdfNamespaceURI#li_"), which we'll use to check for items
	*/
	protected final static String RDF_LI_REFERENCE_PREFIX=createReferenceURI(RDF_NAMESPACE_URI, CONTAINER_MEMBER_PREFIX).toString();

	/**Sets a typed literal property from lexical form and datatype URI by
		creating a <code>RDFPropertyValuePair</code> from the given property and
		value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject of an RDF statement
		with the property as the predicate and the value, stored as a literal, as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the type should be added.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param lexicalForm The lexical form of the literal value that will be stored
		in a <code>RDFTypedLiteral</code> created from the RDF data model.
	@param datatypeURI The reference URI identifying the datatype of this literal.
	@return The added property value.
	*/
/*G***del if we don't want or need this method
	public static RDFTypedLiteral addProperty(final RDFResource resource, final URI propertyNamespaceURI, final String propertyLocalName, final String lexicalForm, final URI datatypeURI)
	{
		return (RDFTypedLiteral)resource.addProperty(locateResource(resource, propertyNamespaceURI, propertyLocalName), rdf.createTypedLiteral(lexicalForm, datatypeURI)); //create a property from the namespace URI and local name, create a new typed literal value, and add the property
	}
*/

	/**Adds a language tag based upon a given locale using a literal to represent
		a language identifier according as defined in
		<a href="http://www.ietf.org/rfc/rfc1766.txt">RFC 1766</a>,
		"Tags for the Identification of Languages".
	@param rdf The RDF data model.
	@param resource The resource to which the value should be added.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param language A locale the language tag of which will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
/*G***del if not needed
	public static RDFLiteral addProperty(final RDF rdf, final RDFResource resource, final URI propertyNamespaceURI, final String propertyLocalName, final Locale language)
	{
		return addProperty(rdf, resource, propertyNamespaceURI, propertyLocalName, LocaleUtilities.getLanguageTag(language)); //store the language tag as a literal
	}
*/

	/**Adds an <code>rdf:type</code> property to the resource.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the type should be added.
	@param value A type property value; the object of an RDF statement.
	@return The added property value.
	*/
	public static RDFObject addType(final RDFResource resource, final RDFObject value)
	{
		return resource.addProperty(locateTypeProperty(resource), value);  //add the value to the resource as a type
	}

	/**Adds an <code>rdf:type</code> property to the resource.
		This is a convenience method that allows specification
		of the property value resource through a namespace URI and a local name.
		<p>If an equivalent property already exists, no action is taken.</p>
	@param resource The resource to which the type should be added.
	@param valueNamespaceURI The XML namespace URI used in the serialization
		of the property value resource that is the object of an RDF statement.
	@param valueLocalName The XML local name used in the serialization of the
		property value resource that is the object of an RDF statement.
	@return The added property value.
	*/
	public static RDFObject addType(final RDFResource resource, final URI valueNamespaceURI, final String valueLocalName)
	{
		return addType(resource, locateResource(resource, valueNamespaceURI, valueLocalName));  //get a resource from the namespace and local name and add it as a type
	}

	/**Determines if the RDF object is a list and, if so, casts the object 
		to a list and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a list, or <code>null</code> if the object is
		not a list or the object is <code>null</code>.
	*/
	public static RDFListResource asListResource(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, RDFListResource.class);	//cast the object to a list if we can
	}

	/**Determines if the RDF object is a literal and, if so, casts the object to a literal and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a literal, or <code>null</code> if the object is not a literal or the object is <code>null</code>.
	*/
	public static RDFLiteral asLiteral(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, RDFLiteral.class);	//cast the object to a literal if we can
	}

	/**Determines if the RDF object is a resource and, if so, casts the object 
		to a resource and returns it.
	@param rdfObject The RDF object in question.
	@return The RDF object as a resource, or <code>null</code> if the object is
		not a resource or the object is <code>null</code>.
	*/
	public static RDFResource asResource(final RDFObject rdfObject)
	{
		return asInstance(rdfObject, RDFResource.class);	//cast the object to a resource if we can
	}

	/**Determines if the RDF object is a plain literal or a URI typed literal and, if so, creates a URI from the literal's lexical form if possible.
	@param rdfObject The RDF object in question.
	@return The RDF object as a URI, or <code>null</code> if the object is not a literal, the lexical form does not contain a valid URI, or the object is <code>null</code>.
	*/
	public static URI asURI(final RDFObject rdfObject)	//TODO should we allow a resource as well and return its reference URI?
	{
		if(rdfObject instanceof RDFLiteral)	//if this object is a literal
		{
			final RDFLiteral rdfLiteral=(RDFLiteral)rdfObject;	//cast the object to a literal
			if(rdfLiteral instanceof URILiteral)	//if this is a URI literal
			{
				return ((URILiteral)rdfLiteral).getValue();	//return the URI literals' value
			}
			else if(rdfLiteral instanceof RDFPlainLiteral)	//if this is a plain literal
			{
				try
				{
					return new URI(rdfLiteral.getLexicalForm());	//create a URI from the literal's lexical form
				}
				catch(final URISyntaxException uriSyntaxException)	//ignore URI format errors and return null
				{
				}
			}
		}
		return null;	//indicate that the object didn't contain a valid URI
	}
	
	/**Creates a resource reference URI from an XML namespace URI (which may be
		<code>null</code> and an XML local name.
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@return An RDF reference URI constructed from the given namespace and local name.
//G***del if not needed	@exception URISyntaxException Thrown if a valid URI cannot be created from the given namespace and local name.
	*/
	public static URI createReferenceURI(final URI namespaceURI, final String localName) //G***del if not needed throws URISyntaxException	//TODO del if not needed with QualifiedName.createReferenceURI
	{
			//TODO check for local names that aren't valid URI characters---see QualifiedName.createReferenceURI
		final StringBuilder stringBuilder=new StringBuilder();  //create a string builder to hold the resource URI
		if(namespaceURI!=null)  //if there is a namespace URI	//G***is this right?
		  stringBuilder.append(namespaceURI);  //append the namespace URI
		stringBuilder.append(localName); //append the local name
		return URI.create(stringBuilder.toString()); //return a URI from the the string we constructed; if somehow concatenating the strings does not create a valid URI, a runtime exception will be thrown
	}

	/**Determines if the given property reference URI is an RDF container member
		reference&mdash;i.e. it begins with <code>rdf:_</code>.
	@param propertyReferenceURI The reference URI of the predicate of an RDF
		statement.
	@return <code>true</code> if the property reference URI represents a
		container member.
	*/
	public static boolean isContainerMemberPropertyReference(final URI propertyReferenceURI)
	{
		return propertyReferenceURI.toString().startsWith(RDF_LI_REFERENCE_PREFIX);	//return whether this property name begins with rdf:_
	}

	/**Locates and returns an existing resource or creates a new resource 
		from the data model associated with the given resource. If no data model
		is associated with the given resource, a new resource will be created and
		returned.
	@param contextResource A resource which may be associated with an RDF data model
	@param referenceURI The reference URI of the resource to retrieve.
	@return A resource with the given reference URI.
	*/
	public static RDFResource locateResource(final RDFResource contextResource, final URI referenceURI)
	{
		return locateResource(contextResource, referenceURI, null, null);	//locate a resource without knowing its type
	}

	/**Locates and returns an existing resource or creates a new resource 
		from the data model associated with the given resource. If no data model
		is associated with the given resource, a new resource will be created and
		returned.
	@param contextResource A resource which may be associated with an RDF data model
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@return A resource with a reference URI corresponding to the given namespace
		URI and local name.
	*/
	public static RDFResource locateResource(final RDFResource contextResource, final URI namespaceURI, final String localName)
	{
		return locateResource(contextResource, RDFUtilities.createReferenceURI(namespaceURI, localName), namespaceURI, localName);	//locate a resource, constructing a reference URI from the given namespace URI and local name
	}

	/**Locates and returns an existing resource or creates a new resource 
		from the data model associated with the given resource. If no data model
		is associated with the given resource, a new resource will be created and
		returned.
	@param contextResource A resource which may be associated with an RDF data model
	@param referenceURI The reference URI of the resource to retrieve.
	@param namespaceURI The XML namespace URI used in the serialization, or
		<code>null</code> if the namespace URI is not known.
	@param localName The XML local name used in the serialization, or
		<code>null</code> if the local name is not known.
	@return A resource with a reference URI corresponding to the given namespace
		URI and local name.
	*/
	protected static RDFResource locateResource(final RDFResource contextResource, final URI referenceURI, final URI namespaceURI, final String localName)
	{
		return locateTypedResource(contextResource, referenceURI, namespaceURI, localName, null, null);	//locate a resource, constructing a reference URI from the given namespace URI and local name
	}

	/**Locates and returns an existing resource or creates a new resource 
		from the data model associated with the given resource. If no data model
		is associated with the given resource, a new resource will be created and
		returned.
	@param contextResource A resource which may be associated with an RDF data model
	@param referenceURI The reference URI of the resource to retrieve.
	@param typeNamespaceURI The XML namespace used in the serialization of the
		type URI, or <code>null</code> if the type is not known.
	@param typeLocalName The XML local name used in the serialization of the type
		URI, or <code>null</code> if the type is not known.
	@return A resource with the given reference URI.
	*/
	public static RDFResource locateTypedResource(final RDFResource contextResource, final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName)
	{
		return locateTypedResource(contextResource, referenceURI, null, null, typeNamespaceURI, typeLocalName);	//locate a resource with no namespace URI or local name
	}

	/**Locates and returns an existing resource or creates a new resource 
		from the data model associated with the given resource. If no data model
		is associated with the given resource, a new resource will be created and
		returned.
	@param contextResource A resource which may be associated with an RDF data model
	@param referenceURI The reference URI of the new resource.
	@param namespaceURI The XML namespace URI used in the serialization, or
		<code>null</code> if the namespace URI is not known.
	@param localName The XML local name used in the serialization, or
		<code>null</code> if the local name is not known.
	@param typeNamespaceURI The XML namespace used in the serialization of the
		type URI, or <code>null</code> if the type is not known.
	@param typeLocalName The XML local name used in the serialization of the type
		URI, or <code>null</code> if the type is not known.
	@return A new resource, optionally with the given type, created from the
		given resource's RDF data model if possible.
	*/
	static RDFResource locateTypedResource(final RDFResource contextResource, final URI referenceURI, final URI namespaceURI, final String localName, final URI typeNamespaceURI, final String typeLocalName)
	{
		final RDF rdf=contextResource.getRDF();	//get the RDF data model of the given resource
		if(rdf!=null)	//if we know which RDF data model to use
		{
				//locate the resource within the RDF data model 
			return rdf.locateTypedResource(referenceURI, namespaceURI, localName, typeNamespaceURI, typeLocalName);
		}
		else	//if there is no known data model
		{
			final RDFResource resource=new DefaultRDFResource(referenceURI, namespaceURI, localName);  //create a new resource from the given reference URI
			if(typeNamespaceURI!=null && typeLocalName!=null)	//if we were given a type
			{
				RDFUtilities.addType(resource, typeNamespaceURI, typeLocalName); //add the type property
			}
			return resource;	//return the resource we created			
		}
	}

	/**Gets an <code>rdf:type</code> property for a given resource.
		This ensures that an existing type property will be used, if the resource
		is associated with a data model and the <code>rdf:type</code> property
		already exists.
	@param contextResource A resource which may be associated with an RDF data model
	@return The <code>rdf:type</code> property resource.
	*/
	public static RDFResource locateTypeProperty(final RDFResource contextResource)
	{
			//TODO pass along the rdf:Property type, maybe, and create a separate method for creating properties
		return locateResource(contextResource, RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME); //get an rdf:type resource
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

	/**Retrieves a resource from an RDF data model that is of the requested type.
	If there are more than one resource with the requested type, it is undefined which one will be returned.
	@param rdf The RDF data model.
	@param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	@param typeLocalName The XML local name that represents part of the reference URI.
	@return A resource of the requested type, or <code>null</code> if there are no resourcees with the specified type.
	*/
	public static RDFResource getResourceByType(final RDF rdf, final URI typeNamespaceURI, final String typeLocalName)	//TODO should we move these to the RDF data model?
	{
		final Iterator resourceIterator=getResourcesByType(rdf, typeNamespaceURI, typeLocalName).iterator();	//get an iterator to matching resources
		return resourceIterator.hasNext() ? (RDFResource)resourceIterator.next() : null;	//return the first resource, if there are any at all
	}

	/**Retrieves the resources in an RDF data model that are of the requested type.
	@param rdf The RDF data model.
	@param typeNamespaceURI The XML namespace URI that represents part of the reference URI.
	@param typeLocalName The XML local name that represents part of the reference URI.
	@return A read-only collection of resources that are of the requested type.
	*/
	public static Collection<RDFResource> getResourcesByType(final RDF rdf, final URI typeNamespaceURI, final String typeLocalName)
	{
		return getResourcesByType(rdf, RDFUtilities.createReferenceURI(typeNamespaceURI, typeLocalName)); //gather the resources with a type property of the URI from the given namespace and local name
	}

	/**Retrieves a resource from an RDF data model that is of the requested type.
	If there are more than one resource with the requested type, it is undefined which one will be returned.
	@param rdf The RDF data model.
	@param typeURI The reference URI of the type resource.
	@return A resource of the requested type, or <code>null</code> if there are no resourcees with the specified type.
	*/
	public static RDFResource getResourceByType(final RDF rdf, final URI typeURI)
	{
		final Iterator resourceIterator=getResourcesByType(rdf, typeURI).iterator();	//get an iterator to matching resources
		return resourceIterator.hasNext() ? (RDFResource)resourceIterator.next() : null;	//return the first resource, if there are any at all
	}

	/**Retrieves the resources in an RDF data model that are of the requested type.
	@param rdf The RDF data model.
	@param typeURI The reference URI of the type resource.
	@return A read-only collection of resources that are of the requested type.
	*/
	public static Collection<RDFResource> getResourcesByType(final RDF rdf, final URI typeURI)
	{
		final List<RDFResource> resourceList=new ArrayList<RDFResource>();  //create a list in which to store the resources
		final Iterator<RDFResource> resourceIterator=rdf.getResourceIterator();  //get an iterator to the resources in this data model
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
		not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
		indicate that an incorrect value has been stored for the type.
	*/
	public static RDFResource getType(final RDFResource resource) throws ClassCastException
	{
		return (RDFResource)resource.getPropertyValue(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME); //return the type property
	}
	
	/**Returns an iterator to each property of <code>rdf:type</code>.
	@param resource The resource the types of which will be returned.
	@return An iterator to a read-only list of values of <code>rdf:type</code>
		properties, each item of which is expected to be an <code>RDFResource</code>
		of the type. 
	*/
	public static Iterator<RDFObject> getTypeIterator(final RDFResource resource)
	{
		return resource.getPropertyValueIterator(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME); //return an iterator to the type properties		
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
		not a <code>RDFResource</code> (such as a <code>RDFLiteral</code>), which would
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

	/**Retrieves the literal value of the resource. If this resource has more than
		one property of <code>rdf:value</code>, it is undefined which of these
		property values will be returned.
	@param resource The resource the value of which will be returned.
	@return The value of the resource, or <code>null</code> if there is no value
		or the value is not a literal.
	*/
	public static RDFLiteral getValue(final RDFResource resource)
	{
		return asLiteral(resource.getPropertyValue(RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME)); //get the value of the value property only if it is a literal
	}

	/**Replaces all <code>rdf:value</code> properties of the resource with a new
		property with the given value. No language is specified.
	@param resource The resource for which the property should be set.
	@param value A string value.
	*/
	public static void setValue(final RDFResource resource, final String value)
	{
		setValue(resource, value, (Locale)null);	//set the value without specifying a language
	}

	/**Replaces all <code>rdf:value</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the property should be set.
	@param value A string value.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	*/
	public static void setValue(final RDFResource resource, final String value, final Locale language)
	{
		resource.setProperty(RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME, value, language); //replace all value properties with a literal value
	}

	/**Replaces all <code>rdf:value</code> properties of the resource with a new
		property with the given literal.
	@param resource The resource for which the property should be set.
	@param literal A literal value.
	*/
	public static void setValue(final RDFResource resource, final RDFLiteral literal)
	{
		resource.setProperty(RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME, literal); //replace all value properties with a literal value
	}

	/**Replaces all <code>rdf:value</code> properties of the resource with a new
		property with the given value.
	@param resource The resource for which the label properties should be replaced.
	@param lexicalForm The lexical form of the literal value that will be stored
		in a <code>RDFTypedLiteral</code> created from the RDF data model.
	@param datatypeURI The reference URI identifying the datatype of this literal.
	*/
/*G***del if we don't need or want this method
	public static void setValue(final RDFResource resource, final String lexicalForm, final URI datatypeURI)
	{
		setProperty(rdf, resource, RDF_NAMESPACE_URI, VALUE_PROPERTY_NAME, lexicalForm, datatypeURI); //replace all value properties with a typed literal value
	}
*/

	/**Determines if the given resource is the empty list.
	@param rdf The RDF data model in which to locate the resource.
	@return A list resource with the reference URI &amp;rdf;nil. 
	*/
	public static boolean isNil(final RDFResource listResource)
	{
		return listResource!=null && NIL_RESOURCE_URI.equals(listResource.getReferenceURI());	//see if the resource has the nil URI 
	}

	/**Determines whether a given resource is of a particular type.
		Every type property of the resource is checked.
	@param resource The resource the type type of which to check.
	@param typeURI The reference URI of the type resource.
	@return <code>true</code> if the resource has the indicated type property.
	*/
	public static boolean isType(final RDFResource resource, final URI typeURI)
	{
		return resource.hasPropertyResourceValue(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME, typeURI); //determine if the resource has a type property of the given URI
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
		return resource.hasPropertyResourceValue(RDF_NAMESPACE_URI, TYPE_PROPERTY_NAME, RDFUtilities.createReferenceURI(typeNamespaceURI, typeLocalName)); //determine if the resource has a type property of the URI from the given namespace and local name
	}

	/**Retreives a nil list resource from the data model, or creates one and
		adds it if one does not exist in the data model.
	@param rdf The RDF data model in which to locate the resource.
	@return A list resource with the reference URI <code>&amp;rdf;nil</code>. 
	*/
/*G***del if not needed
	public static RDFListResource locateNilResource(final RDF rdf)	//G***do we still want this, after the new way we allow multiple nil resources in RDFListResource?
	{
		return (RDFListResource)rdf.locateResource(RDF_NAMESPACE_URI, NIL_RESOURCE_LOCAL_NAME);	//locate the nil resource
	}
*/
	
	/**Sets a typed literal property from a lexical form and datatype URI by first
		removing all such properties and then adding a new property.
	@param resource The resource to which the type should be added.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param lexicalForm The lexical form of the literal value that will be stored
		in a <code>RDFTypedLiteral</code> created from the RDF data model.
	@param datatypeURI The reference URI identifying the datatype of this literal.
	@return The added property value.
	*/
/*G***del if we don't want or need this method
	public static RDFTypedLiteral setProperty(final RDFResource resource, final URI propertyNamespaceURI, final String propertyLocalName, final String lexicalForm, final URI datatypeURI)
	{
		return (RDFTypedLiteral)resource.addProperty(locateResource(resource, propertyNamespaceURI, propertyLocalName), rdf.createTypedLiteral(lexicalForm, datatypeURI)); //create a property from the namespace URI and local name, create a new typed literal value, and set the property
	}
*/

	/**Sets a language tag based upon a given locale using a litearl to represent
		a language identifier according as defined in
		<a href="http://www.ietf.org/rfc/rfc1766.txt">RFC 1766</a>,
		"Tags for the Identification of Languages".
	This is a convenience method to first remove all such properties and then
		add a new property.
	@param rdf The RDF data model.
	@param resource The resource to which the value should be added.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param language A locale the language tag of which will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
/*G***del if not needed
	public static RDFLiteral setProperty(final RDF rdf, final RDFResource resource, final URI propertyNamespaceURI, final String propertyLocalName, final Locale language)
	{
		return setProperty(rdf, resource, propertyNamespaceURI, propertyLocalName, LocaleUtilities.getLanguageTag(language)); //store the language tag as a literal
	}
*/

	/**Determines a string value for resource appropriate for representation.
	The label is determined in the following sequence:
	<ol>
		<li>The lexical form of any literal <code>rdfs:label</code>.</li>
		<li>The reference URI.</li>
		<li>The lexical form of any literal <code>rdfs:value</code>.</li>
		<li>The Java string representation of the resource as given by its <code>toString()</code> method.</li>
	</ol>
	@param resource The resource the label of which will be returned.
	@return The label of the resource, or <code>null</code> if there is no label
		or the label is not a literal.
	*/
	public static String getDisplayLabel(final RDFResource resource)
	{
		final RDFLiteral labelLiteral=RDFSUtilities.getLabel(resource);	//see if there is an rdfs:label property
		if(labelLiteral!=null)	//if a rdfs:label property was provided
		{
			return labelLiteral.getLexicalForm();	//return the rdfs:label property's lexical form
		}
		final URI referenceURI=resource.getReferenceURI();	//get the resource's reference URI
		if(referenceURI!=null)	//if the resource has a reference URI
		{
			return referenceURI.toString();	//return the reference URI
		}
		final RDFLiteral valueLiteral=RDFUtilities.getValue(resource);	//get the rdfs:value property, if any
		if(valueLiteral!=null)	//if a rdfs:value property was provided
		{
			return valueLiteral.getLexicalForm();	//return the rdfs:value property's lexical form
		}
		return resource.toString();	//there's nothing else to do but return the resource's string value
	}
	
	/**Determines the RDF namespace of the given reference URI.
	For most reference URIs, this is the URI formed by all the the reference URI
		characters up to and including the last character that is not a valid
		XML name character. If all characters are valid XML name characters,
		the last non-alphanumeric character is used as a delimiter.
	@param referenceURI The reference URI for which a namespace URI should be
		determined.
	@return The namespace URI of the reference URI, or <code>null</code> if the
		namespace URI could not be determined.
	*/
	public static URI getNamespaceURI(final URI referenceURI)
	{
			//TODO do something special for certain namespaces such as for XLink that do not follow the rules
		final String referenceURIString=referenceURI.toString();	//get a string version of the reference URI
		for(int i=referenceURIString.length()-1; i>=0; --i)	//look at each character in the reference URI, starting at the end
		{
			final char character=referenceURIString.charAt(i);	//get this character
			if(!XMLUtilities.isNameChar(character) && character!=URIConstants.ESCAPE_CHAR)	//if this is not a name character (but it isn't the URI escape character, either)
			{
				return URI.create(referenceURIString.substring(0, i+1));	//create a URI using everything up to and including the last non-XML name character
			}
		}
			//if we still don't know the namespace URI, look for the last non-alphanumeric character
		for(int i=referenceURIString.length()-1; i>=0; --i)	//look at each character in the reference URI, starting at the end
		{
			if(!Character.isLetter(referenceURIString.charAt(i)) && !Character.isDigit(referenceURIString.charAt(i)))	//if this is not a letter or a number
			{
					//TODO correctly check for a URI syntax error here
				return URI.create(referenceURIString.substring(0, i+1));	//create a URI using everything up to and including the last non-XML name character
			}
		}
		return null;	//show that we couldn't determine a namespace URI from the given reference URI
	}

	/**Determines the local name to be used for the given reference URI,
		suitable for XML serialization.
	For most reference URIs, this is the name formed by all the the reference URI
		characters after but not including the last character that is not a valid
		XML name character. If all characters are valid XML name characters,
		the last non-alphanumeric character is used as a delimiter.
	@param referenceURI The reference URI for which a local name should be
		determined.
	@return The local name of the reference URI, or <code>null</code> if a local
		name could not be determined.
	 */
	public static String getLocalName(final URI referenceURI)
	{
			//TODO do something special for certain namespaces such as for XLink that do not follow the rules
		final String referenceURIString=referenceURI.toString();	//get a string version of the reference URI
		for(int i=referenceURIString.length()-1; i>=0; --i)	//look at each character in the reference URI, starting at the end
		{
			final char character=referenceURIString.charAt(i);	//get this character
			if(!XMLUtilities.isNameChar(character) && character!=URIConstants.ESCAPE_CHAR)	//if this is not a name character (but it isn't the URI escape character, either)
			{
				return referenceURIString.substring(i+1);	//create a local name using everything after the last non-XML name character
			}
		}
			//if we still don't know the local name, look for the last non-alphanumeric character
		for(int i=referenceURIString.length()-1; i>=0; --i)	//look at each character in the reference URI, starting at the end
		{
			if(!Character.isLetter(referenceURIString.charAt(i)) && !Character.isDigit(referenceURIString.charAt(i)))	//if this is not a letter or a number
			{
				return referenceURIString.substring(i+1);	//create a local name using everything after the last non-XML name character
			}
		}
		return null;	//show that we couldn't determine a local name from the given reference URI
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

	/**Converts an RDF resource to an XML string. If an error occurs converting
		the XML document document to a string, the normal XML object string will be
		returned.
	@param resource The RDF resource to convert.
	@return A string representation of an XML serialization of the RDF resource.
	*/
	public static String toString(final RDFResource resource)
	{
		final ByteArrayOutputStream outputStream=new ByteArrayOutputStream(); //create an output stream of bytes
		final RDFXMLifier rdfXMLifier=new RDFXMLifier();  //create an object to turn the RDF into XML
/*G***setup namespace prefixes if we can
				//setup the Mentoract namespace prefixes
		rdfXMLifier.getNamespacePrefixMap().put(MENTORACT_PROTOCOL_NAMESPACE_URI, MENTORACT_PROTOCOL_DEFAULT_PREFIX);
		rdfXMLifier.getNamespacePrefixMap().put(MentoractResourceURIConstants.MENTORACT_RESOURCE_NAMESPACE_URI, MentoractResourceURIConstants.MENTORACT_RESOURCE_DEFAULT_PREFIX);
*/
			//create an XML document from the RDF
		final Document document=rdfXMLifier.createDocument(resource, new XMLDOMImplementation());  //G***try to make this XML parser agnostic
		return XMLUtilities.toString(document); //convert the XML document to a string and return it
	}

}