package com.garretwilson.rdf;

import java.lang.ref.*;
import java.net.URI;
import java.util.*;
import com.garretwilson.util.*;

/**Represents the default implementation of an RDF resource.
<p>This class provides compare functionality that sorts according to the reference
	URI.</p>
<p>This resource keeps a weak reference to the data model that created it, if
	any.</p> 
@author Garret Wilson
*/
public class DefaultRDFResource extends DefaultResource implements RDFResource, Comparable
{

	/**The XML namespace URI.*/
	private URI namespaceURI;

		/**@return The XML namespace URI used in serialization, or <code>null</code>
		  if no namespace URI was used or there was no namespace.*/
		public URI getNamespaceURI() {return namespaceURI;}

	/**The XML local name.*/
	private String localName;

		/**@return The XML local name used in serialization, or <code>null</code>
		  if no namespace URI and local name was used.*/
		public String getLocalName() {return localName;}

	/*The data model that created this resource, or <code>null</code> if the
		resource was created separate from a data model.
	*/
	private Reference rdfReference;

		/**@return The RDF data model with which this resource is associated, or
			<code>null</code> if this resource is not associated with a data model.
		*/
		public RDF getRDF()
		{
			return rdfReference!=null ? (RDF)rdfReference.get() : null;	//return the RDF, if any, indicated by the reference
		}

	/**The list of properties, each of which is a <code>RDFPropertyValuePair</code>,
		with the name being the property predicate and the value being the property
		value.
	*/
	protected final List propertyList=new ArrayList();  //G***should this really be protected, and not private? currently only used by RDFSequenceResource

	/**@return The number of properties this resource has.*/
	public int getPropertyCount() {return propertyList.size();}

	/**@return An iterator that allows traversal of all properties, each of which
		is a <code>RDFPropertyValuePair</code>, with the name being the property predicate
		and the value being the property value.
	*/
	public ListIterator getPropertyIterator()
	{
		return propertyList.listIterator(); //return an iterator to the properties
	}

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of <code>propertyResource</code>.
	@param propertyResource The property resource.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final RDFResource propertyResource)
	{
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to look at the properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
		  if(propertyValuePair.getProperty().equals(propertyResource))  //if this resource is the same as the one requested
				return propertyValuePair.getPropertyValue(); //return the value of the property as an RDF object
		}
		return null;  //show that we couldn't find such a property
	}

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of <code>propertyURI</code>.
	@param propertyURI The reference URI of the property resource.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final URI propertyURI)
	{
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to look at the properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
		  if(propertyURI.equals(propertyValuePair.getProperty().getReferenceURI()))  //if this resource is that identified by the property URI
				return propertyValuePair.getPropertyValue(); //return the value of the property as an RDF object
		}
		return null;  //show that we couldn't find such a property
	}

	/**Searches and returns the first occurring property value that appears as
		an RDF statement object with a predicate of a property URI formed by the
		given namespace URI and local name. This is a convenience function that
		creates a property URI from an XML qualified name automatically for searching.
	@param namespaceURI The XML namespace URI used in the serialization.
	@param localName The XML local name used in the serialization.
	@return The value of the property, either a <code>RDFResource</code> or a
		<code>RDFLiteral</code>, or <code>null</code> if this resource has no such
		property.
	*/
	public RDFObject getPropertyValue(final URI namespaceURI, final String localName)
	{
		return getPropertyValue(RDFUtilities.createReferenceURI(namespaceURI, localName)); //look for the property, combining the namespace URI and the local name for the reference URI
	}

	/**Searches and returns an iterator of all property values that appear as RDF
		statement objects with a predicate of <code>propertyURI</code>.
	@param propertyURI The reference URI of the property resources.
	@return An iterator to a read-only list of values of properties, each either a
		<code>RDFResource</code> or a <code>RDFLiteral</code>.
	*/
	public Iterator getPropertyValueIterator(final URI propertyURI)  //G***maybe fix to make the iterator dynamic to the RDF data model
	{
		final List propertyValueList=new ArrayList(); //cerate a list in which to store the property values
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to look at the properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
		  if(propertyURI.equals(propertyValuePair.getProperty().getReferenceURI()))  //if this resource is that identified by the property URI
			{
				propertyValueList.add(propertyValuePair.getPropertyValue()); //add the value of the property to the value list
			}
		}
		return Collections.unmodifiableList(propertyValueList).iterator();  //return an iterator to the list of properties
	}

	/**Searches and returns an iterator of all property values that appear as
		RDF statement objects with a predicate of a property URI formed by the
		given namespace URI and local name. This is a convenience function that
		creates a property URI automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@return An iterator to a read-only list of values of properties, each either a
		<code>RDFResource</code> or a <code>RDFLiteral</code>.
	*/
	public Iterator getPropertyValueIterator(final URI namespaceURI, final String localName)
	{
		return getPropertyValueIterator(RDFUtilities.createReferenceURI(namespaceURI, localName)); //look for the property, combining the namespace URI and the local name for the reference URI
	}

	/**Determines if the resource has the given property with the resource
		identified by the given URI.
	@param propertyURI The reference URI of the property resource.
	@param propertyValueURI The URI of the resource to which the property should
		be compared.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyResourceValue(final URI propertyURI, final URI propertyValueURI)
	{
//G***del Debug.trace("looking for property value for property: ", propertyURI);  //G***del
//G***del Debug.trace("looking for property value: ", propertyValue);  //G***del
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to look at the properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
//G***del Debug.trace("looking at name/value pair: ", nameValuePair);  //G***del
//G***del Debug.trace("looking at name: ", nameValuePair.getName());  //G***del
//G***del Debug.trace("looking at value: ", nameValuePair.getValue());  //G***del
		  if(propertyURI.equals(propertyValuePair.getProperty().getReferenceURI()))  //if this resource is that identified by the property URI
			{
				if(propertyValuePair.getPropertyValue() instanceof RDFResource)	//if the value is a resource
				{
					//if the resource value has the correct reference URI
					if(propertyValueURI.equals(((RDFResource)propertyValuePair.getPropertyValue()).getReferenceURI())) 
						return true;
				}
			}
		}
		return false; //show that there was no matching property
	}

	/**Determines if the resource has the given property with the resource
		identified by the given URI.
	This is a convenience function that creates a property URI from an XML
		qualified name automatically for searching.
	@param namespaceURI The XML namespace URI that represents part of the reference URI.
	@param localName The XML local name that represents part of the reference URI.
	@param propertyValueURI The URI of the resource to which the property should
		be compared.
	@return <code>true</code> if the specified property is set to the specified
		value.
	*/
	public boolean hasPropertyResourceValue(final URI namespaceURI, final String localName, final URI propertyValueURI)
	{
		return hasPropertyResourceValue(RDFUtilities.createReferenceURI(namespaceURI, localName), propertyValueURI); //check the property, combining the namespace URI and the local name for the reference URI
	}
	
	/**Adds a property by creating a <code>RDFPropertyValuePair</code> from the given
		property and value. For each property, this resource serves as the subject
		of an RDF statement with the property as the predicate and the value as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFObject addProperty(final RDFResource property, final RDFObject value)
	{
		final RDFPropertyValuePair propertyValuePair=new RDFPropertyValuePair(property, value); //create a name/value pair with the property and value
		if(!propertyList.contains(propertyValuePair)) //if there is not already this property with this value
			propertyList.add(propertyValuePair);  //add the property and value to the list
		return value; //return the value we added
	}

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. For each property, this resource serves
		as the subject of an RDF statement with the property as the predicate and
		the value, stored as a literal, as the object. No language is specified.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final RDFResource property, final String literalValue)
	{
		return addProperty(property, literalValue, null);	//add the plain literal with no language specified
	}

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. For each property, this resource serves
		as the subject of an RDF statement with the property as the predicate and
		the value, stored as a literal, as the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final RDFResource property, final String literalValue, final Locale language)
	{
		return (RDFLiteral)addProperty(property, new RDFPlainLiteral(literalValue, language)); //create a new literal value and store the property
	}

	/**Removes all properties with the given URI.
	@param propertyURI The reference URI of the property resource of the
		properties to be removed.
	@return The number of properties removed.
	*/
	public int removeProperties(final URI propertyURI)
	{
		int propertiesRemovedCount=0;	//we haven't removed any properties, yet
		final Iterator propertyIterator=getPropertyIterator();  //get an iterator to look at the properties
		while(propertyIterator.hasNext()) //while there are more properties
		{
			final RDFPropertyValuePair propertyValuePair=(RDFPropertyValuePair)propertyIterator.next(); //get the next name/value pair
			if(propertyURI.equals(propertyValuePair.getProperty().getReferenceURI()))  //if this resource is that identified by the property URI
			{
				propertyIterator.remove();	//remove this property
				++propertiesRemovedCount;	//show that we removed another property
			}
		}
		return propertiesRemovedCount;	//return the number of properties we removed
	}
	
	/**Removes all properties with the given URI.
	@param namespaceURI The XML namespace URI that represents part of the
		reference URI of the properties to be removed.
	@param localName The XML local name that represents part of the reference URI
		of the properties to be removed.
	@return The number of properties removed.
	*/
	public int removeProperties(final URI namespaceURI, final String localName)
	{
		return removeProperties(RDFUtilities.createReferenceURI(namespaceURI, localName)); //remove the property, combining the namespace URI and the local name for the reference URI
	}

	/**Sets a property by removing all property values for the given property and
		creating a new <code>RDFPropertyValuePair</code> from the given
		property and value.
	@param property A property resource; the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, RDFObject)
	*/
	public RDFObject setProperty(final RDFResource property, final RDFObject value)
	{
		removeProperties(property.getReferenceURI());	//remove all existing object values for this property
		return addProperty(property, value);	//add the given property and value
	}

	/**Sets a plain literal property from a string by removing all property values
		for the given property and creating a new <code>RDFPropertyValuePair</code>
		from the given property and value. No language is specified.
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, String)
	*/
	public RDFLiteral setProperty(final RDFResource property, final String literalValue)
	{
		return setProperty(property, literalValue, null);	//set the property without specifying a language
	}

	/**Sets a plain literal property from a string by removing all property values
		for the given property and creating a new <code>RDFPropertyValuePair</code>
		from the given property and value.
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, String)
	*/
	public RDFLiteral setProperty(final RDFResource property, final String literalValue, final Locale language)
	{
		removeProperties(property.getReferenceURI());	//remove all existing object values for this property
		return addProperty(property, literalValue, language);	//add the given property and value
	}

	/**Copy constructor. Constructs a resource with an identical reference URI
		and identical properties. Properties themselves are not cloned.
	@param resource The resource to copy.
	*/
/*G***del if not needed
	DefaultRDFResource(final RDFResource resource)	//TODO delete this constructor when we can, now that we can simply change the reference URI at any time
	{
		super(resource.getReferenceURI());  //construct the parent class with the reference URI
		namespaceURI=resource.getNamespaceURI(); //copy the namespace URI
		localName=resource.getLocalName(); //copy the local name
		CollectionUtilities.addAll(propertyList, resource.getPropertyIterator()); //add all the property values from the resource being copied
//G***del when works		propertyList.addAll(resource.propertyList); //add all the property values from the resource being copied
	}
*/

	/**Default constructor that creates a resource without a reference URI.
	@see RDF#createResource
	*/
	public DefaultRDFResource()
	{
		this((URI)null);	//create a resource without a reference URI
	}

	/**Constructs a resource with a reference URI.
	@param referenceURI The reference URI for the new resource.
	@see RDF#createResource
	*/
	public DefaultRDFResource(final URI referenceURI)
	{
		this(null, referenceURI);	//construct the class with no data model
	}

	/**Constructs a resource with a reference URI from a data model.
	@param rdf The data model with which this resource should be associated.
	@param referenceURI The reference URI for the new resource.
	@see RDF#createResource
	*/
	public DefaultRDFResource(final RDF rdf, final URI referenceURI)
	{
		this(rdf, referenceURI, null, null);	//construct the class with no namespace URI or local name
	}

	/**Convenience constructor that constructs a resource using a namespace URI
		and local name which will be combined to form the reference URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	@see RDF#createResource
	*/
	public DefaultRDFResource(final URI newNamespaceURI, final String newLocalName)
	{
		this((RDF)null, newNamespaceURI, newLocalName);  //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**Convenience constructor that constructs a resource using a namespace URI
		and local name which will be combined to form the reference URI.
	@param rdf The data model with which this resource should be associated.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	@see RDF#createResource
	*/
	public DefaultRDFResource(final RDF rdf, final URI newNamespaceURI, final String newLocalName)
	{
		this(rdf, RDFUtilities.createReferenceURI(newNamespaceURI, newLocalName), newNamespaceURI, newLocalName);  //do the default construction, combining the namespace URI and the local name for the reference URI
	}

	/**Constructs a resource with a reference URI and separate namespace URI and
		local name.
	@param referenceURI The reference URI for the new resource.
	@param newNamespaceURI The XML namespace URI used in the serialization, or
		<code>null</code> if the namespace URI is not known.
	@param newLocalName The XML local name used in the serialization, or
		<code>null</code> if the local name is not known.
	@see RDF#createResource
	*/
	DefaultRDFResource(final URI referenceURI, final URI newNamespaceURI, final String newLocalName)
	{
		this(null, referenceURI, newNamespaceURI, newLocalName);	//construct the resource with no known data model
	}

	/**Constructs a resource with a reference URI and separate namespace URI and
		local name from a data model.
	@param rdf The data model with which this resource should be associated.
	@param referenceURI The reference URI for the new resource.
	@param newNamespaceURI The XML namespace URI used in the serialization, or
		<code>null</code> if the namespace URI is not known.
	@param newLocalName The XML local name used in the serialization, or
		<code>null</code> if the local name is not known.
	@see RDF#createResource
	*/
	DefaultRDFResource(final RDF rdf, final URI referenceURI, final URI newNamespaceURI, final String newLocalName)
	{
		super(referenceURI);  //construct the parent class with the reference URI
		rdfReference=rdf!=null ? new WeakReference(rdf) : null;	//save the RDF, if any, using a weak reference
		namespaceURI=newNamespaceURI; //store the namespace URI
		localName=newLocalName; //store the local name
	}

	/**Compares this object to another object.
	<p>This method determines order based upon the reference URI of the resource,
		if any.</p>
	@param object The object with which to compare the component. This must be
		another <code>Resource</code> object.
	@return A negative integer, zero, or a positive integer as this resource
		reference URI is less than, equal to, or greater than the reference URI of
		the specified resource, respectively.
	@exception ClassCastException Thrown if the specified object's type is not
		a <code>Resource</code>.
	@see #getReferenceURI
	*/
	public int compareTo(Object object) throws ClassCastException	//TODO fix, and maybe transfer back up to DefaultResource
	{
//G***fix		final Resource otherResource=(Resource)object;	//cast the object to a resource
		if(getReferenceURI()!=null && ((Resource)object).getReferenceURI()!=null)	//if both resources have reference URIs				
			return getReferenceURI().compareTo(((Resource)object).getReferenceURI()); //compare reference URIs
		else	//if one of the two resources doesn't have a reference URI
			return hashCode()-object.hashCode();	//make an arbitrary comparison G***fix
	}

}