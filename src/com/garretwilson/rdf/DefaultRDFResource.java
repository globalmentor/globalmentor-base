package com.garretwilson.rdf;

import java.net.URI;
import java.util.*;
import com.garretwilson.util.*;

/**Represents the default implementation of an RDF resource.
	This class provides compare functionality that sorts according to the reference
	URI.
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
		<code>Literal</code>, or <code>null</code> if this resource has no such
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
		<code>Literal</code>, or <code>null</code> if this resource has no such
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
		<code>Literal</code>, or <code>null</code> if this resource has no such
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
		<code>RDFResource</code> or a <code>Literal</code>.
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
		<code>RDFResource</code> or a <code>Literal</code>.
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

	/**Adds a literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. For each property, this resource serves
		as the subject of an RDF statement with the property as the predicate and
		the value, stored as a literal, as the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param property A property resource; the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>Literal</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public Literal addProperty(final RDFResource property, final String literalValue)
	{
		return (Literal)addProperty(property, new Literal(literalValue)); //create a new literal value and store the property
	}

	/**@return <code>true</code> if this resource is an anonymous resource;
		currently anonymous resources are those that either have no reference URI
		or that have a reference URI that begins with "anonymous:".
	*/
/*G***del when works
	public boolean isAnonymous()
	{
		return getReferenceURI()!=null && getReferenceURI().toString().startsWith("anonymous:"); //return whether there is no URI or the URI begins with "anonymous:" G***use a constant here; fix better
	}
*/

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

	/**Copy constructor. Constructs a resource with an identical reference URI
		and identical properties. Properties themselves are not cloned.
	@param resource The resource to copy.
	*/
	DefaultRDFResource(final RDFResource resource)
	{
		super(resource.getReferenceURI());  //construct the parent class with the reference URI
		namespaceURI=resource.getNamespaceURI(); //copy the namespace URI
		localName=resource.getLocalName(); //copy the local name
		CollectionUtilities.addAll(propertyList, resource.getPropertyIterator()); //add all the property values from the resource being copied
//G***del when works		propertyList.addAll(resource.propertyList); //add all the property values from the resource being copied
	}

	/**Default constructor that creates a resource without a reference URI.
	@see RDF#createResource
	*/
	protected DefaultRDFResource()
	{
		this((URI)null);	//create a resource without a reference URI
	}

	/**Constructs a resource with a reference URI.
	@param referenceURI The reference URI for the new resource.
	@see RDF#createResource
	*/
	protected DefaultRDFResource(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class with the reference URI
		namespaceURI=null;  //show that there is no namespace URI
		localName=null; //show that there is no local name
	}

	/**Constructs a standalone copy of a resource with a different another
		reference URI. All properties are copied but none are cloned.
		<p>Note that if the original resource is part of a larger RDF data model,
		inconsistencies may occur because both own the same properties.</p>
	@param resource The resource from which to create the duplicate.
	@param newReferenceURI The reference URI for the new resource.
//G***del when works	@exception IllegalArgumentException Thrown if the provided reference URI is
//G***del when works		<code>null</code>.
	@see RDF#createResource
	*/
/*G***del when not needed
	public DefaultRDFResource(final RDFResource rdfResource, final URI newReferenceURI) //G***del when works throws IllegalArgumentException
	{
		this(newReferenceURI);  //create a new resource with the given URI
		CollectionUtilities.addAll(propertyList, rdfResource.getPropertyIterator()); //add all the property values from the resource being copied
	}
*/

	/**Constructs a resource from a reference URI.
	@param newReferenceURI The reference URI for the new resource.
	*/
/*G***del
	public Resource(final String newReferenceURI)
	{
		this(newReferenceURI, null);  //construct a resource with no anchor ID
	}
*/

	/**Convenience constructor that constructs a resource using a namespace URI
		and local name which will be combined to form the reference URI.
	@param newNamespaceURI The XML namespace URI used in the serialization.
	@param newLocalName The XML local name used in the serialization.
	@see RDF#createResource
	*/
	protected DefaultRDFResource(final URI newNamespaceURI, final String newLocalName)
	{
		this(RDFUtilities.createReferenceURI(newNamespaceURI, newLocalName));  //do the default construction, combining the namespace URI and the local name for the reference URI
		namespaceURI=newNamespaceURI; //store the namespace URI
		localName=newLocalName; //store the local name
	}

	/**Compares this object to another object.
		This method determines order based upon the reference URI of the resource.
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