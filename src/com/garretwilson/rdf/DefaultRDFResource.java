package com.garretwilson.rdf;

import java.lang.ref.*;
import java.net.URI;
import java.util.*;
import com.garretwilson.model.*;

/**Represents the default implementation of an RDF resource.
<p>This class provides compare functionality that sorts according to the reference
	URI.</p>
<p>This resource keeps a weak reference to the data model that created it, if
	any.</p> 
@author Garret Wilson
*/
public class DefaultRDFResource extends DefaultResource implements RDFResource, Cloneable
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

		/**Associates this resource with an RDF data model.
		@param rdf The RDF data model with which to associate this resource, or
			<code>null</code> if this resource should not be associated with any
			RDF data model.
		*/
		public void setRDF(final RDF rdf)
		{
			rdfReference=rdf!=null ? new WeakReference(rdf) : null;	//save the RDF, if any, using a weak reference			
		}

	/**The list of properties, each of which is a <code>RDFPropertyValuePair</code>,
		with the name being the property predicate and the value being the property
		value.
	*/
	protected ArrayList propertyList=new ArrayList();  //G***should this really be protected, and not private? currently only used by RDFSequenceResource

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

	/**Adds a property by creating a <code>RDFPropertyValuePair</code> from the given
		property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject
		of an RDF statement with the property as the predicate and the value as
		the object.
	<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
	<p>If an equivalent property already exists, no action is taken.</p>
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param value A property value; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFObject addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final RDFObject value)
	{
		return addProperty(RDFUtilities.locateResource(this, propertyNamespaceURI, propertyLocalName), value); //create a property from the namespace URI and local name, then add the actual property value
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
		from the given property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject of an RDF statement
		with the property as the predicate and the value, stored as a literal, as
		the object. No language is specified
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue)
	{
		return addProperty(RDFUtilities.locateResource(this, propertyNamespaceURI, propertyLocalName), literalValue); //create a new literal value and add the property
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

	/**Adds a plain literal property from a string by creating a <code>RDFPropertyValuePair</code>
		from the given property and value. This is a convenience method that allows specification
		of the property resource through a namespace URI and a local name.
		For each property, this resource serves as the subject of an RDF statement
		with the property as the predicate and the value, stored as a literal, as
		the object.
		<p>Note that the property is not simply a property URI &mdash; it is a
		resource that is identified by the property URI.</p>
		<p>If an equivalent property already exists, no action is taken.</p>
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	*/
	public RDFLiteral addProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue, final Locale language)
	{
		return addProperty(RDFUtilities.locateResource(this, propertyNamespaceURI, propertyLocalName), literalValue, language); //create a new literal value and add the property
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
		property and value. If no value is given, all such properties are removed.
	@param property A property resource&mdash;the predicate of an RDF statement.
	@param value A property value&mdash;the object of an RDF statement&mdash;or
		<code>null</code> if all such properties should be removed with nothing
		to replace them.
	@return The added property value.
	@see #removeProperties(URI)
	@see #addProperty(RDFResource, RDFObject)
	*/
	public RDFObject setProperty(final RDFResource property, final RDFObject value)
	{
		removeProperties(property.getReferenceURI());	//remove all existing object values for this property
		if(value!=null)	//if there is a value to add
			return addProperty(property, value);	//add the given property and value
		else	//if no value was given
			return null;	//show that no value was added
	}

	/**Sets a property by first removing all such properties and then adding
		a new property. If no value is given, all such properties are removed.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param value A property value&mdash;the object of an RDF statement&mdash;or
		<code>null</code> if all such properties should be removed with nothing
		to replace them.
	@return The added property value.
	*/
	public RDFObject setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final RDFObject value)
	{
		return setProperty(RDFUtilities.locateResource(this, propertyNamespaceURI, propertyLocalName), value); //create a property from the namespace URI and local name, then set the actual property value
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

	/**Sets a plain literal property from a string by first removing all such
		properties and then adding a new property. No language is specified.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@return The added property value.
	*/
	public RDFLiteral setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue)
	{
		return setProperty(RDFUtilities.locateResource(this, propertyNamespaceURI, propertyLocalName), literalValue); //create a new literal value and set the property
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

	/**Sets a plain literal property from a string by first removing all such
		properties and then adding a new property.
	@param propertyNamespaceURI The XML namespace URI used in the serialization
		of the property resource that is the predicate of an RDF statement.
	@param propertyLocalName The XML local name used in the serialization of the
		property resource that is the predicate of an RDF statement.
	@param literalValue A literal property value that will be stored in a
		<code>RDFLiteral</code>; the object of an RDF statement.
	@param language The language of the plain literal, or <code>null</code> if
		no language should be specified.
	@return The added property value.
	*/
	public RDFLiteral setProperty(final URI propertyNamespaceURI, final String propertyLocalName, final String literalValue, final Locale language)
	{
		return setProperty(RDFUtilities.locateResource(this, propertyNamespaceURI, propertyLocalName), literalValue, language); //create a new literal value and set the property
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

	/**Constructs a resource from a data model.
	@param rdf The data model with which this resource should be associated.
	*/
	public DefaultRDFResource(final RDF rdf)
	{
		this(rdf, null);	//construct the resource with no reference URI
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
		namespaceURI=newNamespaceURI; //store the namespace URI
		localName=newLocalName; //store the local name
		setRDF(rdf);	//associate the resource with the given RDF data model, if any
	}

	/**Copy constructor.
	@param resource The resource that provides the data for the new copy.
	*/
/*G***del if not needed
	private DefaultRDFResource(final DefaultRDFResource resource)
	{
		this(resource.getRDF(), resource.getReferenceURI(), resource.getNamespaceURI(), resource.getLocalName());  //construct a copy of the resource
		propertyList.addAll(resource.propertyList);	//copy the properties
	}
*/

	/**Returns a hash code value for the resource. 
		If this resource has a reference URI, the default hash code is returned
		(i.e. the hash code of the refeference URI). If this resource has no
		reference URI, the number of properties is returned.
	@return The hash code of the reference URI, or the number of properties if
		this resource has no reference URI.
	*/
	public int hashCode()
	{
		return getReferenceURI()!=null ? super.hashCode() : getPropertyCount();	//return the normal hash code if we have a reference URI, or the number of properties if we have no reference URI 
	}

	/**If this resource has a reference URI, compares using the superclass
		functionality (comparing reference URIs). Otherwise, if the other object
		is also an RDF resource, compares properties.
	@param object The object with which to compare this RDF resource; should be
		another resource.
	@return <code>true<code> if this resource equals that specified in
		<code>object</code>.
	@see #getReferenceURI
	@see RDFUtilities#getValue(RDFResource)
	*/
	public boolean equals(final Object object)
	{
		if(getReferenceURI()==null && object instanceof RDFResource)	//if we don't have a reference URI, and the other object is an RDF resource
		{
			//TODO compare all properties; right now, we only compare rdf:value so that other code will work
			final RDFResource rdfResource=(RDFResource)object;	//cast the other object to an RDF resource
			final RDFObject value1=RDFUtilities.getValue(this);	//get our rdf:value
			if(value1!=null)	//if we have a value
			{
				return value1.equals(RDFUtilities.getValue(rdfResource));	//compare our rdf:value with the other rdf:value			
				
			}
		}
		return super.equals(object);	//if we have a reference URI or the other object isn't an RDF resource, do a default comparison (usually using reference URIs)
	}

	/**@return A copy of this resource with the same URI and identical properties.*/
	public Object clone()
	{
		try
		{ 
			DefaultRDFResource resource=(DefaultRDFResource)super.clone();	//create a cloned copy of this resource
			resource.propertyList=(ArrayList)propertyList.clone();	//clone the property list
			return resource;	//return the cloned resource
		}
		catch(CloneNotSupportedException e)
		{ 
			throw new AssertionError("Cloning is unexpectedly not supported.");
		}
	}

	/**Returns a string representation of the resource.
	<p>This implementation returns the reference URI, if any, along with the
		lexical form of the literal <code>rdf:value</code> property value, if any.
		If this resource has neither a reference URI or a literal
		<code>rdf:value</code> property value, the default representation will be
		used.</p> 
	@return A string representation of the resource.
	@see RDFUtilities#getValue(RDFResource)
	*/
	public String toString()
	{
		final StringBuffer stringBuffer=new StringBuffer();	//create a string buffer in which to construct a string representation
		final RDFLiteral value=RDFUtilities.getValue(this);	//get the value property, if any
		if(getReferenceURI()!=null || value==null)	//if we have a reference URI and/or no value
		{

			stringBuffer.append(new RDFXMLifier().getLabel(this));	//start with the default string TODO fix with a common RDFXMLifier
//G***testing			stringBuffer.append(super.toString());	//start with the default string
		}
		if(value!=null)	//if we have a value
		{
			if(stringBuffer.length()>0)	//if we added something to the string buffer already
			{
				stringBuffer.append(':').append(' ');	//separate the other information and the value
			}
			stringBuffer.append(value.getLexicalForm());	//append the lexical form of the resource
		}
		return stringBuffer.toString();	//return the string we constructed
	}

}