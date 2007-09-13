package com.garretwilson.urf;

import java.net.URI;
import java.util.concurrent.locks.*;

import com.garretwilson.net.BoundPropertyResource;
import static com.garretwilson.urf.URF.*;

/**The default implementation of an URF resource.
This class provides compare functionality that sorts according to the resource URI.
@author Garret Wilson
*/
public class DefaultURFResource extends BoundPropertyResource implements URFResource, ReadWriteLock, Cloneable
{

	/**The decorated read write lock.*/
	private final ReadWriteLock readWriteLock=new ReentrantReadWriteLock();

	/**Returns the lock used for reading.
	@return the lock used for reading.
	*/
	public Lock readLock() {return readWriteLock.readLock();}

	/**Returns the lock used for writing.
	@return the lock used for writing.
	*/
	public Lock writeLock() {return readWriteLock.writeLock();}

	/**The decorated scope for this resource.*/
	private final URFScope scope=new DefaultURFScope(readWriteLock, null);

	/**Default constructor with no URI.*/
	public DefaultURFResource()
	{
		this(null);	//create a resource without a URI
	}

	/**Constructs a resource with a URI.
	@param uri The URI for the resource.
	*/
	public DefaultURFResource(final URI uri)
	{
		super(uri);	//construct the class with no data model
	}

	/**@return The parent scope of this scope, or <code>null</code> if this scope has no parent scope.*/
	public URFScope getParentScope() {return scope.getParentScope();}

	/**Retrieves the scope of a given property and value of this scope.
	@param propertyURI The URI of the property the scope of which to retrieve.
	@param propertyValue The value of the property the scope of which to retrieve.
	@return The scope of the given property and value, or <code>null</code> if no such property and value exists.
	*/
	public URFScope getScope(final URI propertyURI, final URFResource propertyValue) {return scope.getScope(propertyURI, propertyValue);}

	/**@return Whether this scope has properties.*/
	public boolean hasProperties() {return scope.hasProperties();}

	/**@return The number of properties this scope has.*/
	public int getPropertyCount() {return scope.getPropertyCount();}

	/**Retrieves an iterable to all property URIs.
	Any deletions made to the returned iterable will result in all corresponding properties being removed.
	@return An iterable to all property URIs.
	*/
	public Iterable<URI> getPropertyURIs() {return scope.getPropertyURIs();}

	/**Retrieves the first value of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which a value should be returned.
	@return The first value of the property with the given URI, or <code>null</code> if there is no such property.
	*/
	public URFResource getPropertyValue(final URI propertyURI) {return scope.getPropertyValue(propertyURI);}

	/**Retrieves an iterable to the values of the property with the given URI.
	All ordered properties will be returned in their correct order before any non-ordered properties.
	Unordered properties will be returned in an arbitrary order. 
	@param propertyURI The URI of the property for which values should be returned.
	@return An iterable to all values of the property with the given URI.
	*/
	public Iterable<URFResource> getPropertyValues(final URI propertyURI) {return scope.getPropertyValues(propertyURI);}

	/**Adds a property value for the property with the given URI.
	If the given property and value already exists, no action occurs.
	@param propertyURI The URI of the property of the value to add.
	@param propertyValue The value to add for the given property.
	*/
	public void addPropertyValue(final URI propertyURI, final URFResource propertyValue) {scope.addPropertyValue(propertyURI, propertyValue);}

	/**Sets a property value for the property with the given URI.
	@param propertyURI The URI of the property of the value to set.
	@param propertyValue The value to set for the given property.
	*/
	public void setPropertyValue(final URI propertyURI, final URFResource propertyValue) {scope.setPropertyValue(propertyURI, propertyValue);}

	/**Retrieves the types declared for this resource, if any.
	@return An iterable to all types declared for this resource.
	@see URF#TYPE_PROPERTY_URI
	*/
	public Iterable<URFResource> getTypes()
	{
		return getPropertyValues(TYPE_PROPERTY_URI);	//return all the type property values
	}
}