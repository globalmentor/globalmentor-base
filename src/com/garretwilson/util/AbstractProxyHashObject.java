package com.garretwilson.util;

import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.lang.ObjectUtilities;

/**An object that delegates generation of a hash code and determination of equality to other objects.
This class is useful for creating keys used in hash tables, for example.
This class keeps references to the proxy objects for its entire lifespan.
Hash codes and equality are determined dynamically with each call.
This class is abstract to increase type safety through subclassing.
@author Garret Wilson
*/
public abstract class AbstractProxyHashObject
{
	
	/**The objects being proxied.*/
	private final Object[] proxiedObjects;

	/**Proxied object constructor.
	Any or all of the proxied objects may be <code>null</code>, but the array of objects may not.
	@param proxiedObjects The objects to be proxied for generation of hash code and equality determination.
	@exception NullPointerException if the array of proxied objects is <code>null</code>.
	*/
	public AbstractProxyHashObject(final Object... proxiedObjects)
	{
		this.proxiedObjects=(Object[])checkInstance(proxiedObjects, "Array of proxied objects may not be null.").clone();	//clone the array so the caller will not modify its contents
	}

	/**Returns a hash code value for the object.
 	The hash code is determined dynamically from the proxied objects.
	@return A hash code for this object calculated from the proxied objects.
	*/
	public int hashCode()
	{
		return ObjectUtilities.hashCode(proxiedObjects);	//calculate a hash code from the proxied objects
	}

	/**Indicates whether some other object is "equal to" this one.
	The other object must be a proxy hash object and one of the objects must be an instance of the other to be considered equal.
	Equality is determined by calling the {@link Object#equals(Object)} of the corresponding proxied objects, in order.
	@param object The reference object with which to compare.
	@return <code>true</code> if this object's proxied objects and those of the reference object are equal.
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof AbstractProxyHashObject && (getClass().isInstance(object) || object.getClass().isInstance(this)))	//if the object is a proxy hash object, and one of the objects is an instance of the other
		{
			final Object[] referenceProxiedObjects=((AbstractProxyHashObject)object).proxiedObjects;	//get the proxied objects of the referenced object
			final int proxiedObjectCount=proxiedObjects.length;	//see how many proxied objects there are
			if(referenceProxiedObjects.length==proxiedObjectCount)	//if the other object has the correct number of proxied objects
			{
				for(int i=proxiedObjectCount-1; i>=0; --i)	//for each proxied object
				{
					if(!proxiedObjects[i].equals(referenceProxiedObjects[i]))	//if this proxied object isn't equivalent to its corresponding proxied object
					{
						return false;	//these objects aren't equal
					}
				}
				return true;	//indicate that all proxied objects are equivalent, and thus the objects are considered equal
			}
		}
		return false;	//indicate that the object isn't a proxy hash object or it doesn't have the correct number of proxied objects
	}
}
