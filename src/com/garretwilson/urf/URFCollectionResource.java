package com.garretwilson.urf;

import java.util.Collection;

/**An URF resource that is also a collection.
@param <E> The type of element stored in the collection.
@author Garret Wilson
*/
public interface URFCollectionResource<E extends URFResource> extends URFResource, Collection<E>
{
}