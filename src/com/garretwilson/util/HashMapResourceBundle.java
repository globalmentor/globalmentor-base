package com.garretwilson.util;

import java.util.*;

import static com.globalmentor.java.Objects.*;

/**A resource bundle backed by a hash map.
@author Garret Wilson
@see PropertyResourceBundle
*/
public class HashMapResourceBundle extends ResourceBundle
{

	/**The map containing the resource mappings.*/
	private final Map<String, Object> map;

	/**Default constructor with no parent using a hash map.*/
	public HashMapResourceBundle()
	{
		this((ResourceBundle)null);	//construct the class without a parent 
	}

	/**Parent constructor using a hash map.
	@param parent The parent resource bundle, or <code>null</code> if there should be no parent for resolving resources.
	*/
	public HashMapResourceBundle(final ResourceBundle parent)
	{
		map=new HashMap<String, Object>();	//create a default hash map
		setParent(parent);	//set the parent to that given
	}

	/**Map constructor with no parent.
	Keys will be converted to strings.
	@param map The map containing the resource mappings which will be copied to this map.
	@exception NullPointerException if the given map is <code>null</code>.
	*/
	public HashMapResourceBundle(final Map<?, ?> map)
	{
		this(map, null);	//construct the class with no parent
	}

	/**Map and parent constructor.
	All values will be copied to the map.
	Keys will be converted to strings.
	@param map The map containing the resource mappings which will be copied to this map.
	@param parent The parent resource bundle, or <code>null</code> if there should be no parent for resolving resources.
	@exception NullPointerException if the given map is <code>null</code>.
	*/
	public HashMapResourceBundle(final Map<?, ?> map, final ResourceBundle parent)
	{
		this(parent);	//do the default construction
		for(final Map.Entry<?, ?> entry:map.entrySet())	//for each entry in the map
		{
			final Object key=entry.getKey();	//get this entry's key
			if(key!=null)	//if this entry has a key
			{
				this.map.put(key.toString(), entry.getValue());	//save this key and value in the map
			}
		}
	}


	/**Gets an object for the given key from this resource bundle.
	@param key The key for the desired object.
	@exception NullPointerException if the given key is <code>null</code>.
	@return The object for the given key, or <code>null</code>.
	*/
	protected Object handleGetObject(final String key)
	{
		return map.get(checkInstance(key, "Resource key cannot be null."));	//look up the object from the map
	}

	/**@return An enumeration of the resouce keys.*/
	public Enumeration<String> getKeys()
	{
		final ResourceBundle parent=this.parent;	//get the parent resource bundle, if there is one
		final Iterator<String> parentKeyIterator;	//get an iterator to the parent's keys, if there is a parent
		if(parent!=null)	//if there is a parent
		{
			parentKeyIterator=new EnumerationIterator<String>(parent.getKeys());	//convert the parent key enumeration to an iterator
		}
		else	//if there is no parent
		{
			parentKeyIterator=null;	//there is no parent key iterator
		}
		final Iterator<String> keyIterator=map.keySet().iterator();	//get an iterator to the map's keys
		return new JoinIterator<String>(keyIterator, parentKeyIterator);	//join our keys and the parent's keys
	}
}

