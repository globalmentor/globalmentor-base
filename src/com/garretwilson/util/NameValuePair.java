package com.garretwilson.util;

import com.garretwilson.lang.ObjectUtilities;

/**A convenience container for a name and a value.
	Name/value pairs are sorted first by name, then by value, assuming both
	implement the <code>Comparable</code> interface.
@author Garret Wilson
*/
public class NameValuePair implements Comparable  //G***isn't there a better name for this? what does "tuple" mean, anyway?
{

	/**The name of the object.*/
	final private Object name;

		/**@return The name of the object.*/
		public Object getName() {return name;}

	/**The value of the object.*/
	final private Object value;

		/**@return The value of the object.*/
		public Object getValue() {return value;}

	/**Default constructor.*/
/*G***del
	public NameValuePair()
	{
	}
*/

	/**Constructor specifying the name and value.
	@param newName The object's new name.
	@param newValue The object's new value
	*/
	public NameValuePair(final Object newName, final Object newValue)
	{
		name=newName; //set the name
		value=newValue; //set the value
	}

	/**If <code>object</code> is another <code>NameValuePair</code>, compares the
		names and values. Otherwise, compares the objects using the superclass functionality.
	@param object The object with which to compare this name/value pair; should be
		another name/value pair.
	@return <code>true<code> if this name/value pair equals that specified in
		<code>object</code>.
	@see #getName
	@see #getValue
	*/
	public boolean equals(Object object)
	{
		if(object instanceof NameValuePair)	//if we're being compared with another name/value pair
		{
		    //compare the names and values, taking into account that one or the other may be null
			return ObjectUtilities.equals(getName(), ((NameValuePair)object).getName())
					&& ObjectUtilities.equals(getValue(), ((NameValuePair)object).getValue());
		}
		else	//if we're being compared with anything else
			return super.equals(object);	//use the default compare
	}

	/**Compares this object to another object.
		This method determines order based upon first the name, and then the value.
		Both objects must implement <code>Comparable</code> or an exception will be
		thrown.
	@param object The object with which to compare the component. This must be
		another <code>NameValuePair</code> object.
	@return A negative integer, zero, or a positive integer as this name is
		less than, equal to, or greater than the name and value of the specified
		object, respectively.
	@exception ClassCastException Thrown if the specified object's type is not
		a <code>NameValuePair</code>, or if the name and/or the value does not
		implement <code>Comparable</code>.
	@see #getName
	@see #getValue
	*/
	public int compareTo(Object object) throws ClassCastException
	{
		final int result=((Comparable)getName()).compareTo(((NameValuePair)object).getName()); //compare names
			//if the names are equal, compare the values
		return result!=0 ? result : ((Comparable)getValue()).compareTo(((NameValuePair)object).getValue()); //compare values
	}

	/**@return A hashcode value composed from the name.*/
	public int hashCode()
	{
		return getName().hashCode();  //return the hash code of the name
	}

	/**@return A string representation of this object in the format "name=\"value\"".*/
	public String toString()
	{
		return super.toString()+": "+getName()+"="+(getValue()!=null ? "\""+getValue()+"\"" : getValue());  //return a string constructed from the name and the value
	}

}
