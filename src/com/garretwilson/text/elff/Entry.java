package com.garretwilson.text.elff;

import java.util.*;

/**An entry of the Extended Log File Format (ELFF).
@author Garret Wilson
@see <a href="file:///D:/reference/W3C/ELFF/WD-logfile.html">W3C Extended Log File Format</a>
*/
public class Entry
{

	/**The map of values keyed to fields.*/
	private final Map<Field, Object> fieldValueMap=new HashMap<Field, Object>();

	/**Retrieves a value from a field.
	@param <T> The type of value stored in the field.
	@param field The field from which the value should be retrieved.
	@return The value stored in the field, or <code>null</code> if no value is stored in the field.
	*/
	@SuppressWarnings("unchecked")	//we only allow correct types to be stored in the map, so we expect the returned type to be correct
	public <T> T getFieldValue(final Field<T> field)
	{
		return (T)fieldValueMap.get(field);	//retrieve the value from the field		
	}

	/**Stores a value in a field.
	@param <T> The type of value to store in the field; must be appropriate for the given field.
	@param field The field to which a value should be assigned.
	@param value The value to assign to the field, or <code>null</code> if no value should be stored in the field.
	@return The previous value stored in the field, or <code>null</code> if no value was stored in the field.
	*/
	@SuppressWarnings("unchecked")	//we only allow correct types to be stored in the map, so we expect the returned type to be correct
	public <T> T setFieldValue(final Field<T> field, final T value)
	{
		if(value!=null)	//if a value was given
		{
			return (T)fieldValueMap.put(field, value);	//store the value in the field
		}
		else	//if no value was given
		{
			return (T)fieldValueMap.remove(field);	//remove the value from the field
		}
	}
}
