package com.garretwilson.csv;

import java.io.*;
import com.garretwilson.lang.StringBufferUtilities;

import static com.garretwilson.text.CharacterConstants.*;

/**Serializes Comma Separated Value (CSV) information
@author Garret Wilson
*/
public class CSVSerializer implements CSVConstants
{

	/**Serializes the given objects to the provided output stream using the
	UTF-8 encoding scheme.
	@param outputStream The output stream to which the objects will be serialized.
	@param objects The objects to serialize, each of which may be
		<code>null</code>.
	@exception IOException if there is an error writing the CSV record.
	*/
/*G****del; this won't work with multiple calls.
	public static void serialize(final OutputStream outputStream, final Object[] objects) throws IOException
	{
		serialize(outputStream, objects, CharacterEncodingConstants.UTF_8);	//serialize using UTF-8
	}
*/

	/**Serializes the given objects to the provided output stream.
	@param outputStream The output stream to which the objects will be serialized.
	@param objects The objects to serialize, each of which may be
		<code>null</code>.
	@param encoding The character encoding.
	@exception IOException if there is an error writing the CSV record.
	*/
/*G****del; this won't work with multiple calls.
	public static void serialize(final OutputStream outputStream, final Object[] objects, final String encoding) throws IOException
	{
		serialize(new OutputStreamWriter(outputStream, encoding), objects);	//serialize using the given encoding
	}
*/

	/**Serializes the given objects to the provided writer.
	@param writer The writer to which the objects will be serialized.
	@param objects The objects to serialize, each of which may be
		<code>null</code>.
	@exception IOException if there is an error writing the CSV record.
	*/
	public static void serialize(final Writer writer, final Object[] objects) throws IOException
	{
		for(int i=0; i<objects.length; ++i)	//look at each object
		{
			if(i>0)	//if this isn't the first object
			{
				writer.write(FIELD_DELIMITER_CHAR);	//delimit the fields
			}
			final Object object=objects[i];	//get a reference to this object
			if(object!=null)	//if there is an object to write
			{
				final String field=encodeField(object.toString());	//get the string value of the object
				writer.write(field);	//write the field
			}
		}
		writer.write(RECORD_DELIMITER_CHAR);	//write the end of the record
	}

	/**Encodes a string to be represented in a CSV field.
	Encoding includes:
	<ul>
		<li>Escaping internal quotation marks by doubling them.</li>
		<li>Adding beginning and ending quotation marks.</li>
	</ul>
	@param string The string to encode. 
	@return An encoded string appropriate for serialization as a CSV field.
	 */
	public static String encodeField(final String string)
	{
		final StringBuffer stringBuffer=new StringBuffer(string);	//create a new string buffer to process the string 
		StringBufferUtilities.replace(stringBuffer, QUOTATION_MARK_CHAR, ESCAPED_QUOTATION_MARK_STRING);	//replace all quotes with doubled quotes
		stringBuffer.insert(0, QUOTATION_MARK_CHAR);	//prepend a quotation mark
		stringBuffer.append(QUOTATION_MARK_CHAR);	//append a quotation mark
		return stringBuffer.toString();	//return the string we created
	}
}
