package com.garretwilson.text.elff;

import java.io.*;
import java.net.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

import com.garretwilson.util.NameValuePair;
import static com.garretwilson.util.TimeZoneConstants.*;

import static com.garretwilson.lang.StringBuilderUtilities.*;
import static com.garretwilson.lang.Objects.*;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIs.*;
import static com.garretwilson.util.MapUtilities.*;

/**Access to a log in the Extended Log File Format (ELFF).
This class is thread-safe and can be used to concurrently generate log information.
@author Garret Wilson
@see <a href="file:///D:/reference/W3C/ELFF/WD-logfile.html">W3C Extended Log File Format</a>
*/
public class ELFF
{

	/**The latest ELFF version.*/
	public final static String LATEST_VERSION="1.0";

	/**The version of the extended log file format used.*/
	public final static String VERSION_DIRECTIVE="Version";
	/**Specifies the fields recorded in the log.*/
	public final static String FIELDS_DIRECTIVE="Fields";
	/**Identifies the software which generated the log.*/
	public final static String SOFTWARE_DIRECTIVE="Software";
	/**The date and time at which the log was started.*/
	public final static String START_DATE_DIRECTIVE="Start-Date";
	/**The date and time at which the log was finished.*/
	public final static String END_DATE_DIRECTIVE="End-Date";
	/**The date and time at which the entry was added.*/
	public final static String DATE_DIRECTIVE="Date";
	/**Comment information.*/
	public final static String REMARK_DIRECTIVE="Remark";
	
	/**The pattern for formatting an ELFF date value.*/
	public final static String DATE_FORMAT_PATTERN="yyyy-MM-dd";

	/**The pattern for formatting an ELFF time value.*/
	public final static String TIME_FORMAT_PATTERN="HH:mm:ss:SSS";

	/**The pattern for formatting an ELFF date+time value.*/
	public final static String DATE_TIME_FORMAT_PATTERN=DATE_FORMAT_PATTERN+' '+TIME_FORMAT_PATTERN;

	/**The value to represent null in a field.*/
	public final static String NULL_FIELD_VALUE="-";
	
	/**The fields used in this ELFF log.*/
	private final Field[] fields;
	
	/**The thread-safe map of directives to be added when directives are written.*/
	private final Map<String, String> directiveMap=new ConcurrentHashMap<String, String>();

		/**Retreives a set directive.
		@param name The name of a directive to retrieve.
		@return The value of the directive, or <code>null</code> if the directive is not set.
		*/
		public String getDirective(final String name) {return directiveMap.get(name);}

		/**Sets a directive.
		@param name The name of the directive to set.
		@param value The value of the directive, or <code>null</code> if the value should be removed.
		@return The old value of the directive, or <code>null</code> if the directive had no value.
		*/
		public String setDirective(final String name, final String value)
		{
			return putRemoveNull(directiveMap, name, value);	//set the value or remove the key, depending on the value
		}
		
	/**Fields constructor.
	@param fields The fields to use for each log entry.
	@exception NullPointerException if the writer and/or the array of fields is <code>null</code>.
	*/
	public ELFF(final Field<?>... fields)
	{
		this.fields=checkInstance(fields, "Fields cannot be null.").clone();	//store a copy of the fields so that they can't be modified later by the caller
	}

	/**Creates a string representation of the appropriate directives for this log.
	Any values set via {@link #setDirective(String, String)} are included, resulting in duplicating if any of those directives are specified here.
	The {@value #VERSION_DIRECTIVE}, {@value #DATE_DIRECTIVE}, and {@value #FIELDS_DIRECTIVE} directives are always written,
		resulting in duplication if any of these directives are specified.
	@param directives The names and values of the directives to write.
	@return A string representing the given directives.
	*/
	public String serializeDirectives(final NameValuePair<String, String>... directives) throws IOException
	{
		final StringBuilder directiveStringBuilder=new StringBuilder();	//create a string builder for assembling the directives
			//log predefined directives
		for(final Map.Entry<String, String> directiveEntry:directiveMap.entrySet())	//for each directive entry in the map
		{
			formatDirective(directiveStringBuilder, directiveEntry.getKey(), directiveEntry.getValue());	//format this directive
		}
			//log specified directives
		for(final NameValuePair<String, String> directive:directives)	//for each directive
		{
			formatDirective(directiveStringBuilder, directive.getName(), directive.getValue());	//format this directive
		}
		formatDirective(directiveStringBuilder, VERSION_DIRECTIVE, LATEST_VERSION);	//format the version
		formatDirective(directiveStringBuilder, DATE_DIRECTIVE, createDateTimeFormat().format(new Date()));	//format the date+time of the log
		final StringBuilder fieldsStringBuilder=new StringBuilder();	//create a new string builder for formatting the fields specification
		if(fields.length>0)	//if there are fields
		{
			for(final Field<?> field:fields)	//for each field in the log
			{
				final String identifier=field.getIdentifier();	//get the field's identifier
				final FieldIdentifierPrefix prefix=field.getPrefix();	//get the field's prefix
				if(prefix!=null)	//if there is a prefix
				{
					fieldsStringBuilder.append(prefix.getID());	//append the prefix ID
					if(field.isHeader())	//if this is a header field
					{
						fieldsStringBuilder.append('(').append(identifier).append(')');	//prefix(identifier)
					}
					else	//if this is not a header field
					{
						fieldsStringBuilder.append('-').append(identifier);	//prefix-identifier						
					}
				}
				else	//if there is no prefix
				{
					fieldsStringBuilder.append(identifier);	//identifier
				}
				fieldsStringBuilder.append(' ');	//separate the field identifiers
			}
			fieldsStringBuilder.deleteCharAt(fieldsStringBuilder.length()-1);	//remove the last space
		}		
		formatDirective(directiveStringBuilder, FIELDS_DIRECTIVE, fieldsStringBuilder.toString());	//format the fields specifications
		return directiveStringBuilder.toString();	//return the string we constructed
	}

	/**Creates a string representation of a directive.
	The serialization of the directive includes the ending newline character.
	@param name The name of the directive.
	@param value The value of the directive.
	@return A string representing the given directive.
	*/
	public String serializeDirective(final String name, final String value) throws IOException
	{
		return formatDirective(new StringBuilder(), name, value).toString();	//format the directive and return the string value
	}

	/**Formats a directive for a log.
	The directive serialization includes the ending newline character.
	@param stringBuilder The string builder for formatting the value.
	@param field The field with which the value is associated.
	@param value The value to write to the log, or <code>null</code> if this field has no value in the current entry.
	@return The string builder with the new formatted content.
	@exception ClassCastException if the given value is not compatible with the field's type
	*/
	public StringBuilder formatDirective(final StringBuilder stringBuilder, final String name, final String value)
	{
		return stringBuilder.append('#').append(name).append(':').append(' ').append(value).append('\n');	//#name: value\n
	}

	/**Creates a string representation of the given entry.
	The serialized entry string includes the ending newline character.
	@param entry The entry to serialize.
	@return A string representing the given entry.
	*/
	public String serializeEntry(final Entry entry) throws IOException
	{
		return formatEntry(new StringBuilder(), entry).toString();	//format the entry and return the string value
	}

	/**Formats an entry for a log.
	The entry serialization includes the ending newline character.
	@param stringBuilder The string builder for formatting the value.
	@param entry The entry to serialize.
	@return The string builder with the new formatted content.
	@exception ClassCastException if the given value is not compatible with the field's type
	*/
	public StringBuilder formatEntry(final StringBuilder stringBuilder, final Entry entry)
	{
		for(final Field<?> field:fields)	//for each field in the log
		{
			formatFieldValue(stringBuilder, entry, field);	//format this field's value
			stringBuilder.append(' ');	//separate the field values
		}
		final int untrimmedLength=stringBuilder.length();	//find out the length before trimming
		if(untrimmedLength>0)	//if we wrote any fields
		{
			stringBuilder.deleteCharAt(untrimmedLength-1);	//remove the last space
		}
		stringBuilder.append('\n');	//append an end-of-line character
		return stringBuilder;	//return the string builder used
	}

	/**Formats a field value of an entry.
	@param <T> The type of value for the field.
	@param stringBuilder The string builder for formatting the value.
	@param entry The entry that contains the field values.
	@param field The field the value of which to format.
	@return The string builder with the new formatted content.
	*/
	public static <T> StringBuilder formatFieldValue(final StringBuilder stringBuilder, final Entry entry, final Field<T> field)
	{
		return formatFieldValue(stringBuilder, field, entry.getFieldValue(field));	//retrieve the field value from the entry and write it to the field
	}

	/**Formats a field value.
	<p>The following field types accept the following Java types:</p>
	<dl>
		<dt>{@link FieldType#ADDRESS}</dt> <dd>{@link String}</dd>
		<dt>{@link FieldType#DATE}</dt> <dd>{@link Date}</dd>
		<dt>{@link FieldType#FIXED}</dt> <dd>{@link Number}</dd>
		<dt>{@link FieldType#INTEGER}</dt> <dd>{@link Number}</dd>
		<dt>{@link FieldType#STRING}</dt> <dd>{@link String}</dd>
		<dt>{@link FieldType#TIME}</dt> <dd>{@link Date}</dd>
		<dt>{@link FieldType#URI}</dt> <dd>{@link URI}</dd>
	</dl>
	@param <T> The type of value to write.
	@param stringBuilder The string builder for formatting the value.
	@param field The field with which the value is associated.
	@param value The value to write to the log, or <code>null</code> if this field has no value in the current entry.
	@return The string builder with the new formatted content.
	@exception ClassCastException if the given value is not compatible with the field's type
	*/
	public static <T> StringBuilder formatFieldValue(final StringBuilder stringBuilder, final Field<T> field, final T value)
	{
		if(value!=null)	//if there is a value
		{
			final FieldType fieldType=field.getType();	//get the field type
			switch(fieldType)	//see which field type we're using
			{
				case FIXED:
					stringBuilder.append(Double.toString(((Number)value).doubleValue()));	//write the double value
					break;
				case INTEGER:
					stringBuilder.append(Integer.toString(((Number)value).intValue()));	//write the integer value
					break;
				case URI:
					stringBuilder.append(((URI)value).toString());	//write the URI value
					break;
				case DATE:
					{
						final DateFormat dateFormat=new SimpleDateFormat(DATE_FORMAT_PATTERN);	//create a date format object
						dateFormat.setTimeZone(TimeZone.getTimeZone(GMT_ID));	//switch to the GMT time zone
						stringBuilder.append(dateFormat.format((Date)value));	//convert to a date and format to the writer
					}
					break;
				case TIME:
					{
						final DateFormat timeFormat=new SimpleDateFormat(TIME_FORMAT_PATTERN);	//create a time format object
						timeFormat.setTimeZone(TimeZone.getTimeZone(GMT_ID));	//switch to the GMT time zone
						stringBuilder.append(timeFormat.format((Date)value));	//convert to a date and format to the writer
					}
					break;
				case STRING:
					stringBuilder.append(encodeString((String)value));
					break;
				case ADDRESS:
					stringBuilder.append((String)value);
					break;
				default:
					throw new AssertionError("Unrecognized field type: "+fieldType);
			}
		}
		else	//if the value is null
		{
			stringBuilder.append(NULL_FIELD_VALUE);	//write the string for a null value
		}
		return stringBuilder;	//return the string builder with the formatted content
	}

	/**Encodes a string for storing as a field value.
	Every instance of a quote character ('"') is replaced with two quotes.
	@param string The string to encode.
	@return The encoded string.
	 */
	public final static String encodeString(final String string)
	{

		final StringBuilder stringBuilder=new StringBuilder(string);	//create a new string builder
//TODO reconcile with ELFF specification; WebTrends URL-encodes strings		replace(stringBuilder, '"', "\"");	//replace each quote with two quotes
		replace(stringBuilder, '+', "++");	//replace each plus with two plusses (it is not clear whether WebTrends does this or not, but they have to do something to compensate for literal plus characters)
		replace(stringBuilder, ' ', "+");	//replace each quote with two quotes		
		return stringBuilder.toString();	//return the encoded string
	}

	/**Constructs a query name/values pair in the form <code><var>name</var>=<var>value1</var>;<var>value2</var>...</code>.
	Multiple values will be separated by the ';' character.
	@param stringBuilder The string builder to which the query parameter should be appended.
	@param name The name of the query parameter.
	@param values The values to associate with the query parameter name.
	@return The string builder being used to build the query parameter.  
	 */
	public static StringBuilder appendURIQueryParameter(final StringBuilder stringBuilder, final String name, final String... values)
	{
		stringBuilder.append(encode(name));	//append the parameter name
		stringBuilder.append(QUERY_NAME_VALUE_ASSIGNMENT);	//append the value-assignment character
		if(values.length>0)	//if there are values
		{
			for(final String value:values)	//for each value
			{
				stringBuilder.append(encode(value));	//append the parameter value
				stringBuilder.append(';');	//append the value delimeter TODO use a constant
			}
			stringBuilder.delete(stringBuilder.length()-1, stringBuilder.length());	//remove the last value delimiter
		}
		return stringBuilder;	//return the string builder, which now also contains the query parameter we constructed		
	}

	/**Creates a date format to format a date and a time in GMT according to the ELFF specification.
	@return A new date format for formatting a date and time
	*/
	public static DateFormat createDateTimeFormat()
	{
		final DateFormat dateTimeFormat=new SimpleDateFormat(DATE_TIME_FORMAT_PATTERN);	//create a date+time format object
		dateTimeFormat.setTimeZone(TimeZone.getTimeZone(GMT_ID));	//switch to the GMT time zone
		return dateTimeFormat;	//return the date+time format object
	}
}
