package com.garretwilson.text.elff;

import java.io.*;
import java.net.*;
import java.text.SimpleDateFormat;
import java.util.*;

import com.garretwilson.util.NameValuePair;

import static com.garretwilson.lang.StringBuilderUtilities.*;
import static com.garretwilson.lang.ObjectUtilities.*;

/**Access to a log in the Extended Log File Format (ELFF).
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
	
		/**@return The fields used in this ELFF log.*/
//TODO del or provide a read-only iterator		public Field[] getFields() {return fields;}

	/**Fields constructor.
	@param fields The fields to use for each log entry.
	@exception NullPointerException if the array of fields is <code>null</code>.
	*/
	public ELFF(final Field<?>... fields)
	{
		this.fields=checkInstance(fields, "Fields cannot be null.");	//TODO make a copy
	}

	/**Writes the appropriate directives for this log.
	The {@value #VERSION_DIRECTIVE}, {@value #DATE_DIRECTIVE}, and {@value #FIELDS_DIRECTIVE} directives are always written,
		resulting in duplication if any of these directives are specified.
	The writer is flushed after writing.
	This method is thread-safe.
	@param writer The writer for writing to the log.
	@param directives The names and values of the directives to write.
	@exception IOException if there is an error writing to the log.
	*/
	public synchronized void logDirectives(final Writer writer, final NameValuePair<String, String>... directives) throws IOException
	{
		for(final NameValuePair<String, String> directive:directives)	//for each directive
		{
			logDirective(writer, directive.getName(), directive.getValue());	//log this directive
		}
		logDirective(writer, VERSION_DIRECTIVE, LATEST_VERSION);	//log the version
		logDirective(writer, DATE_DIRECTIVE, new SimpleDateFormat(DATE_TIME_FORMAT_PATTERN).format(new Date()));	//log the date+time
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
		logDirective(writer, FIELDS_DIRECTIVE, fieldsStringBuilder.toString());	//log the fields specifications
	}

	/**Writes a directive to the log.
	The writer is flushed after writing.
	This method is thread-safe.
	@param writer The writer for writing to the log.
	@param name The name of the directive.
	@param value The value of the directive.
	@exception IOException if there is an error writing to the log.
	*/
	public synchronized void logDirective(final Writer writer, final String name, final String value) throws IOException
	{
		final StringBuilder directiveStringBuilder=new StringBuilder();	//create a string builder for the directive
		formatDirective(directiveStringBuilder, name, value);	//format this directive
		directiveStringBuilder.append('\n');	//append an end-of-line character
		writer.write(directiveStringBuilder.toString());	//write the formatted entry
		writer.flush();	//flush the contents
	}

	/**Formats a directive for a log.
	@param stringBuilder The string builder for formatting the value.
	@param field The field with which the value is associated.
	@param value The value to write to the log, or <code>null</code> if this field has no value in the current entry.
	@return The string builder with the new formatted content.
	@exception ClassCastException if the given value is not compatible with the field's type
	*/
	public static <T> StringBuilder formatDirective(final StringBuilder stringBuilder, final String name, final String value)
	{
		return stringBuilder.append('#').append(name).append(':').append(' ').append(value);	//#name: value
	}

	/**Writes the given entry to the given writer.
	The writer is flushed after writing.
	This method is thread-safe.
	@param writer The writer for writing to the log.
	@param entry The entry to write to the log.
	@exception IOException if there is an error writing to the log.
	*/
	public synchronized void log(final Writer writer, final Entry entry) throws IOException
	{
		final StringBuilder entryStringBuilder=new StringBuilder();	//create a string builder for the entry
		for(final Field<?> field:fields)	//for each field in the log
		{
			formatFieldValue(entryStringBuilder, entry, field);	//format this field's value
			entryStringBuilder.append(' ');	//separate the field values
		}
		final int untrimmedLength=entryStringBuilder.length();	//find out the length before trimming
		if(untrimmedLength>0)	//if we wrote any fields
		{
			entryStringBuilder.deleteCharAt(untrimmedLength-1);	//remove the last space
		}
		entryStringBuilder.append('\n');	//append an end-of-line character
		writer.write(entryStringBuilder.toString());	//write the formatted entry
		writer.flush();	//flush the contents
	}

	/**Formats a field value of an entry.
	@param <T> The type of value for the field.
	@param stringBuilder The string builder for formatting the value.
	@param entry The entry that contains the field values.
	@param field The field the value of which to format.
	@return The string builder with the new formatted content.
	*/
	public <T> StringBuilder formatFieldValue(final StringBuilder stringBuilder, final Entry entry, final Field<T> field)
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
					stringBuilder.append(new SimpleDateFormat(DATE_FORMAT_PATTERN).format((Date)value));	//convert to a date and format to the writer
					break;
				case TIME:
					stringBuilder.append(new SimpleDateFormat(TIME_FORMAT_PATTERN).format((Date)value));	//convert to a date and format to the writer
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
		replace(stringBuilder, '"', "\"");	//replace each quote with two quotes
		return stringBuilder.toString();	//return the encoded string
	}
}
