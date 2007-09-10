package com.garretwilson.urf;

import java.io.IOException;
import java.io.Reader;
import java.net.*;
import java.util.*;
import static java.util.Collections.*;

import static com.garretwilson.io.ReaderParser.*;
import com.garretwilson.io.ParseIOException;
import com.garretwilson.net.*;
import static com.garretwilson.net.URIUtilities.*;
import static com.garretwilson.text.CharacterConstants.*;

import com.garretwilson.util.Debug;

import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;

/**Class that is able to construct an RDF data model from an XML-based
	RDF serialization. Each instance of an RDF processor maintains an internal
	RDF data model throughout its lifetime that is continually updated with
	every new RDF processing that occurs.
	<p>The RDF processor maintains RDF data in two separate formats: the RDF
	data model <code>RDF</code>, as well as a list of statements used to create
	the data model. The RDF data model may be replaced and its members modified,
	but these actions will not update the list of RDF statements. The RDF
	statements are only generated by the RDF processor itself as it parses
	RDF serializations, and are available to give information on the parser
	actions.</p>
	TODO If a property from a non-RDF namespace has (for example) a "resource" attribute (i.e. a property with no namespace), property resources are not correctly created and can cause endless loops when trying to analyze the namespace
@author Garret Wilson
*/
public class TURFProcessor extends AbstractURFProcessor
{

	/**Default constructor.*/
	public TURFProcessor()
	{
		this(new URF());  //create an URF data model to use
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param urf The RDF data model to use.
	*/
	public TURFProcessor(final URF urf)
	{
		super(urf);  //construct the parent class
	}

	/**Parses a resources.
	The current position must that of the first character of the first resource in the list.
	The new position will be that of the first non-separator character after the resource or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if a resource in the list is missing, or if the reader has no more characters before a resource in the list is completely parsed.
	*/
	public void process(final Reader reader, final URI baseURI) throws IOException, ParseIOException
	{
		skipSeparators(reader);	//skip separators
		parseResourceList(reader, baseURI, NULL_CHAR);	//parse as list of resources
	}

	/**Skips over TURF separator characters in a reader.
	The new position will either be the that of the first non-separator character or the end of the input stream.
	@param reader The reader the contents of which to be parsed.
	@return The next character that will be returned the reader's {@link Reader#read()} operation, or <code>-1</code> if the end of the reader has been reached.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	*/
	protected static int skipSeparators(final Reader reader) throws IOException
	{
		return skip(reader, SEPARATORS);	//skip all separators
	}

	/**Parses a single resource and returns a proxy to the resource.
	The current position must be that of a separator or that of the first character of the first resource in the list.
	The new position will be that of the first non-separator character after the resources or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@return The resource parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters before the current resource is completely parsed.
	*/
	public Resource parseResource(final Reader reader, final URI baseURI) throws IOException, ParseIOException
	{
Debug.trace("ready to parse resource");
		final URF urf=getURF();	//get the URF data model
		String label=null;	//the label of the resource, if any
		URI resourceURI=null;	//the URI of the resource, if any
		final List<Resource> types=new ArrayList<Resource>();	//the types, if any
		Resource[] arrayElements=null;	//if we find an array, we'll store the elements here so that we can add them to the resource later
		boolean foundComponent=false;	//we'll keep track of whether at least one description component was present
		int c=peek(reader);	//peek the next character
		if(c==LABEL_BEGIN)	//check for a label
		{
Debug.trace("ready to parse label");
			foundComponent=true;	//indicate that at least one description component is present
			label=parseLabel(reader);	//parse the label
Debug.trace("label:", label);
			c=skipSeparators(reader);	//skip separators and peek the next character
		}
		switch(c)	//check for a reference or a short form
		{
			case REFERENCE_BEGIN:
Debug.trace("found reference beginning");
				foundComponent=true;	//indicate that at least one description component is present
				resourceURI=parseURI(reader, baseURI, REFERENCE_BEGIN, REFERENCE_END);	//parse the resource URI
				c=skipSeparators(reader);	//skip separators and peek the next character
				break;
			case ARRAY_BEGIN:
				foundComponent=true;	//indicate that at least one description component is present
				types.add(getResourceProxy(ARRAY_CLASS_URI));	//add a proxy to the array type
				check(reader, ARRAY_BEGIN);	//read the beginning array delimiter
				arrayElements=parseResourceList(reader, baseURI, ARRAY_END);	//parse the resources serving as array elements; we'll actually add them to the resource after creating the resource proxy
				check(reader, ARRAY_END);	//read the ending array delimiter
				c=skipSeparators(reader);	//skip separators and peek the next character
				break;
			case BOOLEAN_BEGIN:	//boolean
				foundComponent=true;	//indicate that at least one description component is present
				final boolean b=parseBoolean(reader);	//parse the boolean
				resourceURI=createLexicalURI(NUMBER_CLASS_URI, Boolean.toString(b));	//create a URI for the resource
				c=skipSeparators(reader);	//skip separators and peek the next character
				break;
			case NUMBER_BEGIN:	//number
				foundComponent=true;	//indicate that at least one description component is present
				final Number number=parseNumber(reader);	//parse the number
				resourceURI=createLexicalURI(NUMBER_CLASS_URI, number.toString());	//create a URI for the resource
				c=skipSeparators(reader);	//skip separators and peek the next character
				break;
			case STRING_BEGIN:	//string
				foundComponent=true;	//indicate that at least one description component is present
				final String string=parseString(reader, STRING_BEGIN, STRING_END);	//parse the string
				resourceURI=createLexicalURI(STRING_CLASS_URI, string);	//create a URI for the string
				c=skipSeparators(reader);	//skip separators and peek the next character
				break;
			case URI_BEGIN:	//URI
				foundComponent=true;	//indicate that at least one description component is present
				final URI uri=parseURI(reader, baseURI, URI_BEGIN, URI_END);	//parse the URI
				resourceURI=createLexicalURI(URI_CLASS_URI, uri.toString());	//create a URI for the resource
				c=skipSeparators(reader);	//skip separators and peek the next character
				break;
		}
		if(resourceURI!=null && isLexicalNamespaceURI(resourceURI))	//if there is a resource URI that is in a lexical namespace
		{
			types.add(getResourceProxy(getLexicalNamespaceTypeURI(resourceURI)));	//add a proxy to the lexical namespace type
		}		
		if(c==TYPE_BEGIN)	//check for a type
		{
			foundComponent=true;	//indicate that at least one description component is present
			check(reader, TYPE_BEGIN);	//read the beginning type delimiter
			final Resource[] typeResources=parseResourceList(reader, baseURI, TYPE_END);	//parse the resources serving as types
			addAll(types, typeResources);	//add all the types we found
			check(reader, TYPE_END);	//read the ending type delimiter
			c=skipSeparators(reader);	//skip separators and peek the next character
		}
		if(!foundComponent && c!=PROPERTIES_BEGIN)	//if there were no description components so far, and we don't see any properties coming up
		{
			checkReaderEnd(c);	//make sure we're not at the end of the reader
			throw new ParseIOException("Expected resource; found character: "+(char)c);	//TODO improve with source throwable
		}
Debug.trace("ready to get resource proxy for label", label, "resource URI", resourceURI);
		final ResourceProxy resourceProxy=getResourceProxy(label, resourceURI);	//get a resource proxy from the label and/or reference URI, or use one already available for the reference URI
Debug.trace("type count", types.size());
		if(!types.isEmpty())	//if we have at least one type
		{
			final Resource typePropertyResource=getResourceProxy(TYPE_PROPERTY_URI);	//get a proxy to the type property resource
			for(final Resource type:types)	//for each type
			{
				addAssertion(new Assertion(resourceProxy, typePropertyResource, type));	//assert this type
			}
		}
		if(arrayElements!=null)	//if this is an array, asset the elements of the array now that we've created the array resource proxy
		{
			long index=0;	//start with the first index
				//TODO make sure there are not too many array elements
			for(final Resource arrayElement:arrayElements)	//for each array element
			{
				addAssertion(new Assertion(resourceProxy, getResourceProxy(createIndexURI(index)), arrayElement));	//assert add the element at this index of the resource
				++index;	//go to the next index
			}
		}
		if(c==PROPERTIES_BEGIN)	//check for properties
		{
			parseProperties(reader, baseURI, resourceProxy);	//parse the resource properties
		}
Debug.trace("ready to return resource proxy with URI", resourceProxy.getURI());
		return resourceProxy;	//return the resource proxy we created
	}

	/**Checks that the description component can be changed to the new description component while parsing a resource description.
	This method ensures that description components come in the correct order and are not repeated.
	@param oldDescriptionComponent The last description component, or <code>null</code> if no description component has been parsed.
	@param newDescriptionComponent The new description component ready to be parsed.
	@return The new description component, if it is a valid component to parse in this context.
	@exception NullPointerException if the new description component is <code>null</code>.
	@exception ParseIOException if the given description component is being repeated or appears out of order.
	*/
	protected static DescriptionComponent checkDescriptionComponent(final DescriptionComponent oldDescriptionComponent, final DescriptionComponent newDescriptionComponent) throws ParseIOException
	{
		if(oldDescriptionComponent!=null && oldDescriptionComponent.ordinal()>=newDescriptionComponent.ordinal())	//if we've already reached or passed the old description component
		{
			throw new ParseIOException("Resource description component "+newDescriptionComponent+" cannot be repeated or appear out of order.");
		}
		return newDescriptionComponent;	//the new description component passed the tests; return it
	}


	/**Parses a list of resources.
	The current position must be that of a separator or that of the first character of the first resource in the list.
	The new position will be that of the first non-separator character after the list of resources or the end of the reader.
	@param reader The reader the contents of which to be parsed.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param end The character that marks the end of the list.
	@return The resource parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if a resource in the list is missing, or if the reader has no more characters before a resource in the list is completely parsed.
	*/
	public Resource[] parseResourceList(final Reader reader, final URI baseURI, final char end) throws IOException, ParseIOException	//TODO del end if not needed
	{
//Debug.trace("ready to parse resource list for end", end);
		final List<Resource> resourceList=new ArrayList<Resource>();	//create a new list in which to place the resources
		int c=skipSeparators(reader);	//skip separators and peek the next character
Debug.trace("peeked", (char)c);
//TODO del if not needed		if(indexOf(RESOURCE_BEGINS, c)<0 && c!=LIST_DELIMITER)	//if this is not the beginning of a resource, return (but don't return for the list delimiter, which is an error
//TODO del		while(c>=0 &&)	//while we are not out of data
//TODO del if not needed		while(indexOf(RESOURCE_BEGINS, c)>=0)	//while there is another resource to parse
//TODO del		while(indexOf(RESOURCE_BEGINS, c)>=0)	//while there is another resource to parse
		while(c>=0 && c!=end)	//while the end of the data has not been reached and there is another resource to parse
		{
			final Resource resource=parseResource(reader, baseURI);	//parse another resource
Debug.trace("parsed resource from list", resource);
			resourceList.add(resource);	//parse another resource and add it to the list
			c=skipSeparators(reader);	//skip separators and peek the next character
Debug.trace("after resource, peeked", (char)c);
			if(c==LIST_DELIMITER)	//if this is a list delimiter
			{
				check(reader, LIST_DELIMITER);	//skip the list delimiter
				c=skipSeparators(reader);	//skip separators and peek the next character
			}
			else	//if there's anything besides a list delimiter, we've reached the end of the list
			{
				break;	//stop parsing the list
			}
		}
//Debug.trace("ready to return list of resources for end", end, "next character", (char)peek(reader));
		return resourceList.toArray(new Resource[resourceList.size()]);	//return the parsed resources
	}

	/**Parses a label surrounded by label delimiters.
	The current position must be that of the first label delimiter character.
	The new position will be that immediately after the last label delimiter character.
	@param reader The reader the contents of which to be parsed.
	@return The label parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters before the current label is completely parsed.
	*/
	public static String parseLabel(final Reader reader) throws IOException, ParseIOException
	{
		check(reader, LABEL_BEGIN);	//read the beginning label delimiter
		final String label=reachAfter(reader, LABEL_END);	//read the label
		//TODO make sure this is a valid label
		return label;	//return the label we read
	}

	/**Parses a boolean surrounded by boolean delimiters.
	The current position must be that of the first boolean delimiter character.
	The new position will be that immediately after the last boolean delimiter character.
	@param reader The reader the contents of which to be parsed.
	@return The boolean parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the boolean is not syntactically correct or if the reader has no more characters before the current boolean is completely parsed.
	*/
	public static boolean parseBoolean(final Reader reader) throws IOException, ParseIOException
	{
		check(reader, BOOLEAN_BEGIN);	//read the beginning boolean delimiter
		final boolean b;	//we'll store the boolean here
		int c=peek(reader);	//peek the next character
		switch(c)	//see what the next character is
		{
			case BOOLEAN_FALSE_BEGIN:	//false
				check(reader, BOOLEAN_FALSE_LEXICAL_FORM);	//make sure this is really false
				b=false;	//store the boolean value
				break;
			case BOOLEAN_TRUE_BEGIN:	//true
				check(reader, BOOLEAN_TRUE_LEXICAL_FORM);	//make sure this is really true
				b=false;	//store the boolean value
				break;
			default:	//if we don't recognize the start of the boolean lexical form
				checkReaderEnd(c);	//make sure we're not at the end of the reader
				throw new ParseIOException("Unrecognized start of boolean: "+(char)c);
		}
		check(reader, BOOLEAN_END);	//read the ending boolean delimiter
		return b;	//return the boolean we read
	}

	/**Parses a number surrounded by number delimiters.
	The current position must be that of the first number delimiter character.
	The new position will be that immediately after the last number delimiter character.
	@param reader The reader the contents of which to be parsed.
	@return The number parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters before the current number is completely parsed.
	*/
	public static Number parseNumber(final Reader reader) throws IOException, ParseIOException
	{
		check(reader, NUMBER_BEGIN);	//read the beginning number delimiter
		final StringBuilder stringBuilder=new StringBuilder();	//create a new string builder to use when reading the number
		int c=peek(reader);	//peek the first character
		if(c=='-')	//if the number starts with a minus sign
		{
			stringBuilder.append(check(reader, '-'));	//append the character
		}
		stringBuilder.append(check(reader, '0', '9'));	//there should be at least one digit
		stringBuilder.append(read(reader, '0', '9')); //read all remaining digits
		c=peek(reader);	//peek the next character
		if(c>=0)	//if we're not at the end of the reader
		{
			boolean hasFraction=false;	//we don't have a fraction yet
			boolean hasExponent=false;	//we don't have an exponent yet
			if(c=='.')	//if this is a floating point number
			{
				hasFraction=true;	//we found a fraction
				stringBuilder.append(check(reader, '.'));	//read and append the beginning decimal point
				stringBuilder.append(check(reader, '0', '9'));	//there should be at least one digit
				stringBuilder.append(read(reader, '0', '9')); //read all remaining digits
				c=peek(reader);	//peek the next character
			}
			if(c=='e')	//if this is an exponent
			{
				hasExponent=true;	//we found an exponent
				stringBuilder.append(check(reader, 'e'));	//read and append the exponent character
				c=peek(reader);	//peek the next character
				if(c=='-' || c=='+')	//if the exponent starts with a sign
				{
					stringBuilder.append(readCharacter(reader));	//append the sign					
				}
				stringBuilder.append(check(reader, '0', '9'));	//there should be at least one digit
				stringBuilder.append(read(reader, '0', '9')); //read all remaining digits
			}
			if(hasFraction || hasExponent)	//if there was a fraction or exponent
			{
				return Double.valueOf(Double.parseDouble(stringBuilder.toString()));	//parse a double and return it
			}
		}
		check(reader, NUMBER_END);	//read the ending number delimiter
			//TODO check for a number format error
		return Integer.valueOf(Integer.parseInt(stringBuilder.toString()));	//parse an integer and return it 
	}

	/**Parses a string surrounded by string delimiters.
	The current position must be that of the first string delimiter character.
	The new position will be that immediately after the string number delimiter character.
	@param reader The reader the contents of which to be parsed.
	@param stringBegin The beginning string delimiter.
	@param stringEnd The ending string delimiter.
	@return The string parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the string is not escaped correctly or reader has no more characters before the current string is completely parsed.
	*/
	public static String parseString(final Reader reader, final char stringBegin, final char stringEnd) throws IOException, ParseIOException
	{
		check(reader, stringBegin);	//read the beginning string delimiter
		final StringBuilder stringBuilder=new StringBuilder();	//create a new string builder to use when reading the string
		char c=readCharacter(reader);	//read a character
		while(c!=stringEnd)	//keep reading character until we reach the end of the string
		{
			if(c==STRING_ESCAPE)	//if this is an escape character
			{
				c=readCharacter(reader);	//read another a character
				switch(c)	//see what the next character
				{
					case ESCAPED_BACKSPACE:	//b backspace
						c=BACKSPACE_CHAR;	//use the character that was escaped
						break;
					case ESCAPED_FORM_FEED:	//f form feed
						c=FORM_FEED_CHAR;	//use the character that was escaped
						break;
					case ESCAPED_LINE_FEED:	//n line feed
						c=LINE_FEED_CHAR;	//use the character that was escaped
						break;
					case ESCAPED_CARRIAGE_RETURN:	//r carriage return
						c=CARRIAGE_RETURN_CHAR;	//use the character that was escaped
						break;
					case ESCAPED_TAB:	//t tab	
						c=HORIZONTAL_TABULATION_CHAR;	//use the character that was escaped
						break;
					case ESCAPED_UNICODE:	//u Unicode
						final String unicodeString=readString(reader, 4);	//read the four Unicode code point hex characters
							//TODO make sure the Unicode sequence is lowercase
						c=(char)Integer.parseInt(unicodeString, 16);	//parse the hex characters and use the resulting code point
						break;
					default:	//if another character was escaped
						if(c!=stringBegin && c!=stringEnd)	//if this is not the delimiter that was escaped
						{
							throw new ParseIOException("Unknown escaped character: "+c);
						}
						break;
				}
			}
			stringBuilder.append(c);	//append the character to the string we are constructing
			c=readCharacter(reader);	//read another a character
		}
		return stringBuilder.toString();	//return the string we constructed
	}

	/**Parses a URI surrounded by specified URI delimiters.
	The current position must be that of the first URI delimiter character.
	The new position will be that immediately after the last URI delimiter character.
	@param reader The reader the contents of which to be parsed.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param uriBegin The beginning URI delimiter.
	@param uriEnd The ending URI delimiter.
	@return The label parsed from the reader.
	@exception NullPointerException if the given reader is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters before the current URI is completely parsed.
	*/
	public URI parseURI(final Reader reader, final URI baseURI, final char uriBegin, final char uriEnd) throws IOException, ParseIOException
	{
		check(reader, uriBegin);	//read the beginning URI delimiter
		int c=peek(reader);	//peek the next character
		final String lexicalForm;	//the lexical form, if any
		if(c==STRING_BEGIN)	//check for a string
		{
			lexicalForm=parseString(reader, STRING_BEGIN,	STRING_END);	//parse the string
			c=skipSeparators(reader);	//skip separators and peek the next character
		}
		else	//if no string was encountered
		{
			lexicalForm=null;	//show that there is no string
		}
		final String label;	//the label reference, if any
		if(c==LABEL_BEGIN)	//check for a label
		{
			label=parseLabel(reader);	//parse the label
			c=skipSeparators(reader);	//skip separators and peek the next character
		}
		else	//if no label was encountered
		{
			label=null;	//show that there is no label
		}
		try
		{
			URI uri=new URI(reachAfter(reader, uriEnd));	//read the rest of the URI
			if(label!=null)	//if we have a label, dereference that as a base URI and resolve this URI against it
			{
				final URI localBaseURI=asURI(getResourceProxy(label));	//get a resource proxy for the label and use it as a URI
				uri=resolve(localBaseURI, uri);	//resolve the URI against the local base URI				
			}
			if(lexicalForm!=null)	//if we have a lexical form, use the existing URI as a type and create a lexical URI
			{
				uri=createLexicalURI(uri, lexicalForm);	//create a lexical URI using the lexical form and the type URI
			}
			if(baseURI!=null)	//if there is a base URI
			{
				uri=resolve(baseURI, uri);	//resolve the URI against the base URI				
			}
			return uri;	//return the resulting URI
		}
		catch(final URISyntaxException uriSyntaxException)	//one of the string was not a valid URI
		{
			throw new ParseIOException("Invalid URI: "+uriSyntaxException.getInput());
		}
	}
	
	/**Determines the URI represented by the given resource.
	@param resource The resource which is expected to represent a URI.
	@exception ParseIOException if the given resource has no URI or the URI does not represent a URI.
	*/
	protected static URI asURI(final Resource resource) throws ParseIOException
	{
		final URI resourceURI=resource.getURI();	//get the resource URI
		final String resourceURIFragment=resourceURI!=null && removeFragment(resourceURI).equals(URI_NAMESPACE_URI) ? resourceURI.getFragment() : null;	//get the fragment, if any
		if(resourceURIFragment!=null)	//if this is a URI resource fragment
		{
			try
			{
				return new URI(decode(resourceURIFragment));	//get the URI from the fragment TODO allow multiple resource URIs
			}
			catch(final URISyntaxException uriSyntaxException)	//if the fragment wasn't a valid URI
			{
				throw new ParseIOException("URI resource URI fragment "+resourceURIFragment+" not a valid URI");
			}
		}
		else	//if there is no fragment or this is not a URI namespace
		{
			throw new ParseIOException("Resource "+resource+" not a URI");
		}
	}

	/**Parses properties surrounded by property delimiters.
	The current position must be that of the first property delimiter character.
	The new position will be that immediately after the last property delimiter character.
	@param reader The reader the contents of which to be parsed.
	@param baseURI The base URI of the data, or <code>null</code> if no base URI is available.
	@param subject The resource to serve as the subject for the parsed properties.
	@exception NullPointerException if the given reader and/or subject resource is <code>null</code>.
	@exception IOException if there is an error reading from the reader.
	@exception ParseIOException if the reader has no more characters before the current properties are completely parsed.
	*/
	public void parseProperties(final Reader reader, final URI baseURI, final Resource subject) throws IOException, ParseIOException
	{
		final URF urf=getURF();	//get the URF data model
		check(reader, PROPERTIES_BEGIN);	//read the beginning properties delimiter
		int c=skipSeparators(reader);	//skip separators and peek the next character
		while(c!=PROPERTIES_END)	//while we haven't reached the end of the properties
		{			
			final Resource predicate=parseResource(reader, baseURI);	//parse the predicate resource
			skipSeparators(reader);	//skip separators
			final char propertyValueDelimiter=check(reader, PROPERTY_VALUE_DELIMITERS);	//read the next character and make sure it's a property-value delimiter
			final Resource object;	//we'll use this to store the object, if there is just one object
			switch(skipSeparators(reader))	//skip separators and see what the next character will be
			{
				case SEQUENCE_BEGIN:	//sequence short form
						//TODO finish
					break;
				default:	//assume everything else is a normal resource object
					object=parseResource(reader, baseURI);	//parse the object
					switch(propertyValueDelimiter)	//see what sort of assignment this is
					{
						case PROPERTY_VALUE_DELIMITER:	//property assignment
							addAssertion(new Assertion(subject, predicate, object));	//assert the assertion
							break;
						case PROPERTY_VALUE_CONTEXT_DELIMITER:	//contextual property assignment
							//TODO finish
							break;
						default:
							throw new AssertionError("Unrecognized property-value delimiter: "+propertyValueDelimiter);	//we already checked this character, so we shouldn't get an unknown delimiter here
					}
			}
			c=skipSeparators(reader);	//skip separators and peek the next character
Debug.trace("after property-value pair, peeked", (char)c);
			if(c==LIST_DELIMITER)	//if this is a list delimiter
			{
				check(reader, LIST_DELIMITER);	//skip the list delimiter
				c=skipSeparators(reader);	//skip separators and peek the next character
			}
		}			
		check(reader, PROPERTIES_END);	//read the ending properties delimiter
	}

}