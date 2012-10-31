/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.urframework;

import java.io.*;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.*;

import static java.util.Collections.*;

import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.*;

import org.urframework.content.*;
import org.urframework.select.Select;

import static com.globalmentor.collections.Collections.*;
import static com.globalmentor.io.Charsets.*;

import com.globalmentor.collections.CollectionMap;
import com.globalmentor.collections.IdentityHashSet;
import com.globalmentor.collections.IdentityHashSetMap;
import com.globalmentor.iso.datetime.AbstractISODateTime;
import com.globalmentor.iso.datetime.ISODate;
import com.globalmentor.iso.datetime.ISODateTime;
import com.globalmentor.java.*;

import static com.globalmentor.java.Booleans.*;
import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Java.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.model.Locales.*;
import com.globalmentor.net.*;

import static com.globalmentor.net.URIs.*;

import com.globalmentor.text.RegularExpressions;
import com.globalmentor.util.*;

/**
 * An URF data model. This data model keeps track of all resources that are being created as a linked group, such as parsed from a TURF interchange document,
 * and are thought of as a separate universe of descriptions. This implementation by default registers the following namespace factories for the following
 * namespaces:
 * <dl>
 * <dt>{@value URF#NAMESPACE_URI}</dt>
 * <dd>{@link #DEFAULT_URF_RESOURCE_FACTORY}</dd>
 * <dt>{@value Content#CONTENT_NAMESPACE_URI}</dt>
 * <dd>{@link Content#DEFAULT_CONTENT_RESOURCE_FACTORY}</dd>
 * <dt>{@value Select#SELECT_NAMESPACE_URI}</dt>
 * <dd>{@link JavaURFResourceFactory} for {@link Select#getClass()}</dd>
 * </dl>
 * @author Garret Wilson
 */
public class URF
{

	/** The name of URF. */
	public final static String NAME = "URF";

	/** The URI to the URF namespace. */
	public final static URI NAMESPACE_URI = URI.create("http://urf.name/urf/");
	/** The URI to the URF default namespace. */
	public final static URI DEFAULT_NAMESPACE_URI = URI.create("http://urf.name/default/");
	/** The string form of the URF inline namespace. */
	private final static String BASE_INLINE_NAMESPACE_URI_STRING = "http://urf.name/inline/";
	/** The URF base inline namespace. */
	public final static URI BASE_INLINE_NAMESPACE_URI = URI.create(BASE_INLINE_NAMESPACE_URI_STRING);

	//classes 
	/** The URI of the URF <code>Binary</code> class. */
	public final static URI BINARY_CLASS_URI = createResourceURI(NAMESPACE_URI, "Binary");
	/** The URI of the URF <code>Boolean</code> class. */
	public final static URI BOOLEAN_CLASS_URI = createResourceURI(NAMESPACE_URI, "Boolean");
	/** The URI of the URF <code>Character</code> class. */
	public final static URI CHARACTER_CLASS_URI = createResourceURI(NAMESPACE_URI, "Character");
	/** The URI of the URF <code>Community</code> class. */
	public final static URI COMMUNITY_CLASS_URI = createResourceURI(NAMESPACE_URI, "Community");
	/** The URI of the URF <code>Date</code> class. */
	public final static URI DATE_CLASS_URI = createResourceURI(NAMESPACE_URI, "Date");
	/** The URI of the URF <code>DateTime</code> class. */
	public final static URI DATE_TIME_CLASS_URI = createResourceURI(NAMESPACE_URI, "DateTime");
	/** The URI of the URF <code>Duration</code> class. */
	public final static URI DURATION_CLASS_URI = createResourceURI(NAMESPACE_URI, "Duration");
	/** The URI of the URF <code>Integer</code> class. */
	public final static URI INTEGER_CLASS_URI = createResourceURI(NAMESPACE_URI, "Integer");
	/** The URI of the URF <code>Language</code> class. */
	public final static URI LANGUAGE_CLASS_URI = createResourceURI(NAMESPACE_URI, "Language");
	/** The URI of the URF <code>List</code> class. */
	public final static URI LIST_CLASS_URI = createResourceURI(NAMESPACE_URI, "List");
	/** The URI of the URF <code>Map</code> class. */
	public final static URI MAP_CLASS_URI = createResourceURI(NAMESPACE_URI, "Map");
	/** The URI of the URF <code>MapEntry</code> class. */
	public final static URI MAP_ENTRY_CLASS_URI = createResourceURI(NAMESPACE_URI, "MapEntry");
	/** The URI of the URF <code>Number</code> class. */
	public final static URI NUMBER_CLASS_URI = createResourceURI(NAMESPACE_URI, "Number");
	/** The URI of the URF <code>Ordinal</code> class. */
	public final static URI ORDINAL_CLASS_URI = createResourceURI(NAMESPACE_URI, "Ordinal");
	/** The URI of the URF <code>Proposition</code> class. */
	public final static URI PROPOSITION_CLASS_URI = createResourceURI(NAMESPACE_URI, "Proposition");
	/** The URI of the URF <code>Rational</code> class. */
	public final static URI RATIONAL_CLASS_URI = createResourceURI(NAMESPACE_URI, "Rational");
	/** The URI of the URF <code>RegularExpression</code> class. */
	public final static URI REGULAR_EXPRESSION_CLASS_URI = createResourceURI(NAMESPACE_URI, "RegularExpression");
	/** The URI of the URF <code>Resource</code> class. */
	public final static URI RESOURCE_CLASS_URI = createResourceURI(NAMESPACE_URI, "Resource");
	/** The URI of the URF <code>Set</code> class. */
	public final static URI SET_CLASS_URI = createResourceURI(NAMESPACE_URI, "Set");
	/** The URI of the URF <code>String</code> class. */
	public final static URI STRING_CLASS_URI = createResourceURI(NAMESPACE_URI, "String");
	/** The URI of the URF <code>Temporal</code> class. */
	public final static URI TEMPORAL_CLASS_URI = createResourceURI(NAMESPACE_URI, "Temporal");
	/** The URI of the URF <code>Time</code> class. */
	public final static URI TIME_CLASS_URI = createResourceURI(NAMESPACE_URI, "Time");
	/** The URI of the URF <code>UTCOffset</code> class. */
	public final static URI UTC_OFFSET_CLASS_URI = createResourceURI(NAMESPACE_URI, "UTCOffset");
	/** The URI of the URF <code>URI</code> class. */
	public final static URI URI_CLASS_URI = createResourceURI(NAMESPACE_URI, "URI");

	//properties
	/** The URI of the property indicating an element of a collection such as a set. */
	public final static URI ELEMENT_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "element");
	/** The URI of the property indicating an entry of a map. */
	public final static URI ENTRY_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "entry");
	/** A visual representation associated with the resource. */
	public final static URI ICON_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "icon");
	/** The interface implemented by a class. */
	public final static URI IMPLEMENTATION_OF_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "implementationOf");
	/** The URI of the property indicating the key of a map entry. */
	public final static URI KEY_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "key");
	/**
	 * A short name meant for human consumption which is perhaps more appropriate than display of the class or property name but perhaps less complete than a full
	 * title.
	 */
	public final static URI LABEL_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "label");
	/** The name of a resource meant for machine processing, which may differ from that indicated by the URI, if any. */
	public final static URI NAME_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "name");
	/** The URI of the URF object property. */
	public final static URI OBJECT_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "object");
	/** The URI of the URF order property. */
	public final static URI ORDER_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "order");
	/** The URI of the URF predicate property. */
	public final static URI PREDICATE_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "predicate");
	/** A list of resources to be used to select a resource of a particular type. */
	public final static URI SELECTOR_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "selector");
	/** The URI of the URF subject property. */
	public final static URI SUBJECT_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "subject");
	/** The superclass extended by a class. */
	public final static URI SUBCLASS_OF_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "subclassOf");
	/** The URI of the URF type property. */
	public final static URI TYPE_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "type");
	/** The URI of the property indicating the value of a map entry. */
	public final static URI VALUE_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "value");
	/** The identifier of the version of the resource. */
	public final static URI VERSION_PROPERTY_URI = createResourceURI(NAMESPACE_URI, "version");

	//inline namespaces
	/** The binary inline namespace URI. */
	public final static URI BINARY_NAMESPACE_URI = createInlineNamespaceURI(BINARY_CLASS_URI);
	/** The boolean inline namespace URI. */
	public final static URI BOOLEAN_NAMESPACE_URI = createInlineNamespaceURI(BOOLEAN_CLASS_URI);
	/** The lexical form of the boolean value <code>false</code>. */
	public final static String BOOLEAN_FALSE_LEXICAL_FORM = Boolean.FALSE.toString();
	/** The lexical form of the boolean value <code>true</code>. */
	public final static String BOOLEAN_TRUE_LEXICAL_FORM = Boolean.TRUE.toString();
	/** The URI of the boolean value <code>false</code>. */
	public final static URI BOOLEAN_FALSE_URI = createInlineURI(BOOLEAN_CLASS_URI, BOOLEAN_FALSE_LEXICAL_FORM);
	/** The URI of the boolean value <code>true</code>. */
	public final static URI BOOLEAN_TRUE_URI = createInlineURI(BOOLEAN_CLASS_URI, BOOLEAN_TRUE_LEXICAL_FORM);
	/** The character inline namespace URI. */
	public final static URI CHARACTER_NAMESPACE_URI = createInlineNamespaceURI(CHARACTER_CLASS_URI);
	/** The date inline namespace URI. */
	public final static URI DATE_NAMESPACE_URI = createInlineNamespaceURI(DATE_CLASS_URI);
	/** The date time inline namespace URI. */
	public final static URI DATE_TIME_NAMESPACE_URI = createInlineNamespaceURI(DATE_TIME_CLASS_URI);
	/** The duration inline namespace URI. */
	public final static URI DURATION_NAMESPACE_URI = createInlineNamespaceURI(DURATION_CLASS_URI);
	/** The delimiter that marks the beginning of a duration lexical form. */
	public final static char DURATION_LEXICAL_FORM_BEGIN = 'P';
	/** The delimiter that indicates a duration year. */
	public final static char DURATION_YEARS_DELIMITER = 'Y';
	/** The delimiter that indicates a duration month. */
	public final static char DURATION_MONTHS_DELIMITER = 'M';
	/** The delimiter that indicates a duration day. */
	public final static char DURATION_DAYS_DELIMITER = 'D';
	/** The delimiter that indicates a duration hour. */
	public final static char DURATION_HOURS_DELIMITER = 'H';
	/** The delimiter that indicates a duration minute. */
	public final static char DURATION_MINUTES_DELIMITER = 'M';
	/** The delimiter that indicates a duration second. */
	public final static char DURATION_SECONDS_DELIMITER = 'S';
	/** The integer inline namespace URI. */
	public final static URI INTEGER_NAMESPACE_URI = createInlineNamespaceURI(INTEGER_CLASS_URI);
	/** The URI of the integer value <code>0</code>. */
	public final static URI INTEGER_0_URI = createInlineURI(INTEGER_CLASS_URI, Long.toString(0));
	/** The language inline namespace URI. */
	public final static URI LANGUAGE_NAMESPACE_URI = createInlineNamespaceURI(LANGUAGE_CLASS_URI);
	/** The ordinal inline namespace URI. */
	public final static URI ORDINAL_NAMESPACE_URI = createInlineNamespaceURI(ORDINAL_CLASS_URI);
	/** The URI of the ordinal value <code>0</code>. */
	public final static URI ORDINAL_0_URI = createInlineURI(ORDINAL_CLASS_URI, Long.toString(0));
	/** The rational inline namespace URI. */
	public final static URI RATIONAL_NAMESPACE_URI = createInlineNamespaceURI(RATIONAL_CLASS_URI);
	/** The URI of the rational value <code>0.0</code>. */
	public final static URI RATIONAL_0_URI = createInlineURI(RATIONAL_CLASS_URI, Double.toString(0));
	/** The regular expression inline namespace URI. */
	public final static URI REGULAR_EXPRESSION_NAMESPACE_URI = createInlineNamespaceURI(REGULAR_EXPRESSION_CLASS_URI);
	/** The string inline namespace URI. */
	public final static URI STRING_NAMESPACE_URI = createInlineNamespaceURI(STRING_CLASS_URI);
	/** The URI of the empty string "". */
	public final static URI EMPTY_STRING_URI = createInlineURI(STRING_CLASS_URI, "");
	/** The time inline namespace URI. */
	public final static URI TIME_NAMESPACE_URI = createInlineNamespaceURI(TIME_CLASS_URI);
	/** The URI inline namespace URI. */
	public final static URI URI_NAMESPACE_URI = createInlineNamespaceURI(URI_CLASS_URI);
	/** The UTC offset inline namespace URI. */
	public final static URI UTC_OFFSET_NAMESPACE_URI = createInlineNamespaceURI(UTC_OFFSET_CLASS_URI);

	//general lexical delimiters
	/** The delimiter for decimal separation of numbers and times. */
	public final static char DECIMAL_DELIMITER = '.';
	/** The delimiter for exponent separation of numbers. */
	public final static char EXPONENT_DELIMITER = 'e';
	/** The signs of a number. */
	public final static char[] SIGNS = { '-', '+' };

	/** The shared empty array of resources. */
	public final static URFResource[] NO_RESOURCES = new URFResource[0];

	/** The atomic variable used to generate scope creation orders. */
	private final static AtomicLong scopeCreationOrder = new AtomicLong(0);

	/**
	 * Generates a new scope creation order unique to this JVM.
	 * @return A new scope creation order unique to this JVM.
	 */
	public static long generateScopeCreationOrder()
	{
		return scopeCreationOrder.getAndIncrement(); //atomically get the next counter value
	}

	/**
	 * Checks to ensure that the given URI meets the criteria of an URF namespace URI. In particular, the URI must be an absolute plain collection URI.
	 * @param namespaceURI The URI to check.
	 * @return The given namespace URI.
	 * @throws NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI does not meet the criteria of an URF namespace URI.
	 * @see URIs#checkAbsolute(URI)
	 * @see URIs#checkCollectionURI(URI)
	 * @see URIs#checkPlainURI(URI)
	 */
	public static URI checkNamespaceURI(final URI namespaceURI)
	{
		return checkPlainURI(checkCollectionURI(checkAbsolute(namespaceURI)));
	}

	/**
	 * Creates a resource URI from a local name. The default namespace is assumed.
	 * @param localName The unencoded local name of the resource.
	 * @return A URI constructed from the default namespace and the given local name.
	 * @exception NullPointerException if the given local name is <code>null</code>.
	 * @see #DEFAULT_NAMESPACE_URI
	 */
	public static URI createResourceURI(final String localName)
	{
		return createResourceURI(DEFAULT_NAMESPACE_URI, localName); //use the default namespace
	}

	/**
	 * Creates a resource URI from a given namespace URI and a local name. The local name is encoded and appended to the URI.
	 * @param namespaceURI The URI of the namespace.
	 * @param localName The unencoded local name of the resource.
	 * @return A URI constructed from the given namespace URI and local name.
	 * @exception NullPointerException if the given namespace URI and/or local name is <code>null</code>.
	 * @exception IllegalArgumentException if the given URI is not a valid namespace URI.
	 */
	public static URI createResourceURI(final URI namespaceURI, final String localName)
	{
		return resolve(checkNamespaceURI(namespaceURI), encode(localName)); //encode the local name and resolve it to the namespace
	}

	/**
	 * Retrieves the namespace from the given URI. The namespace URI is the parent collection of the URI.
	 * @param uri The URI from which a namespace should be retrieved.
	 * @return The namespace represented by the given URI, or <code>null</code> if the URI has no path or the path ends with a path separator.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 */
	public static URI getNamespaceURI(final URI uri)
	{
		if(uri.isAbsolute()) //only absolute URIs can be namespaces
		{
			final String rawPath = uri.getRawPath(); //get the raw path
			if(rawPath != null && !endsWith(rawPath, PATH_SEPARATOR)) //if there is a raw path that isn't a collection
			{
				return getCurrentLevel(uri); //return the base level of the URI without the local name
			}
		}
		return null; //indicate that this URI has no namespace
	}

	/**
	 * Retrieves the local name from the given URI. The local name is the decoded last path segment of the URI.
	 * @param uri The URI from which a namespace should be retrieved.
	 * @return The local name of the given URI, or <code>null</code> if the URI has no path or the path ends with a path separator.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 */
	public static String getLocalName(final URI uri)
	{
		final String rawPath = uri.getRawPath(); //get the raw path
		if(rawPath != null && !endsWith(rawPath, PATH_SEPARATOR)) //if there is a raw path that isn't a collection
		{
			return decode(getName(rawPath)); //return the name from the raw path
		}
		return null; //indicate that this URI has no namespace
	}

	/**
	 * Retrieves the lexical form stored in the local name of the given lexical URI. The lexical value is the decoded form of the value stored inside
	 * <code>"..."</code> in the local name.
	 * @param uri The inline URI from which a lexical form should be retrieved.
	 * @return The lexical form stored in the given URI, or <code>null</code> if the URI has no path or the path ends with a path separator.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if there is no valid lexical form.
	 * @see #getLocalName(URI)
	 */
	public static String getInlineLexicalForm(final URI uri)
	{
		final String localName = getLocalName(uri); //start with the decoded local name
		if(localName != null)
		{
			final StringReader localNameReader = new StringReader(localName);
			try
			{
				return URFTURFProcessor.parseString(localNameReader); //parse a string from the local name
			}
			catch(final IOException ioException)
			{
				throw new AssertionError(ioException);
			}
			catch(final DataException dataException)
			{
				throw new IllegalArgumentException("URI " + uri + " has an invalid inline value: " + dataException.getMessage(), dataException);
			}
		}
		throw new IllegalArgumentException("URI " + uri + " has no inline value.");
	}

	/**
	 * Determines whether the given namespace URI the URI of an inline namespace.
	 * @param namespaceURI The URI to check for being that of an inline namespace
	 * @return <code>true</code> if the URI is that of an inline namespace.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid namespace URI.
	 */
	public static boolean isInlineNamespaceURI(final URI namespaceURI)
	{
		return checkNamespaceURI(namespaceURI).toString().startsWith(BASE_INLINE_NAMESPACE_URI_STRING); //see if this is namespace URI that starts with the inline namespace URI string
	}

	/**
	 * Determines whether the given namespace URI is the URI of an inline namespace.
	 * @param inlineNamespaceURI The URI to check for being that of an inline namespace
	 * @return The inline namespace URI.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 * @throws IllegalArgumentException if the given URI is not a valid inline namespace URI.
	 */
	public static URI checkInlineNamespaceURI(final URI inlineNamespaceURI)
	{
		if(!isInlineNamespaceURI(inlineNamespaceURI)) //if this is not an inline namespace URI
		{
			throw new IllegalArgumentException("URI " + inlineNamespaceURI + " is not an inline namespace URI.");
		}
		return inlineNamespaceURI;
	}

	/**
	 * Determines whether the given URI is in an inline namespace. This method returns <code>false</code> for inline namespaces themselves.
	 * @param uri The URI to check for being in an inline namespace.
	 * @return <code>true</code> if the URI is is in an inline namespace.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 * @see #isInlineNamespaceURI(URI)
	 */
	public static boolean isInlineURI(final URI uri)
	{
		final URI namespaceURI = getNamespaceURI(uri); //get the namespace of the URI, if any
		return namespaceURI != null && isInlineNamespaceURI(namespaceURI); //see if there is a namespace URI that is a lexical namespace URI
	}

	/**
	 * Determines whether the given URI is in an inline namespace with the given type. This method returns <code>false</code> for inline namespaces themselves.
	 * @param uri The URI to check for being in an inline namespace with the given inline type.
	 * @param inlineTypeURI The URI of the type of the resource.
	 * @return <code>true</code> if the URI is is in an inline namespace with the given inline type.
	 * @exception NullPointerException if the given URI and/or inline type URI is <code>null</code>.
	 * @see #isInlineURI(URI)
	 * @see #getInlineTypeURI(URI)
	 */
	public static boolean isInlineTypeURI(final URI uri, final URI inlineTypeURI)
	{
		return isInlineURI(uri) && inlineTypeURI.equals(getInlineTypeURI(uri)); //see if the URI is a lexical URI with the given lexical type
	}

	/**
	 * Retrieves the type URI of a URI in an inline namespace. This method throws an exception for inline namespaces themselves.
	 * @param inlineURI A URI in an inline namespace.
	 * @return The type URI of the namespace of the inline URI.
	 * @exception IllegalArgumentException if the given URI is not in an inline namespace.
	 * @exception IllegalArgumentException if the given URI's inline namespace URI does not have a correctly encoded type URI.
	 * @see #getInlineNamespaceTypeURI(URI)
	 */
	public static URI getInlineTypeURI(final URI inlineURI)
	{
		final URI namespaceURI = getNamespaceURI(inlineURI); //get the namespace of the URI
		if(namespaceURI == null) //if this URI has no namespace
		{
			throw new IllegalArgumentException("URI " + inlineURI + " is not in any namespace.");
		}
		return getInlineNamespaceTypeURI(namespaceURI);
	}

	/**
	 * Retrieves the type URI of an inline namespace URI.
	 * @param namespaceURI The URI of an inline namespace.
	 * @return The type URI of the inline namespace.
	 * @exception IllegalArgumentException if the given URI is not an inline namespace.
	 * @exception IllegalArgumentException if the given URI's inline namespace URI does not have a correctly encoded type URI.
	 */
	public static URI getInlineNamespaceTypeURI(final URI namespaceURI)
	{
		final String inlineNamespaceURIString = checkNamespaceURI(namespaceURI).toString(); //get the string version of the namespace URI
		if(!inlineNamespaceURIString.startsWith(BASE_INLINE_NAMESPACE_URI_STRING)) //if this URI doesn't start with the base inline namespace URI
		{
			throw new IllegalArgumentException("URI " + namespaceURI + " is not an inline namespace URI.");
		}
		return URI.create(decode(inlineNamespaceURIString.substring(BASE_INLINE_NAMESPACE_URI_STRING.length(), inlineNamespaceURIString.length() - 1))); //retrieve the type substring and decode it
	}

	/**
	 * Creates an inline namespace URI for the given resource type.
	 * @param typeURI The URI of the type of the resource.
	 * @return The inline namespace for the specified type.
	 * @exception NullPointerException if the given type URI is <code>null</code>.
	 */
	public static URI createInlineNamespaceURI(final URI typeURI)
	{
		return URI.create(BASE_INLINE_NAMESPACE_URI.toString() + encode(typeURI.toString()) + PATH_SEPARATOR); //encode the type and append it to the inline namespace URI
	}

	/**
	 * Creates a URI in an inline namespace for the given resource type and lexical form.
	 * @param typeURI The URI of the type of the resource.
	 * @param lexicalForm The canonical lexical form of the resource.
	 * @return A URI in the inline namespace for the specified type of a resource based upon its lexical form.
	 * @exception NullPointerException if the given type URI and/or lexical form is <code>null</code>.
	 * @exception IllegalArgumentException if the given type URI is not absolute.
	 */
	public static URI createInlineURI(final URI typeURI, final String lexicalForm)
	{
		return createResourceURI(createInlineNamespaceURI(typeURI), URFTURFGenerator.toStringString(lexicalForm)); //get the appropriate inline namespace and form a resource URI from the lexical form of the local name
	}

	/**
	 * Creates a URI to represent URF binary data.
	 * @param binary The binary value to represent.
	 * @return A URI representing the given URF binary data.
	 * @exception NullPointerException if the given binary data is <code>null</code>.
	 * @see #BINARY_CLASS_URI
	 */
	public static URI createBinaryURI(final byte[] binary)
	{
		return createInlineURI(BINARY_CLASS_URI, Base64.encodeBytes(binary, Base64.URL_SAFE & Base64.DONT_BREAK_LINES)); //encode the binary data and create a URI from base64url form
	}

	/**
	 * Creates a URI to represent an URF boolean.
	 * @param b The boolean value to represent.
	 * @return A URI representing the given URF boolean.
	 * @see #BOOLEAN_CLASS_URI
	 */
	public static URI createBooleanURI(final boolean b)
	{
		return b ? BOOLEAN_TRUE_URI : BOOLEAN_FALSE_URI; //return the pre-made boolean true or false URI as appropriate
	}

	/**
	 * Creates a URI to represent an URF character.
	 * @param c The character value to represent.
	 * @return A URI representing the given URF character.
	 * @see #CHARACTER_CLASS_URI
	 */
	public static URI createCharacterURI(final char c)
	{
		return createInlineURI(CHARACTER_CLASS_URI, String.valueOf(c)); //create a character URI
	}

	/**
	 * Creates a URI to represent an URF date time.
	 * @param dateTime The date time to represent.
	 * @return A URI representing the given URF date time.
	 * @exception NullPointerException if the given date time is <code>null</code>.
	 * @see #DATE_TIME_CLASS_URI
	 */
	public static URI createDateTimeURI(final Date dateTime)
	{
		return createInlineURI(DATE_TIME_CLASS_URI, URFDateFormat.format(dateTime, URFDateFormat.Style.DATE_TIME)); //create a date time URI
	}

	/**
	 * Creates a URI to represent an URF string.
	 * @param string The string value to represent.
	 * @return A URI representing the given URF string.
	 * @exception NullPointerException if the given string is <code>null</code>.
	 * @see #STRING_CLASS_URI
	 */
	public static URI createStringURI(final String string)
	{
		return string.isEmpty() ? EMPTY_STRING_URI : createInlineURI(STRING_CLASS_URI, string); //create a string URI, using the pre-made empty string URI if we can
	}

	/**
	 * Creates a URI to represent an URF integer.
	 * @param integer The integer value to represent.
	 * @return A URI representing the given URF integer.
	 * @see #INTEGER_CLASS_URI
	 */
	public static URI createIntegerURI(final long integer)
	{
		return integer == 0 ? INTEGER_0_URI : createInlineURI(INTEGER_CLASS_URI, Long.toString(integer)); //create an integer URI, using the pre-made zero integer URI if we can
	}

	/**
	 * Creates a URI to represent an URF language.
	 * @param language The language to represent.
	 * @return A URI representing the given URF language.
	 * @exception NullPointerException if the given language is <code>null</code>.
	 * @see #LANGUAGE_CLASS_URI
	 */
	public static URI createLanguageURI(final Locale language)
	{
		return createInlineURI(LANGUAGE_CLASS_URI, getLanguageTag(language)); //create a language URI
	}

	/**
	 * Creates a URI to represent an URF ordinal.
	 * @param ordinal The ordinal value to represent.
	 * @return A URI representing the given URF ordinal.
	 * @exception IllegalArgumentException if the given ordinal is negative.
	 * @see #ORDINAL_CLASS_URI
	 */
	public static URI createOrdinalURI(final long ordinal)
	{
		return ordinal == 0 ? ORDINAL_0_URI : createInlineURI(ORDINAL_CLASS_URI, Long.toString(checkArgumentNotNegative(ordinal))); //create an ordinal URI, using the pre-made zero ordinal URI if we can and making sure that the value is not less than zero
	}

	/**
	 * Creates a URI to represent an URF rational.
	 * @param rational The rational value to represent.
	 * @return A URI representing the given URF rational.
	 * @see #RATIONAL_CLASS_URI
	 */
	public static URI createRationalURI(final double rational)
	{
		return rational == 0.0 ? RATIONAL_0_URI : createInlineURI(RATIONAL_CLASS_URI, Double.toString(rational)); //create a rational URI, using the pre-made zero rational URI if we can
	}

	/**
	 * Creates a URI to represent an URF regular expression.
	 * @param pattern The regular expression value to represent.
	 * @return A URI representing the given URF regular expression.
	 * @exception NullPointerException if the given pattern is <code>null</code>.
	 * @see #REGULAR_EXPRESSION_CLASS_URI
	 */
	public static URI createRegularExpressionURI(final Pattern pattern)
	{
		return createInlineURI(REGULAR_EXPRESSION_CLASS_URI, pattern.toString()); //create a regular expression URI
	}

	/**
	 * Creates a URI to represent an URF URI.
	 * @param uri The URI value to represent.
	 * @return A URI representing the given URF URI.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 * @see #URI_CLASS_URI
	 */
	public static URI createURIURI(final URI uri)
	{
		return createInlineURI(URI_CLASS_URI, uri.toString()); //create a URI URI
	}

	/**
	 * Returns an array containing the URIs of the given resources.
	 * @param resources The resources of which URIs should be returned.
	 * @return The URIs of the given resources.
	 * @exception NullPointerException if one of the given resources is <code>null</code>.
	 */
	public static URI[] getURIs(final URFResource... resources)
	{
		final int resourceCount = resources.length; //find out how many resources there are
		final URI[] uris = new URI[resourceCount]; //create a URI array of appropriate length
		for(int i = 0; i < resourceCount; ++i) //for each resource
		{
			uris[i] = resources[i].getURI(); //get this resource's URI
		}
		return uris; //return the resource URIs
	}

	/**
	 * Determines the resource URI to represent the given Java object, if possible This method can return resource URIs in the given namespaces for the following
	 * types of objects:
	 * <dl>
	 * <dt><code>byte[]</code></dt>
	 * <dd>{@value #BINARY_NAMESPACE_URI}</dd>
	 * <dt>{@link Boolean}</dt>
	 * <dd>{@value #BOOLEAN_NAMESPACE_URI}</dd>
	 * <dt>{@link Character}</dt>
	 * <dd>{@value #CHARACTER_NAMESPACE_URI}</dd>
	 * <dt>{@link Charset}</dt>
	 * <dd>{@value Content#CHARSET_NAMESPACE_URI}</dd>
	 * <dt>{@link ContentType}</dt>
	 * <dd>{@value Content#MEDIA_TYPE_NAMESPACE_URI}</dd>
	 * <dt>{@link Integer}</dt>
	 * <dd>{@value #INTEGER_NAMESPACE_URI}</dd>
	 * <dt>{@link Double}</dt>
	 * <dd>{@value #RATIONAL_NAMESPACE_URI}</dd>
	 * <dt>{@link Float}</dt>
	 * <dd>{@value #RATIONAL_NAMESPACE_URI}</dd>
	 * <dt>{@link Locale}</dt>
	 * <dd>{@value #LANGUAGE_NAMESPACE_URI}</dd>
	 * <dt>{@link Long}</dt>
	 * <dd>{@value #INTEGER_NAMESPACE_URI}</dd>
	 * <dt>{@link Pattern}</dt>
	 * <dd>{@value #REGULAR_EXPRESSION_NAMESPACE_URI}</dd>
	 * <dt>{@link String}</dt>
	 * <dd>{@value #STRING_NAMESPACE_URI}</dd>
	 * <dt>{@link URI}</dt>
	 * <dd>{@value #URI_NAMESPACE_URI}</dd>
	 * </dl>
	 * This method can return resource URIs using the following schemes for objects of the following types:
	 * <dl>
	 * <dt>{@link Class}</dt>
	 * <dd>{@value Java#JAVA_URI_SCHEME}</dd>
	 * <dt>{@link Package}</dt>
	 * <dd>{@value Java#JAVA_URI_SCHEME}</dd>
	 * </dl>
	 * This method can return inline resource URIs with inline type URIs using the following schemes for objects of the following types:
	 * <dl>
	 * <dt>{@link Class} subclass of {@link Enum}</dt>
	 * <dd>{@value Java#JAVA_URI_SCHEME}</dd>
	 * </dl>
	 * @param resourceURI The URI to represent as a Java object, or <code>null</code>.
	 * @return An object representing the resource represented by the given URI, or <code>null</code> if the URI does not represent a known object.
	 * @exception IllegalArgumentException if the given URI represents an object but does not have the correct syntax for that object.
	 * @exception ClassNotFoundException if the class represented by the given resource could not be found.
	 */
	public static URI asResourceURI(final Object object)
	{
		if(object != null) //if an object was given
		{
			//inline URIs
			if(object instanceof byte[])//if this is an byte array
			{
				return createBinaryURI((byte[])object); //return a binary URI
			}
			else if(object instanceof Boolean) //if this is a boolean
			{
				return createBooleanURI(((Boolean)object).booleanValue()); //return a boolean URI
			}
			else if(object instanceof Character) //if this is a character
			{
				return createCharacterURI(((Character)object).charValue()); //return a character URI
			}
			else if(object instanceof Charset) //if this is a charset
			{
				return Content.createCharsetURI(((Charset)object)); //return a charset URI
			}
			else if(object instanceof ContentType) //if this is a content type
			{
				return Content.createMediaTypeURI(((ContentType)object)); //return a media type URI
			}
			else if(object instanceof Integer) //if this is an integer
			{
				return createIntegerURI(((Integer)object).longValue()); //return an integer URI
			}
			else if(object instanceof Double) //if this is a double
			{
				return createRationalURI(((Double)object).doubleValue()); //return a rational URI
			}
			else if(object instanceof Enum<?>) //if this is an enum
			{
				return createInlineURI(Classes.createJavaURI(object.getClass()), ((Enum<?>)object).name()); //return an inline URI using the enum class as the inline type and the name of the enum as the local name
			}
			else if(object instanceof Float) //if this is a float
			{
				return createRationalURI(((Float)object).doubleValue()); //return a rational URI
			}
			else if(object instanceof Long) //if this is a long
			{
				return createIntegerURI(((Long)object).longValue()); //return an integer URI
			}
			else if(object instanceof Locale) //if this is a locale
			{
				return createLanguageURI(((Locale)object)); //return a language URI
			}
			else if(object instanceof Pattern) //if this is a pattern
			{
				return createRegularExpressionURI(((Pattern)object)); //return a regular expression URI
			}
			else if(object instanceof String) //if this is a string
			{
				return createStringURI(((String)object)); //return a string URI
			}
			else if(object instanceof URI) //if this is a URI
			{
				return createURIURI(((URI)object)); //return a URI URI
			}
			//other schemes
			else if(object instanceof Class<?>) //if this is a class
			{
				return Classes.createJavaURI((Class<?>)object); //create a java: URI for a class
			}
			else if(object instanceof Package) //if this is a package
			{
				return Packages.createJavaURI((Package)object); //create a java: URI for a package
			}
		}
		return null; //we can't represent this object as a resource URI
	}

	/**
	 * Determines the URF collection object, if any, represented by the given resource.
	 * @param resource The resource which is expected to represent an URF collection, or <code>null</code>.
	 * @return The URF collection object represented by the given resource, or <code>null</code> if the resource is not an instance of
	 *         {@link URFCollectionResource}.
	 */
	@SuppressWarnings("unchecked")
	//we must trust that they asked for the correct generic type; a class cast exception will be thrown later if the incorrect generic type was requested
	public static <T extends URFResource> URFCollectionResource<T> asCollectionInstance(final Resource resource)
	{
		return resource instanceof URFCollectionResource ? (URFCollectionResource<T>)resource : null; //if a collection was given, return it with the requested generic type
	}

	/**
	 * Determines the URF list object, if any, represented by the given resource.
	 * @param resource The resource which is expected to represent an URF list, or <code>null</code>.
	 * @return The URF list object represented by the given resource, or <code>null</code> if the resource is not an instance of {@link URFListResource}.
	 */
	@SuppressWarnings("unchecked")
	//we must trust that they asked for the correct generic type; a class cast exception will be thrown later if the incorrect generic type was requested
	public static <T extends URFResource> URFListResource<T> asListInstance(final Resource resource)
	{
		return resource instanceof URFListResource ? (URFListResource<T>)resource : null; //if a list was given, return it with the requested generic type
	}

	/**
	 * Determines the URF set object, if any, represented by the given resource.
	 * @param resource The resource which is expected to represent an URF set, or <code>null</code>.
	 * @return The URF set object represented by the given resource, or <code>null</code> if the resource is not an instance of {@link URFSetResource}.
	 */
	@SuppressWarnings("unchecked")
	//we must trust that they asked for the correct generic type; a class cast exception will be thrown later if the incorrect generic type was requested
	public static <T extends URFResource> URFSetResource<T> asSetInstance(final Resource resource)
	{
		return resource instanceof URFSetResource ? (URFSetResource<T>)resource : null; //if a set was given, return it with the requested generic type
	}

	/**
	 * Determines the URF map object, if any, represented by the given resource.
	 * @param resource The resource which is expected to represent an URF map, or <code>null</code>.
	 * @return The URF map object represented by the given resource, or <code>null</code> if the resource is not an instance of {@link URFMapResource}.
	 */
	@SuppressWarnings("unchecked")
	//we must trust that they asked for the correct generic type; a class cast exception will be thrown later if the incorrect generic type was requested
	public static <K extends URFResource, V extends URFResource> URFMapResource<K, V> asMapInstance(final Resource resource)
	{
		return resource instanceof URFMapResource ? (URFMapResource<K, V>)resource : null; //if a map was given, return it with the requested generic type
	}

	/**
	 * Determines the URF map entry object, if any, represented by the given resource.
	 * @param resource The resource which is expected to represent an URF map entry, or <code>null</code>.
	 * @return The URF map entry object represented by the given resource, or <code>null</code> if the resource is not an instance of {@link URFMapEntryResource}.
	 */
	@SuppressWarnings("unchecked")
	//we must trust that they asked for the correct generic type; a class cast exception will be thrown later if the incorrect generic type was requested
	public static <K extends URFResource, V extends URFResource> URFMapEntryResource<K, V> asMapEntryInstance(final Resource resource)
	{
		return resource instanceof URFMapEntryResource ? (URFMapEntryResource<K, V>)resource : null; //if a map entry was given, return it with the requested generic type
	}

	/**
	 * Determines the Java object represented by the given resource based solely upon its URI.
	 * @param resource The resource which is expected to represent a Java object, or <code>null</code>.
	 * @return The object represented by the given resource, or <code>null</code> if the resource does not represent a known object based upon its URI.
	 * @exception IllegalArgumentException if the given resource represents an object but does not have the correct syntax for that object.
	 * @see #asObject(URI)
	 */
	public static Object asObject(final Resource resource)
	{
		return resource != null ? asObject(resource.getURI()) : null; //if a resource was given, see if its URI represents an object
	}

	/**
	 * Determines the Java object to represent the given URI, if possible This method can return objects for the URIs in the following namespaces:
	 * <dl>
	 * <dt>{@value #BINARY_NAMESPACE_URI}</dt>
	 * <dd><code>byte[]</code></dd>
	 * <dt>{@value #BOOLEAN_NAMESPACE_URI}</dt>
	 * <dd>{@link Boolean}</dd>
	 * <dt>{@value #CHARACTER_NAMESPACE_URI}</dt>
	 * <dd>{@link Character}</dd>
	 * <dt>{@value #INTEGER_NAMESPACE_URI}</dt>
	 * <dd>{@link Long}</dd>
	 * <dt>{@value #LANGUAGE_NAMESPACE_URI}</dt>
	 * <dd>{@link Locale}</dd>
	 * <dt>{@value Content#CHARSET_NAMESPACE_URI}</dt>
	 * <dd>{@link Charset}</dd>
	 * <dt>{@value Content#MEDIA_TYPE_NAMESPACE_URI}</dt>
	 * <dd>{@link ContentType}</dd>
	 * <dt>{@value #DATE_NAMESPACE_URI}</dt>
	 * <dd>{@link ISODate}</dd>
	 * <dt>{@value #DATE_TIME_NAMESPACE_URI}</dt>
	 * <dd>{@link ISODateTime}</dd>
	 * <dt>{@value #ORDINAL_NAMESPACE_URI}</dt>
	 * <dd>{@link Long}</dd>
	 * <dt>{@value #RATIONAL_NAMESPACE_URI}</dt>
	 * <dd>{@link Double}</dd>
	 * <dt>{@value #REGULAR_EXPRESSION_NAMESPACE_URI}</dt>
	 * <dd>{@link RegularExpressions}</dd>
	 * <dt>{@value #STRING_NAMESPACE_URI}</dt>
	 * <dd>{@link String}</dd>
	 * <dt>{@value #URI_NAMESPACE_URI}</dt>
	 * <dd>{@link URI}</dd>
	 * </dl>
	 * This method can return objects for the resources with URIs of the following schemes:
	 * <dl>
	 * <dt>{@value Java#JAVA_URI_SCHEME}</dt>
	 * <dd>{@link Class}</dd>
	 * <dt>{@value URIs#PATH_SCHEME}</dt>
	 * <dd>{@link URIPath}</dd>
	 * </dl>
	 * This method can return objects for the resources with inline URIs with inline type URIs of the following schemes:
	 * <dl>
	 * <dt>{@value Java#JAVA_URI_SCHEME} indicating subclass of {@link Enum}</dt>
	 * <dd>{@link Enum}</dd>
	 * </dl>
	 * @param resourceURI The URI to represent as a Java object, or <code>null</code>.
	 * @return An object representing the resource represented by the given URI, or <code>null</code> if the URI does not represent a known object.
	 * @exception IllegalArgumentException if the given URI represents an object but does not have the correct syntax for that object.
	 * @exception ClassNotFoundException if the class represented by the given resource could not be found.
	 */
	@SuppressWarnings("unchecked")
	public static Object asObject(final URI resourceURI)
	{
		if(resourceURI != null) //if a resource URI was given
		{
			final String resourceURIScheme = resourceURI.getScheme(); //get the resource URI scheme, if any
			if(JAVA_URI_SCHEME.equals(resourceURIScheme)) //if this is a Java URI
			{
				try
				{
					return Classes.asClass(resourceURI); //return a class
				}
				catch(final ClassNotFoundException classNotFoundException)
				{
					throw new IllegalArgumentException(classNotFoundException);
				}
			}
			else if(PATH_SCHEME.equals(resourceURIScheme)) //if this is a path
			{
				return asURIPath(resourceURI); //return a URI path
			}
			else if(isInlineURI(resourceURI)) //if the resource URI is an inline URI
			{
				final URI inlineTypeURI = getInlineTypeURI(resourceURI); //get the inline type
				try
				{
					final Class<?> inlineClass = Classes.asClass(inlineTypeURI); //see if this is inline type represents a Java class
					if(inlineClass != null && Enum.class.isAssignableFrom(inlineClass)) //if the inline type is an enum
					{
						return Enum.valueOf((Class<? extends Enum>)inlineClass, getInlineLexicalForm(resourceURI)); //create an enum using the given inline type and enum value
					}
				}
				catch(final ClassNotFoundException classNotFoundException)
				{
					throw new IllegalArgumentException(classNotFoundException);
				}
			}
			final URI namespaceURI = getNamespaceURI(resourceURI); //get the URI namespace
			if(namespaceURI != null) //if this URI has a namespace
			{
				if(BINARY_NAMESPACE_URI.equals(namespaceURI)) //binary
				{
					return asBinary(resourceURI); //return an array of bytes
				}
				else if(BOOLEAN_NAMESPACE_URI.equals(namespaceURI)) //boolean
				{
					return asBoolean(resourceURI); //return a boolean
				}
				else if(CHARACTER_NAMESPACE_URI.equals(namespaceURI)) //character
				{
					return asCharacter(resourceURI); //return a character
				}
				else if(Content.CHARSET_NAMESPACE_URI.equals(namespaceURI)) //charset
				{
					return Content.asCharset(resourceURI); //return a charset
				}
				else if(DATE_NAMESPACE_URI.equals(namespaceURI)) //date
				{
					return asDate(resourceURI); //return a date
				}
				else if(DATE_TIME_NAMESPACE_URI.equals(namespaceURI)) //date time
				{
					return asDateTime(resourceURI); //return a date time
				}
				else if(INTEGER_NAMESPACE_URI.equals(namespaceURI)) //integer
				{
					return asInteger(resourceURI); //return a long
				}
				else if(LANGUAGE_NAMESPACE_URI.equals(namespaceURI)) //language
				{
					return asLanguage(resourceURI); //return a language
				}
				else if(Content.MEDIA_TYPE_NAMESPACE_URI.equals(namespaceURI)) //media type
				{
					return Content.asMediaType(resourceURI); //return a media type
				}
				else if(ORDINAL_NAMESPACE_URI.equals(namespaceURI)) //ordinal
				{
					return asOrdinal(resourceURI); //return a long
				}
				else if(RATIONAL_NAMESPACE_URI.equals(namespaceURI)) //rational
				{
					return asRational(resourceURI); //return a rational
				}
				else if(REGULAR_EXPRESSION_NAMESPACE_URI.equals(namespaceURI)) //regular expression
				{
					return asPattern(resourceURI); //return a pattern
				}
				else if(STRING_NAMESPACE_URI.equals(namespaceURI)) //string
				{
					return asString(resourceURI); //return a string
				}
				else if(URI_NAMESPACE_URI.equals(namespaceURI)) //URI
				{
					return asURI(resourceURI); //return a URI
				}
			}
		}
		return null; //we can't represent this URI as an object
	}

	/**
	 * Determines the binary data represented by the given resource.
	 * @param resource The resource which is expected to represent binary data, or <code>null</code>.
	 * @return The binary data represented by the given resource, or <code>null</code> if the resource does not represent binary data.
	 * @exception IllegalArgumentException if the given resource represents binary data that does not have the correct syntax.
	 * @see #asBinary(URI)
	 */
	public static byte[] asBinary(final Resource resource)
	{
		return resource != null ? asBinary(resource.getURI()) : null; //if a resource was given, see if its URI represents binary data
	}

	/**
	 * Determines the binary data represented by the given URI.
	 * @param resourceURI The URI which is expected to represent binary data, or <code>null</code>.
	 * @return The binary data represented by the given URI, or <code>null</code> if the URI does not represent binary data.
	 * @exception IllegalArgumentException if the given URI represents binary data that does not have the correct syntax.
	 * @see #BINARY_CLASS_URI
	 * @see #BINARY_NAMESPACE_URI
	 */
	public static byte[] asBinary(final URI resourceURI)
	{
		if(resourceURI != null && BINARY_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a binary URI was given
		{
			final String base64urlString = getInlineLexicalForm(resourceURI); //get the base64url-encoded binary data from the value
			return Base64.decode(base64urlString.getBytes(UTF_8_CHARSET), 0, base64urlString.length(), Base64.URL_SAFE & Base64.DONT_BREAK_LINES); //decode and return the data
		}
		return null; //no boolean could be found
	}

	/**
	 * Determines the boolean represented by the given resource.
	 * @param resource The resource which is expected to represent a boolean, or <code>null</code>.
	 * @return The boolean represented by the given resource, or <code>null</code> if the resource does not represent a boolean.
	 * @exception IllegalArgumentException if the given resource represents a boolean that does not have the correct syntax.
	 * @see #asBoolean(URI)
	 */
	public static Boolean asBoolean(final Resource resource)
	{
		return resource != null ? asBoolean(resource.getURI()) : null; //if a resource was given, see if its URI represents a boolean
	}

	/**
	 * Determines the boolean represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a boolean , or <code>null</code>.
	 * @return The boolean represented by the given URI, or <code>null</code> if the URI does not represent a boolean.
	 * @exception IllegalArgumentException if the given URI represents a boolean that does not have the correct syntax.
	 * @see #BOOLEAN_CLASS_URI
	 * @see #BOOLEAN_NAMESPACE_URI
	 */
	public static Boolean asBoolean(final URI resourceURI)
	{
		if(resourceURI != null && BOOLEAN_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a boolean URI was given
		{
			return parseBoolean(getInlineLexicalForm(resourceURI)); //create a boolean from the value
		}
		return null; //no boolean could be found
	}

	/**
	 * Determines the character represented by the given resource.
	 * @param resource The resource which is expected to represent a character, or <code>null</code>.
	 * @return The character represented by the given resource, or <code>null</code> if the resource does not represent a character.
	 * @exception IllegalArgumentException if the given resource represents a character that does not have the correct syntax.
	 * @see #asCharacter(URI)
	 */
	public static Character asCharacter(final Resource resource)
	{
		return resource != null ? asCharacter(resource.getURI()) : null; //if a resource was given, see if its URI represents a character
	}

	/**
	 * Determines the character represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a character , or <code>null</code>.
	 * @return The character represented by the given URI, or <code>null</code> if the URI does not represent a character.
	 * @exception IllegalArgumentException if the given URI represents a character that does not have the correct syntax.
	 * @see #CHARACTER_CLASS_URI
	 * @see #CHARACTER_NAMESPACE_URI
	 */
	public static Character asCharacter(final URI resourceURI)
	{
		if(resourceURI != null && CHARACTER_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a character URI was given
		{
			return parseCharacter(getInlineLexicalForm(resourceURI)); //create a character from the value
		}
		return null; //no boolean could be found
	}

	/**
	 * Determines the Java class represented by the given resource. A resource represents a Java class if it has a {@value Java#JAVA_URI_SCHEME} scheme URI in the
	 * form <code>java:/<var>com</var>/<var>example</var>/<var>package</var>/<var>Class</var></code>.
	 * @param resource The resource which is expected to represent a Java class, or <code>null</code>.
	 * @return The Java class represented by the given resource, or <code>null</code> if the resource does not represent a Java class.
	 * @exception IllegalArgumentException if the given resource represents a Java class that does not have the correct syntax.
	 * @exception ClassNotFoundException if the class represented by the given resource could not be found.
	 * @see Classes#asClass(URI)
	 */
	public static Class<?> asClass(final Resource resource) throws ClassNotFoundException
	{
		return resource != null ? Classes.asClass(resource.getURI()) : null; //if a resource was given, see if its URI represents a Java class
	}

	/**
	 * Determines the date or date time represented by the given resource.
	 * @param resource The resource which is expected to represent a date or date time, or <code>null</code>.
	 * @return The date or date time represented by the given resource, or <code>null</code> if the resource does not represent a date or date time.
	 * @exception IllegalArgumentException if the given resource represents a date or date time that does not have the correct syntax.
	 * @see #asAbstractDateTime(URI)
	 */
	public static AbstractISODateTime asAbstractDateTime(final Resource resource)
	{
		return resource != null ? asAbstractDateTime(resource.getURI()) : null; //if a resource was given, see if its URI represents a date or date time
	}

	/**
	 * Determines the date or date time represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a date or date time, or <code>null</code>.
	 * @return The date or date time represented by the given URI, or <code>null</code> if the URI does not represent a date or date time.
	 * @exception IllegalArgumentException if the given URI represents a date or date time that does not have the correct syntax.
	 * @see #DATE_CLASS_URI
	 * @see #DATE_NAMESPACE_URI
	 * @see #DATE_TIME_CLASS_URI
	 * @see #DATE_TIME_NAMESPACE_URI
	 */
	public static AbstractISODateTime asAbstractDateTime(final URI resourceURI)
	{
		if(resourceURI != null) //if there is a resource URI
		{
			final URI namespaceURI = getNamespaceURI(resourceURI); //get the namespace URI of the resource URI
			if(DATE_NAMESPACE_URI.equals(namespaceURI)) //if a date URI was given
			{
				return ISODate.valueOf(getInlineLexicalForm(resourceURI)); //create a date from the value TODO use specific valueOf() version that allows configuration for URF syntax
			}
			else if(DATE_TIME_NAMESPACE_URI.equals(namespaceURI)) //if a date time URI was given
			{
				return ISODateTime.valueOf(getInlineLexicalForm(resourceURI)); //create a date time from the value TODO use specific valueOf() version that allows configuration for URF syntax
			}
		}
		return null; //no abstract date time could be found
	}

	/**
	 * Determines the date represented by the given resource.
	 * @param resource The resource which is expected to represent a date, or <code>null</code>.
	 * @return The date represented by the given resource, or <code>null</code> if the resource does not represent a date.
	 * @exception IllegalArgumentException if the given resource represents a date that does not have the correct syntax.
	 * @see #asDate(URI)
	 */
	public static ISODate asDate(final Resource resource)
	{
		return resource != null ? asDate(resource.getURI()) : null; //if a resource was given, see if its URI represents a date
	}

	/**
	 * Determines the date represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a date, or <code>null</code>.
	 * @return The date represented by the given URI, or <code>null</code> if the URI does not represent a date.
	 * @exception IllegalArgumentException if the given URI represents a date that does not have the correct syntax.
	 * @see #DATE_CLASS_URI
	 * @see #DATE_NAMESPACE_URI
	 */
	public static ISODate asDate(final URI resourceURI)
	{
		if(resourceURI != null && DATE_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a date URI was given
		{
			return ISODate.valueOf(getInlineLexicalForm(resourceURI)); //create a date from the value TODO use specific valueOf() version that allows configuration for URF syntax
		}
		return null; //no date could be found
	}

	/**
	 * Determines the date time represented by the given resource.
	 * @param resource The resource which is expected to represent a date time, or <code>null</code>.
	 * @return The date time represented by the given resource, or <code>null</code> if the resource does not represent a date time.
	 * @exception IllegalArgumentException if the given resource represents a date time that does not have the correct syntax.
	 * @see #asDateTime(URI)
	 */
	public static ISODateTime asDateTime(final Resource resource)
	{
		return resource != null ? asDateTime(resource.getURI()) : null; //if a resource was given, see if its URI represents a date time
	}

	/**
	 * Determines the date time represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a date time, or <code>null</code>.
	 * @return The date time represented by the given URI, or <code>null</code> if the URI does not represent a date time.
	 * @exception IllegalArgumentException if the given URI represents a date time that does not have the correct syntax.
	 * @see #DATE_TIME_CLASS_URI
	 * @see #DATE_TIME_NAMESPACE_URI
	 */
	public static ISODateTime asDateTime(final URI resourceURI)
	{
		if(resourceURI != null && DATE_TIME_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a date time URI was given
		{
			return ISODateTime.valueOf(getInlineLexicalForm(resourceURI)); //create a date time from the value TODO use specific valueOf() version that allows configuration for URF syntax
		}
		return null; //no pattern could be found
	}

	/**
	 * Determines the number represented by the given resource.
	 * @param resource The resource which is expected to represent a number, or <code>null</code>.
	 * @return The number represented by the given resource, or <code>null</code> if the resource does not represent a number.
	 * @exception IllegalArgumentException if the given resource represents a number that does not have the correct syntax.
	 * @see #asNumber(URI)
	 */
	public static Number asNumber(final Resource resource)
	{
		return resource != null ? asNumber(resource.getURI()) : null; //if a resource was given, see if its URI represents a URI
	}

	/**
	 * Determines the number represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a number, or <code>null</code>.
	 * @return The number represented by the given URI, or <code>null</code> if the URI does not represent a number.
	 * @exception IllegalArgumentException if the given URI represents a number that does not have the correct syntax.
	 * @see #INTEGER_CLASS_URI
	 * @see #INTEGER_NAMESPACE_URI
	 * @see #ORDINAL_CLASS_URI
	 * @see #ORDINAL_NAMESPACE_URI
	 * @see #RATIONAL_CLASS_URI
	 * @see #RATIONAL_NAMESPACE_URI
	 */
	public static Number asNumber(final URI resourceURI)
	{
		if(resourceURI != null) //if a URI was given
		{
			final URI namespaceURI = getNamespaceURI(resourceURI); //get the namespace of the URI
			if(INTEGER_NAMESPACE_URI.equals(namespaceURI)) //if this is an integer
			{
				return Long.valueOf(Long.parseLong(getInlineLexicalForm(resourceURI))); //parse a long from the value
			}
			else if(ORDINAL_NAMESPACE_URI.equals(namespaceURI)) //if this is an ordinal
			{
				return Long.valueOf(Long.parseLong(getInlineLexicalForm(resourceURI))); //parse a long from the value
			}
			else if(RATIONAL_NAMESPACE_URI.equals(namespaceURI)) //if this is a rational
			{
				return Double.valueOf(Double.parseDouble(getInlineLexicalForm(resourceURI))); //parse a double from the value
			}
		}
		return null; //no number could be found
	}

	/**
	 * Determines the integers represented by the resources returned from the given resource iterator. Non-integer resources will be ignored.
	 * @param resources The resources which are expected to represent integers.
	 * @return The integers represented by the resources returned by the given iterable.
	 * @exception NullPointerException if the given resources is <code>null</code>.
	 * @exception IllegalArgumentException if one of the resources represents an integer that does not have the correct syntax.
	 * @see #asInteger(Resource)
	 */
	public static long[] asIntegers(final Iterable<? extends Resource> resources)
	{
		final List<Long> list = new ArrayList<Long>(); //create a list in which to store the iterator contents
		for(final Resource resource : resources) //for each resource
		{
			final Long integer = asInteger(resource); //get this resource as an integer
			if(integer != null) //if this is a integer
			{
				list.add(integer); //add this integer to the list
			}
		}
		final int count = list.size(); //find out how many integers there are
		final long[] integers = new long[count]; //create an array to hold the integers
		for(int i = 0; i < count; ++i) //for each integer
		{
			integers[i] = list.get(i).longValue(); //get this integer
		}
		return integers; //return the array
	}

	/**
	 * Determines the integer represented by the given resource.
	 * @param resource The resource which is expected to represent an integer, or <code>null</code>.
	 * @return The integer represented by the given resource, or <code>null</code> if the resource does not represent an integer.
	 * @exception IllegalArgumentException if the given resource represents an integer that does not have the correct syntax.
	 * @see #asInteger(URI)
	 */
	public static Long asInteger(final Resource resource)
	{
		return resource != null ? asInteger(resource.getURI()) : null; //if a resource was given, see if its URI represents a integer
	}

	/**
	 * Determines the integer represented by the given URI.
	 * @param resourceURI The URI which is expected to represent an integer, or <code>null</code>.
	 * @return The integer represented by the given URI, or <code>null</code> if the URI does not represent an integer.
	 * @exception IllegalArgumentException if the given URI represents an integer that does not have the correct syntax.
	 * @see #INTEGER_CLASS_URI
	 * @see #INTEGER_NAMESPACE_URI
	 */
	public static Long asInteger(final URI resourceURI)
	{
		if(resourceURI != null && INTEGER_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if an integer URI was given
		{
			return Long.valueOf(Long.parseLong(getInlineLexicalForm(resourceURI))); //parse a long from the value
		}
		return null; //no integer could be found
	}

	/**
	 * Determines the language represented by the given resource.
	 * @param resource The resource which is expected to represent a language, or <code>null</code>.
	 * @return The language represented by the given resource, or <code>null</code> if the resource does not represent a language.
	 * @exception IllegalArgumentException if the given resource represents a language that does not have the correct syntax.
	 * @see #asLanguage(URI)
	 */
	public static Locale asLanguage(final Resource resource)
	{
		return resource != null ? asLanguage(resource.getURI()) : null; //if a resource was given, see if its URI represents a language
	}

	/**
	 * Determines the language represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a language, or <code>null</code>.
	 * @return The language represented by the given URI, or <code>null</code> if the URI does not represent a language.
	 * @exception IllegalArgumentException if the given URI represents a language that does not have the correct syntax.
	 * @see #LANGUAGE_CLASS_URI
	 * @see #LANGUAGE_NAMESPACE_URI
	 */
	public static Locale asLanguage(final URI resourceURI)
	{
		if(resourceURI != null && LANGUAGE_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a language URI was given
		{
			return createLocale(getInlineLexicalForm(resourceURI)); //create a locale from the value
		}
		return null; //no language could be found
	}

	/**
	 * Determines the ordinal represented by the given URI.
	 * @param resourceURI The URI which is expected to represent an ordinal, or <code>null</code>.
	 * @return The ordinal represented by the given URI, or <code>null</code> if the URI does not represent an ordinal.
	 * @exception IllegalArgumentException if the given URI represents an ordinal that does not have the correct syntax.
	 * @see #ORDINAL_CLASS_URI
	 * @see #ORDINAL_NAMESPACE_URI
	 */
	public static Long asOrdinal(final URI resourceURI)
	{
		if(resourceURI != null && ORDINAL_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if an ordinal URI was given
		{
			return Long.valueOf(Long.parseLong(getInlineLexicalForm(resourceURI))); //parse a long from the value
		}
		return null; //no ordinal could be found
	}

	/**
	 * Determines the pattern represented by the given resource. A URI represents a pattern if it is a valid regular expression inline URI.
	 * @param resource The resource which is expected to represent a pattern, or <code>null</code>.
	 * @return The pattern represented by the given resource, or <code>null</code> if the resource does not represent a pattern.
	 * @exception IllegalArgumentException if the given resource represents a pattern that does not have the correct syntax.
	 * @see #asPattern(URI)
	 */
	public static Pattern asPattern(final Resource resource)
	{
		return resource != null ? asPattern(resource.getURI()) : null; //if a resource was given, see if its URI represents a pattern
	}

	/**
	 * Determines the pattern represented by the given URI. A resource represents a pattern if it has a valid regular expression inline URI.
	 * @param resourceURI The URI which is expected to represent a pattern, or <code>null</code>.
	 * @return The pattern represented by the given URI, or <code>null</code> if the URI does not represent a pattern.
	 * @exception IllegalArgumentException if the given URI represents a pattern that does not have the correct syntax.
	 * @see #REGULAR_EXPRESSION_CLASS_URI
	 * @see #REGULAR_EXPRESSION_NAMESPACE_URI
	 */
	public static Pattern asPattern(final URI resourceURI)
	{
		if(resourceURI != null && REGULAR_EXPRESSION_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a regular expression URI was given
		{
			try
			{
				return Pattern.compile(getInlineLexicalForm(resourceURI)); //create a pattern from the value
			}
			catch(final PatternSyntaxException patternSyntaxException)
			{
				throw new IllegalArgumentException(patternSyntaxException);
			}
		}
		return null; //no pattern could be found
	}

	/**
	 * Determines the rational represented by the given resource.
	 * @param resource The resource which is expected to represent a rational, or <code>null</code>.
	 * @return The rational represented by the given resource, or <code>null</code> if the resource does not represent a rational.
	 * @exception IllegalArgumentException if the given resource represents a rational that does not have the correct syntax.
	 * @see #asRational(URI)
	 */
	public static Double asRational(final Resource resource)
	{
		return resource != null ? asRational(resource.getURI()) : null; //if a resource was given, see if its URI represents a rational
	}

	/**
	 * Determines the rational represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a rational, or <code>null</code>.
	 * @return The rational represented by the given URI, or <code>null</code> if the URI does not represent a rational.
	 * @exception IllegalArgumentException if the given URI represents a rational that does not have the correct syntax.
	 * @see #RATIONAL_CLASS_URI
	 * @see #RATIONAL_NAMESPACE_URI
	 */
	public static Double asRational(final URI resourceURI)
	{
		if(resourceURI != null && RATIONAL_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a rational URI was given
		{
			return Double.parseDouble(getInlineLexicalForm(resourceURI)); //parse a double from the value
		}
		return null; //no rational could be found
	}

	/**
	 * Determines the strings represented by the resources returned from the given resource iterator. Non-string resources will be ignored.
	 * @param resources The resources which are expected to represent strings.
	 * @return The strings represented by the resources returned by the given iterable.
	 * @exception NullPointerException if the given resources is <code>null</code>.
	 * @exception IllegalArgumentException if one of the resources represents a string that does not have the correct syntax.
	 * @see #asString(Resource)
	 */
	public static String[] asStrings(final Iterable<? extends Resource> resources)
	{
		final List<String> list = new ArrayList<String>(); //create a list in which to store the iterator contents
		for(final Resource resource : resources) //for each resource
		{
			final String string = asString(resource); //get this resource as a string
			if(string != null) //if this is a string
			{
				list.add(string); //add this string to the list
			}
		}
		return list.toArray(new String[list.size()]); //return the list as an array
	}

	/**
	 * Determines the string represented by the given resource.
	 * @param resource The resource which is expected to represent a string, or <code>null</code>.
	 * @return The string represented by the given resource, or <code>null</code> if the resource does not represent a string.
	 * @exception IllegalArgumentException if the given resource represents a string that does not have the correct syntax.
	 * @see #asString(URI)
	 */
	public static String asString(final Resource resource)
	{
		return resource != null ? asString(resource.getURI()) : null; //if a resource was given, see if its URI represents a string
	}

	/**
	 * Determines the string represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a string, or <code>null</code>.
	 * @return The string represented by the given URI, or <code>null</code> if the URI does not represent a string.
	 * @exception IllegalArgumentException if the given URI represents a string that does not have the correct syntax.
	 * @see #STRING_CLASS_URI
	 * @see #STRING_NAMESPACE_URI
	 */
	public static String asString(final URI resourceURI)
	{
		if(resourceURI != null && STRING_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a string URI was given
		{
			return getInlineLexicalForm(resourceURI); //return the value, which is the string value
		}
		return null; //no string could be found
	}

	/**
	 * Determines the URIs represented by the resources returned from the given resource iterator. Non-URI resources will be ignored.
	 * @param resources The resources which are expected to represent URIs.
	 * @return The URIs represented by the resources returned by the given iterable.
	 * @exception NullPointerException if the given resources is <code>null</code>.
	 * @exception IllegalArgumentException if one of the resources represents a URI that does not have the correct syntax.
	 * @see #asURI(Resource)
	 */
	public static URI[] asURIs(final Iterable<? extends Resource> resources)
	{
		final List<URI> list = new ArrayList<URI>(); //create a list in which to store the iterator contents
		for(final Resource resource : resources) //for each resource
		{
			final URI uri = asURI(resource); //get this resource as a URI
			if(uri != null) //if this is a URI
			{
				list.add(uri); //add this URI to the list
			}
		}
		return list.toArray(new URI[list.size()]); //return the list as an array
	}

	/**
	 * Determines the URI represented by the given resource.
	 * @param resource The resource which is expected to represent a URI, or <code>null</code>.
	 * @return The URI represented by the given resource, or <code>null</code> if the resource does not represent a URI.
	 * @exception IllegalArgumentException if the given resource represents a URI that does not have the correct syntax.
	 * @see #asURI(URI)
	 */
	public static URI asURI(final Resource resource)
	{
		return resource != null ? asURI(resource.getURI()) : null; //if a resource was given, see if its URI represents a URI
	}

	/**
	 * Determines the URI represented by the given URI.
	 * @param resourceURI The URI which is expected to represent a URI, or <code>null</code>.
	 * @return The URI represented by the given URI, or <code>null</code> if the URI does not represent a URI.
	 * @exception IllegalArgumentException if the given URI represents a URI that does not have the correct syntax.
	 * @see #URI_CLASS_URI
	 * @see #URI_NAMESPACE_URI
	 */
	public static URI asURI(final URI resourceURI)
	{
		if(resourceURI != null && URI_NAMESPACE_URI.equals(getNamespaceURI(resourceURI))) //if a URI URI was given
		{
			return URI.create(getInlineLexicalForm(resourceURI)); //create a URI from the value
		}
		return null; //no URI could be found
	}

	/**
	 * Determines the URI path represented by the given resource. A resource represents a URI path if it is has a URI with the {@value URIs#PATH_SCHEME} scheme.
	 * @param resource The resource which is expected to represent a URI path, or <code>null</code>.
	 * @return The URI path represented by the given resource, or <code>null</code> if the resource does not represent a URI path.
	 * @exception IllegalArgumentException if the given resource represents a URI path that does not have the correct syntax.
	 * @see #asURIPath(URI)
	 */
	public static URIPath asURIPath(final Resource resource)
	{
		return resource != null ? asURIPath(resource.getURI()) : null; //if a resource was given, see if its URI represents a URI path
	}

	/**
	 * Determines the URI path represented by the given URI. A URI represents a URI path if it is has the {@value URIs#PATH_SCHEME} scheme.
	 * @param resourceURI The URI which is expected to represent a URI path, or <code>null</code>.
	 * @return The URI path represented by the given URI, or <code>null</code> if the URI does not represent a URI path.
	 * @exception IllegalArgumentException if the given URI represents a URI path that does not have the correct syntax.
	 * @see URIs#PATH_SCHEME
	 */
	public static URIPath asURIPath(final URI resourceURI)
	{
		if(resourceURI != null && PATH_SCHEME.equals(resourceURI.getScheme())) //if a URI path URI was given
		{
			return getPathURIPath(resourceURI); //return a URIPath from the resource URI path
		}
		return null; //no URI could be found
	}

	/**
	 * Converts an URF data model to a string for debugging purposes.
	 * @param urf The URF data model to represent as a string.
	 * @return A string representation of the URF data model.
	 */
	public static String toString(final URF urf)
	{
		try
		{
			return URFTURFGenerator.forPrint(false).generateResources(new StringBuilder(), urf).toString(); //generate terse TURF from the URF
		}
		catch(final IOException ioException) //there should never be a problem writing to a string builder
		{
			throw unexpected(ioException);
		}
	}

	/**
	 * Converts an URF resource to a string for debugging purposes.
	 * @param resource The URF resource to represent as a string.
	 * @return A string representation of the URF resource.
	 */
	public static String toString(final URFResource resource)
	{
		try
		{
			return URFTURFGenerator.forPrint(false).generateResources(new StringBuilder(), false, resource).toString(); //generate terse TURF from the resource
		}
		catch(final IOException ioException) //there should never be a problem writing to a string builder
		{
			throw unexpected(ioException);
		}
	}

	/** Comparator for sorting resources in by their property counts, from few to many. */
	public final static Comparator<URFResource> RESOURCE_PROPERTY_COUNT_COMPARATOR = new Comparator<URFResource>()
	{
		/**
		 * Compares its two arguments for order. Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater
		 * than the second. This implementation compares by property count.
		 * @param resource1 The first object to be compared.
		 * @param resource2 The second object to be compared.
		 * @return A negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second.
		 */
		public int compare(final URFResource resource1, final URFResource resource2)
		{
			int result = Longs.compare(resource1.getPropertyValueCount(), resource2.getPropertyValueCount()); //compare property counts
			if(result == 0) //if property counts are the same
			{
				result = Longs.compare(resource1.getCreationOrder(), resource2.getCreationOrder()); //compare creation order
			}
			return result; //return the result of the comparison
		}
	};

	/** A map of resource factories, keyed to namespace URIs. */
	private final Map<URI, URFResourceFactory> namespaceURIResourceFactoryMap = new HashMap<URI, URFResourceFactory>();

	/**
	 * Registers a resource factory to be used to create resources with a type from the specified namespace. If a resource factory is already registered for this
	 * namespace, it will be replaced.
	 * @param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	 * @param factory The resource factory that will be used to create resources of types from this namespace.
	 */
	public void registerResourceFactory(final URI typeNamespaceURI, final URFResourceFactory factory)
	{
		namespaceURIResourceFactoryMap.put(typeNamespaceURI, factory);
	}

	/**
	 * Removes the resource factory being used to create resources with a type from the specified namespace. If there is no resource factory registered for this
	 * namespace, no action will be taken.
	 * @param typeNamespaceURI The namespace of the resource type for which this factory should be used to create objects.
	 */
	public void unregisterResourceFactory(final URI typeNamespaceURI)
	{
		namespaceURIResourceFactoryMap.remove(typeNamespaceURI);
	}

	/**
	 * Retrieves a resource factory to be used for creating resources with a type from the specified namespace URI.
	 * @param typeNamespaceURI The namespace of the type for which a resource factory should be returned.
	 * @return The factory registered for this type namespace, or <code>null</code> if there is no factory registered for this type namespace.
	 */
	protected URFResourceFactory getResourceFactory(final URI typeNamespaceURI)
	{
		return namespaceURIResourceFactoryMap.get(typeNamespaceURI); //return any factory registered for this namespace
	}

	/** The set of all resources, identified and anonymous, using identity rather than equality for equivalence. */
	private final IdentityHashSet<URFResource> resourceSet = new IdentityHashSet<URFResource>();

	/** The map of all identified resources, keyed to resource URIs. */
	private final Map<URI, URFResource> resourceMap = new HashMap<URI, URFResource>();

	/**
	 * @return A read-only set of the URIs of all named resources in this data model.
	 * @see #getResourceURIReferences()
	 */
	public Set<URI> getResourceURIs()
	{
		return unmodifiableSet(resourceMap.keySet()); //return the set of keys to the resource map
	}

	/**
	 * @return A read-only set of the URIs of all named resources in this data model, as well as all the URIs of resource properties.
	 * @see #getResourceURIs()
	 */
	public Set<URI> getResourceURIReferences()
	{
		final Set<URI> resourceURIReferences = new HashSet<URI>(); //get the set of resource URIs
		for(final URFResource resource : resourceMap.values()) //look at each named resource
		{
			assert resource.getURI() != null : "Resource with no URI found in the named resource map.";
			resourceURIReferences.add(resource.getURI()); //add the URI of this resource
			addAll(resourceURIReferences, resource.getPropertyURIs()); //add all the URIs of the resource's properties
		}
		return unmodifiableSet(resourceURIReferences); //return the set of all URI references
	}

	/**
	 * Adds a resource to the data model. All property value resources are recursively added to the model.
	 * @param resource The resource to add.
	 * @exception NullPointerException if the given resource is <code>null</code>.
	 */
	public void addResource(final URFResource resource)
	{
		addResource(resource, new IdentityHashSet<URFResource>()); //add this resource, using an identity hash map to determine which resources have been added
	}

	/**
	 * Adds a resource to the data model if it hasn't been added already. All property value resources are recursively added to the model. If the resource
	 * @param resource The resource to add.
	 * @param addedResources The set of resources added to the set to prevent infinite recursion in self-connected graphs.
	 * @exception NullPointerException if the given resource and or added resources set is <code>null</code>.
	 * @see #addPropertyValues(URFScope, Set)
	 */
	protected void addResource(final URFResource resource, final Set<URFResource> addedResources)
	{
		if(!addedResources.contains(resource)) //if we haven't already added this resource
		{
			resourceSet.add(resource); //add the resource to our set
			final URI resourceURI = resource.getURI(); //get the resource's URI, if any
			if(resourceURI != null) //if this is not an anonymous resource
				resourceMap.put(resourceURI, resource); //store the resource in the map
			addedResources.add(resource); //indicate that we added this resource
			addPropertyValues(resource, addedResources); //add all property values from the resource recursively
		}
	}

	/**
	 * Adds all the property values of a particular scope to the data model if they hasn't been added already. The property values of each property value's scope
	 * are also recursively added to the model.
	 * @param scope The scope the property values of which to add.
	 * @param addedResources The set of resources added to the set to prevent infinite recursion in self-connected graphs.
	 * @exception NullPointerException if the given resource and or added resources set is <code>null</code>.
	 */
	protected void addPropertyValues(final URFScope scope, final Set<URFResource> addedResources)
	{
		for(final URFProperty property : scope.getProperties()) //for each property in the scope
		{
			addResource(property.getValue(), addedResources); //add this property value resource
			addPropertyValues(property.getScope(), addedResources); //add all property values from the property-value's scope recursively
		}
	}

	/**
	 * Determines whether a resource exists in the data model.
	 * @param resourceURI The URI of the resource for which to check.
	 * @return <code>true</code> if a resource with the given URI exists in the data model.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 */
	public boolean containsResource(final URI resourceURI)
	{
		return resourceMap.containsKey(checkInstance(resourceURI, "Resource URI cannot be null.")); //determine whether the resource exists
	}

	/**
	 * Retrieves an identified resource from the data model using its URI.
	 * @param resourceURI The URI of the resource to retrieve.
	 * @return The resource, or <code>null</code> if no matching resource was found.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 */
	public URFResource getResource(final URI resourceURI)
	{
		return resourceMap.get(checkInstance(resourceURI, "Resource URI cannot be null.")); //retrieve the resource
	}

	/** @return Whether this data model contains resources. */
	public boolean hasResources()
	{
		return !resourceSet.isEmpty(); //whether the resource set is not empty
	}

	/** @return The number of resources in this data model. */
	public int getResourceCount()
	{
		return resourceSet.size(); //return the size of the resource set
	}

	/**
	 * Returns a read-only iterable of all resources in the data model.
	 * @return A read-only iterable of resources in the data model.
	 */
	public Iterable<URFResource> getResources()
	{
		return unmodifiableSet(resourceSet); //return an unmodifiable iterable to the set of all resources
	}

	/**
	 * Returns the root resouces in this data model; that is, the resources which have no references to them.
	 * @return A read-only iterable of the root resources in this data model.
	 */
	public Iterable<URFResource> getRootResources()
	{
		final ReferenceSummary referenceSummary = getReferenceSummary(); //get a summary all references to each resource
		final Iterator<Map.Entry<URFResource, Set<URFScope>>> resourceScopeReferencesEntryIterator = referenceSummary.objectReferenceMap.entrySet().iterator(); //get an iterator to all the entries of the reference map
		while(resourceScopeReferencesEntryIterator.hasNext()) //while there are more entries
		{
			final Map.Entry<URFResource, Set<URFScope>> resourceScopeReferencesEntry = resourceScopeReferencesEntryIterator.next(); //get the next entry
			if(!resourceScopeReferencesEntry.getValue().isEmpty()) //if there are references for this resource
			{
				resourceScopeReferencesEntryIterator.remove(); //remove this non-root resource from the reference map
			}
		}
		return unmodifiableSet(referenceSummary.objectReferenceMap.keySet()); //return an unmodifiable iterable to the set of all remaining resources in the reference map: the root resource which have no references
	}

	/**
	 * Retrieves the first encountered resource in the data model that is of the requested type. If there are more than one resource with the requested type, it
	 * is undefined which one will be returned.
	 * @param typeURI The URI of the type requested.
	 * @return A resource of the requested type, or <code>null</code> if there are no resourcees with the specified type.
	 * @exception NullPointerException if the given type URI is <code>null</code>.
	 */
	public URFResource getResourceByTypeURI(final URI typeURI)
	{
		for(final URFResource resource : getResources()) //for each resource in this data model
		{
			if(resource.hasTypeURI(typeURI)) //if this resource is of the requested type
			{
				return resource; //return the resource
			}
		}
		return null; //indicate that no resources of the given type could be found
	}

	/**
	 * Retrieves the resources in the data model that are of the requested type.
	 * @param typeURI The URI of the type requested.
	 * @return A read-only iterable of resources that are of the requested type.
	 * @exception NullPointerException if the given type URI is <code>null</code>.
	 */
	public Iterable<URFResource> getResourcesByTypeURI(final URI typeURI)
	{
		final List<URFResource> resourceList = new ArrayList<URFResource>(); //create a list in which to store the resources; because we iterate a set, the gathered resources are ensured not to be duplicated, so storing them in a list is faster then storing them in another set
		for(final URFResource resource : getResources()) //for each resource in this data model
		{
			if(resource.hasTypeURI(typeURI)) //if this resource is of the requested type
			{
				resourceList.add(resource); //add this resource to our list
			}
		}
		return unmodifiableList(resourceList); //make the list read-only and return it
	}

	/** Default constructor. */
	public URF()
	{
		registerResourceFactory(NAMESPACE_URI, DEFAULT_URF_RESOURCE_FACTORY); //register the default URF resource factory with the URF namespace
		registerResourceFactory(Content.CONTENT_NAMESPACE_URI, Content.DEFAULT_CONTENT_RESOURCE_FACTORY); //register the default content resource factory with the content namespace
		registerResourceFactory(Select.SELECT_NAMESPACE_URI, new JavaURFResourceFactory(Select.class.getPackage())); //instantiate select classes for select resources
	}

	/**
	 * Retreives a resource from the data model based upon a URI. If no such resource exists, a resource will be created and added to the data model. If the given
	 * resource URI is in an inline namespace, its inline type will be used.
	 * @param resourceURI The URI of the resource to retrieve, or <code>null</code> if the resource should have no URI.
	 * @return A resource with the given URI.
	 * @exception NullPointerException if the given URI is <code>null</code>.
	 * @exception IllegalArgumentException if a resource could not be created based upon the given criteria.
	 */
	public URFResource locateResource(final URI resourceURI)
	{
		final URI[] typeURIs = isInlineURI(resourceURI) ? new URI[] { getInlineTypeURI(resourceURI) } : NO_URIS; //get the inline type, if we can
		return locateResource(resourceURI, typeURIs); //locate a resource with whatever type we determined, if any
	}

	/**
	 * Retrieves a resource from the data model based upon the URI of the resource and optional type URIs. If no such resource exists, or no resource URI was
	 * given, a resource will be created and added to the data model. The given type URIs, if any, will be used to locate a resource factory to create the
	 * resource, and that type URI may be added as a type property. If the resource already exists, no checks are performed to ensure that the existing resource
	 * is of the requested type.
	 * @param resourceURI The URI of the resource to retrieve, or <code>null</code> if the resource should have no URI.
	 * @param typeURIs The URIs of the known types.
	 * @exception NullPointerException if any of the given type URIs is <code>null</code>.
	 * @exception IllegalArgumentException if a resource could not be created based upon the given criteria.
	 * @return A resource with the given URI.
	 */
	public URFResource locateResource(final URI resourceURI, final URI... typeURIs)
	{
		URFResource resource = resourceURI != null ? getResource(resourceURI) : null; //retrieve a resource from the data model if a resource URI was given
		if(resource == null) //if no such resource exists
		{
			resource = createResource(resourceURI, typeURIs); //create a new resource of the given types from the given URI and store the resource in the data model
		}
		return resource; //return the resource we either found or created
	}

	/**
	 * Creates an anonymous resource and stores it in this data model.
	 * @return An anonymous resource.
	 */
	public URFResource createResource()
	{
		return createResource(null); //create a resource with no URI
	}

	/**
	 * Creates a resource with the given URI and type URIs. The given type URIs will be used to attempt to find a resource factory to create the resource. The
	 * returned resource will have no properties, which implies that no type will be indicated for the resource. The created resource will be stored in this data
	 * model.
	 * @param resourceURI The URI of the resource to create, or <code>null</code> if the created resource created have no URI.
	 * @param typeURIs The URIs of the known types.
	 * @return The resource created with this URI, with the given type added if a type was given.
	 * @exception NullPointerException if any of the given type URIs is <code>null</code>.
	 * @exception IllegalArgumentException if a resource could not be created based upon the given criteria.
	 * @see #DEFAULT_RESOURCE_FACTORY
	 * @see URFResourceFactory#createResource(URI, URI)
	 */
	public URFResource createResource(final URI resourceURI, final URI... typeURIs)
	{
		URFResourceFactory selectedResourceFactory = DEFAULT_RESOURCE_FACTORY; //we'll try to find a matching resource factory; if we can't, we'll use the default resource factory
		URI selectedTypeURI = null; //we'll remember the type URI used for finding the resource factory		
		for(final URI typeURI : typeURIs) //for each type URI
		{
			final URI typeNamespaceURI = getNamespaceURI(typeURI); //try to get the namespace of this type
			if(typeNamespaceURI != null) //if this type URI is in a namespace
			{
				final URFResourceFactory resourceFactory = getResourceFactory(typeNamespaceURI); //get a resource factory for this namespace
				if(resourceFactory != null) //if we have a resource factory for this namespace
				{
					selectedResourceFactory = resourceFactory; //note the resource factory
					selectedTypeURI = typeURI; //note the type URI
				}
			}
		}
		final URFResource resource = selectedResourceFactory.createResource(resourceURI, selectedTypeURI); //create a resource from the resource factory, using the selected type URI, if any
		resource.removeProperties(); //remove any properties that the resource factory may have added
		addResource(resource); //store the resource in the data model
		return resource; //return the resource we created
	}

	/**
	 * Returns the version of the resource.
	 * @param resource The resource the property of which should be located.
	 * @return The string value of the property, or <code>null</code> if there is no such property or the property value is not a string.
	 * @see #VERSION_PROPERTY_URI
	 */
	public static String getVersion(final URFResource resource)
	{
		return asString(resource.getPropertyValue(VERSION_PROPERTY_URI));
	}

	/**
	 * Sets the version of the resource.
	 * @param resource The resource of which the property should be set.
	 * @param value The property value to set.
	 * @see #VERSION_PROPERTY_URI
	 */
	public static void setVersion(final URFResource resource, final String value)
	{
		resource.setPropertyValue(VERSION_PROPERTY_URI, value);
	}

	/**
	 * The encapsulation of a summary of reference information about an URF data model.
	 * @author Garret Wilson
	 */
	public static class ReferenceSummary
	{

		/** The map of resources and the sets of scopes that reference them as property objects. */
		public final CollectionMap<URFResource, URFScope, Set<URFScope>> objectReferenceMap = new IdentityHashSetMap<URFResource, URFScope>(
				new HashMap<URFResource, Set<URFScope>>()); //create a new map in which to store reference sets; don't make the use identity for the map, or we wouldn't recognize references to multiple instances of resources with the same URI

		/** The map of property URIs and the sets of scopes that reference them. */
		public final CollectionMap<URI, URFScope, Set<URFScope>> propertyURIReferenceMap = new IdentityHashSetMap<URI, URFScope>(new HashMap<URI, Set<URFScope>>()); //create a new map in which to store reference sets; don't make the use identity for the map, or we wouldn't recognize references to multiple instances of resources with the same URI

		/** The map of the number of times each each property URI is referenced as a property URI, including multiple times by the same subject. */
		public final Map<URI, Integer> propertyURIReferenceCountMap = new HashMap<URI, Integer>();

	}

	/**
	 * Looks at all the scopes in the URF data model and recursively gathers which scopes reference which other resources. Circular references are correctly
	 * handled. The returned sets of scopes use identity equivalence.
	 * @return A summary of scope references to resources and property URIs.
	 */
	public ReferenceSummary getReferenceSummary()
	{
		final ReferenceSummary referenceSummary = new ReferenceSummary(); //create a new reference summary to populate
		final Set<URFScope> referrerScopeSet = new IdentityHashSet<URFScope>(); //create a set of referring scopes to prevent circular references
		for(final URFResource resource : getResources()) //for each resource in this data model
		{
			getReferenceSummary(resource, referenceSummary, referrerScopeSet); //gather all references to this resource
		}
		return referenceSummary; //return the summary we populated
	}

	/**
	 * Looks at the scope and all its properties and recursively gathers which scopes reference which other resources. Circular references are correctly handled.
	 * @param scope The scope for which references should be gathered for the scope and all child scopes and resources that are property values of this resource's
	 *          properties, and so on.
	 * @param referenceSummary A summary of scope references to resources and property URIs.
	 * @param referrerScopeSet The set of referrers the properties and scopes of which have been traversed, the checking of which prevents circular reference
	 *          problems.
	 * @return The summary of scope references to resources and property URIs.
	 */
	protected ReferenceSummary getReferenceSummary(final URFScope scope, final ReferenceSummary referenceSummary, final Set<URFScope> referrerScopeSet)
	{
		if(!referrerScopeSet.contains(scope)) //if we haven't checked this scope before
		{
			referrerScopeSet.add(scope); //show that we've now checked this scope (in case one of the scope's own properties, subproperties, or child scopes reference this resource)
			if(scope instanceof URFResource) //if the scope that we're checking is a resource
			{
				referenceSummary.objectReferenceMap.getCollection((URFResource)scope); //make sure that there is a reference collection for the resources; otherwise, if there were no references to this resource, it would not appear in the reference map				
			}
			for(final URFProperty property : scope.getProperties()) //for each property in the scope
			{
				final URI propertyURI = property.getPropertyURI(); //get the property URI
				referenceSummary.propertyURIReferenceMap.addItem(propertyURI, scope); //note that this scope references this property
				final Integer oldPropertyURIReferenceCount = referenceSummary.propertyURIReferenceCountMap.get(propertyURI); //see how many times this property has already been referenced
				referenceSummary.propertyURIReferenceCountMap.put(propertyURI,
						Integer.valueOf(oldPropertyURIReferenceCount != null ? oldPropertyURIReferenceCount.intValue() + 1 : 1)); //increment the count of property URI references, starting with one for the first reference
				final URFResource value = property.getValue(); //get the property value
				referenceSummary.objectReferenceMap.addItem(value, scope); //note that this scope references this value
				getReferenceSummary(value, referenceSummary, referrerScopeSet); //get all references that the value makes
				getReferenceSummary(property.getScope(), referenceSummary, referrerScopeSet); //get all references that the scope makes
			}
		}
		return referenceSummary; //return the reference summary that was provided, which now holds sets of references to resources
	}

	/**
	 * Returns a string representation of this data model. This implementation returns {@value #NAME}.
	 * @return A string representation of this data model.
	 */
	public String toString()
	{
		return NAME; //return "URF"
	}

	/** The legacy, discontinued namespace URIs used during development of URF. */
	private final static Set<URI> LEGACY_NAMESPACE_URIS;

	static
	{
		final Set<URI> legacyNamespaceURIs = new HashSet<URI>();
		legacyNamespaceURIs.add(URI.create("http://urf.name/urf"));
		legacyNamespaceURIs.add(URI.create("http://urf.name/content"));
		legacyNamespaceURIs.add(URI.create("http://urf.name/default"));
		legacyNamespaceURIs.add(URI.create("http://urf.name/select"));
		legacyNamespaceURIs.add(URI.create("http://urf.name/vcard"));
		legacyNamespaceURIs.add(URI.create("http://urf.name/xml"));
		legacyNamespaceURIs.add(URI.create("http://example.com/example"));
		legacyNamespaceURIs.add(URI.create("http://globalmentor.com/marmot/resource"));
		legacyNamespaceURIs.add(URI.create("http://globalmentor.com/marmot/resource/xhtml"));
		legacyNamespaceURIs.add(URI.create("http://globalmentor.com/marmot/security"));
		legacyNamespaceURIs.add(URI.create("http://guiseframework.com/namespaces/resources"));
		legacyNamespaceURIs.add(URI.create("http://guiseframework.com/namespaces/theme"));
		LEGACY_NAMESPACE_URIS = unmodifiableSet(legacyNamespaceURIs);
	}

	/**
	 * Converts a legacy namespace URI to a standard namespace URI. This method converts a non-collection path into a collection path, ending with the
	 * {@value URIs#PATH_SEPARATOR} character, if the legacy namespace is one of those recognized. If the given namespace URI is not a legacy namespace URI, no
	 * action occurs.
	 * @param legacyNamespaceURI The possibly legacy namespace URI to convert.
	 * @return The new namespace URI converted from the given possibly legacy namespace URI.
	 * @see #LEGACY_NAMESPACE_URIS
	 */
	public static URI convertLegacyNamespaceURI(final URI legacyNamespaceURI)
	{
		if(LEGACY_NAMESPACE_URIS.contains(legacyNamespaceURI)) //if this is a legacy URI
		{
			assert legacyNamespaceURI.getRawPath() != null && !isCollectionPath(legacyNamespaceURI.getRawPath());
			return changeRawPath(legacyNamespaceURI, legacyNamespaceURI.getRawPath() + PATH_SEPARATOR); //change the path to a collection path
		}
		return legacyNamespaceURI; //the URI didn't need converting
	}

	/**
	 * Converts a URI with a legacy namespace URI to a URI with a standard namespace URI. This method converts URIs using the legacy namespace delimiter
	 * {@value URIs#FRAGMENT_SEPARATOR} to use the {@value URIs#PATH_SEPARATOR} character if the legacy namespace is one of those recognized. If the given URI
	 * does not have a legacy namespace URI, no action occurs.
	 * @param legacyNamespacedURI The URI with possibly a legacy namespace URI to convert.
	 * @return The new URI converted from the given URI with a possibly legacy namespace URI.
	 * @see #LEGACY_NAMESPACE_URIS
	 */
	public static URI convertLegacyNamespacedURI(final URI legacyNamespacedURI)
	{
		final String rawFragment = legacyNamespacedURI.getRawFragment(); //get the URI fragment, if any
		if(rawFragment != null && legacyNamespacedURI.getRawPath() != null) //if the URI has a fragment and a path
		{
			final URI legacyNamespaceURI = removeFragment(legacyNamespacedURI); //remove the fragment to get the legacy namespace
			if(LEGACY_NAMESPACE_URIS.contains(legacyNamespaceURI)) //if this is a legacy URI
			{
				return resolve(convertLegacyNamespaceURI(legacyNamespaceURI), rawFragment); //convert that namespace and resolve the fragment against it
			}
		}
		return legacyNamespacedURI; //the URI didn't need converting
	}

	/**
	 * The shared resource factory for default resources.
	 * @see DefaultURFResource
	 */
	public final static URFResourceFactory DEFAULT_RESOURCE_FACTORY = new DefaultURFResourceFactory();

	/**
	 * The default resource factory for the URF ontology. This resource factory can create the following types of resource objects for the given types:
	 * <dl>
	 * <dt>{@value #LIST_CLASS_URI}</dt>
	 * <dd>{@link URFListResource}</dd>
	 * <dt>{@value #SET_CLASS_URI}</dt>
	 * <dd>{@link URFSetResource}</dd>
	 * <dt>{@value #MAP_CLASS_URI}</dt>
	 * <dd>{@link URFMapResource}</dd>
	 * <dt>{@value #MAP_ENTRY_CLASS_URI}</dt>
	 * <dd>{@link URFMapEntryResource}</dd>
	 * </dl>
	 */
	public final static DefaultURFResourceFactory DEFAULT_URF_RESOURCE_FACTORY = new DefaultURFResourceFactory()
	{
		/**
		 * Creates a resource with the provided URI based upon the type URI, if any. If a type URI is provided, a corresponding type property value may be added to
		 * the resource before it is returned.
		 * @param resourceURI The URI of the resource to create, or <code>null</code> if the resource created should be anonymous.
		 * @param typeURI The URI of the resource type, or <code>null</code> if the type is not known.
		 * @return The resource created with this URI.
		 * @exception IllegalArgumentException if an inline resource URI was given with a different type URI than the specified type URI.
		 * @see URF#asObject(URI)
		 */
		public URFResource createResource(final URI resourceURI, final URI typeURI)
		{
			if(LIST_CLASS_URI.equals(typeURI)) //if this is a list
			{
				return new URFListResource<URFResource>(resourceURI); //create a new list
			}
			else if(SET_CLASS_URI.equals(typeURI)) //if this is a set
			{
				return new URFSetResource<URFResource>(resourceURI); //create a new set
			}
			else if(MAP_CLASS_URI.equals(typeURI)) //if this is a map
			{
				return new URFMapResource<URFResource, URFResource>(resourceURI); //create a new map
			}
			else if(MAP_ENTRY_CLASS_URI.equals(typeURI)) //if this is a map entry
			{
				return new URFMapEntryResource<URFResource, URFResource>(resourceURI); //create a new map entry
			}
			return super.createResource(resourceURI, typeURI); //if we don't recognize the type, create a default resource
		}
	};

}