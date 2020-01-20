/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

import java.net.URLConnection;
import java.util.*;
import java.util.regex.*;

import javax.annotation.*;

import static java.util.Objects.*;

import static com.globalmentor.collections.Sets.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.SPACE_CHAR;
import static com.globalmentor.java.Characters.QUOTATION_MARK_CHAR;
import static com.globalmentor.text.ABNF.*;
import static com.globalmentor.text.RegularExpressions.*;
import static java.nio.charset.StandardCharsets.*;
import static java.util.Collections.*;

import com.globalmentor.java.*;
import com.globalmentor.model.NameValuePair;
import com.globalmentor.text.*;

/**
 * An encapsulation of an Internet media content type as originally defined in <a href="https://tools.ietf.org/html/rfc2046"><cite>RFC 2046: MIME Part 2: Media
 * Types</cite></a>; and most recently in <a href="https://tools.ietf.org/html/rfc6838"><cite>RFC 6838: Media Type Specifications and Registration
 * Procedures</cite></a>. The full syntax for a content type and its parameters are found in <a href="https://tools.ietf.org/html/rfc2045"><cite>RFC 2045: MIME
 * Part 1: Format of Internet Message Bodies</cite></a>
 * <p>
 * The content type and names of parameters are compared in a case-insensitive manner as per RFC 2046.
 * </p>
 * <p>
 * Neither <code>javax.activation.MimeType</code> nor <code>javax.mail.internet.ContentType</code> correctly implements {@link Object#equals(Object)} and
 * therefore cannot reliably be used in sets and maps. Furthermore, <code>javax.mail.internet.ContentType</code> as of JDK 6 is not included in default JDK
 * distributions. <code>javax.activation.MimeType</code> was recently added to JDK distributions, so in an earlier implementation this class provided
 * appropriate factory methods, to provide special <code>javax.activation.MimeType</code> instances that provide correct equality checking. Because the
 * <code>javax.activation</code> package is not included in the Android Development Kit, however, and seeing that neither <code>javax.activation.MimeType</code>
 * nor <code>javax.mail.internet.ContentType</code> are in common use, the current implementation provides a fully independent version.
 * </p>
 * @apiNote <a href="https://tools.ietf.org/html/rfc7231">RFC 7231 § 3.1.1.1. Media Type</a> indicates that a lowercase form of <code>charset</code> value is
 *          preferred, e.g.<code>text/html;charset=utf-8</code>.
 * @author Garret Wilson
 * @see <a href="https://tools.ietf.org/html/rfc2045">RFC 2045</a>
 * @see <a href="https://tools.ietf.org/html/rfc2046">RFC 2046</a>
 * @see <a href="https://tools.ietf.org/html/rfc6838">RFC 6838</a>
 * @see <a href="https://tools.ietf.org/html/rfc7231">RFC 7231 § 3.1.1.1. Media Type</a>
 * @see <a href="https://www.iana.org/assignments/media-types/media-types.xhtml">IANA Media Types</a>
 * @see <a href="https://www.w3.org/TR/xhtml-media-types/">XHTML Media Types</a>
 */
public class ContentType { //TODO major version: rename to MediaType; make final; tighten up value object type

	/** The divider character for media type strings. */
	public static final char TYPE_DIVIDER = '/';
	/** The delimiter character separating parameters from the base content type and from each other. */
	public static final char PARAMETER_DELIMITER_CHAR = ';';
	/** The character used to assign parameter values. */
	public static final char PARAMETER_ASSIGNMENT_CHAR = '=';
	/** The character for quoting a string, such as a parameter value with special characters. */
	public static final char STRING_QUOTE_CHAR = '"';
	/** The character delimiting a facet name from the rest of the name as per RFC 6838. */
	public static final char FACET_DELIMITER_CHAR = '.';
	/** The separator character that begins a non-standard extension type. */
	public static final String SUBTYPE_EXTENSION_PREFIX = "x-";
	/** The separator character that delimits a subtype suffix. */
	public static final char SUBTYPE_SUFFIX_DELIMITER_CHAR = '+';
	/** The wildcard character. */
	public static final char TYPE_WILDCARD_CHAR = '*';
	/** The wildcard subtype, matching any subtype. */
	public static final String WILDCARD_SUBTYPE = String.valueOf(TYPE_WILDCARD_CHAR);
	/** The <code>restricted-name-first</code> characters of RFC 6838, defining the first character of a <code>restricted-name</code>. */
	public static final Characters RESTRICTED_NAME_FIRST_CHARACTERS = ALPHA_CHARACTERS.add(DIGIT_CHARACTERS);
	/** The <code>restricted-name-chars</code> characters of RFC 6838, making up a <code>restricted-name</code>. */
	public static final Characters RESTRICTED_NAME_CHARACTERS = RESTRICTED_NAME_FIRST_CHARACTERS.add('!', '#', '$', '&', '-', '^', '_', FACET_DELIMITER_CHAR,
			SUBTYPE_SUFFIX_DELIMITER_CHAR);
	/** The maximum number of <code>restricted-name-chars</code> of a <code>restricted-name</code> according to RFC 6838. */
	public static final byte RESTRICTED_NAME_CHARS_MAX_LENGTH = 126;
	/** The maximum length of a <code>restricted-name</code> according to RFC 6838. */
	public static final byte RESTRICTED_NAME_MAX_LENGTH = RESTRICTED_NAME_CHARS_MAX_LENGTH + 1;
	/** The regular expression pattern defining a <code>restricted-name</code> as per RFC 6838. */
	public static final Pattern RESTRICTED_NAME_PATTERN = Pattern.compile(String.format("%s%s{0,%d}+", characterClassOf(RESTRICTED_NAME_FIRST_CHARACTERS),
			characterClassOf(RESTRICTED_NAME_CHARACTERS), RESTRICTED_NAME_CHARS_MAX_LENGTH));
	/** The <code>tspecials</code> characters of RFC 2045, which require a string to be quoted in a parameter value. */
	public static final Characters SPECIAL_CHARACTERS = Characters.of('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=');
	/** The regular expression pattern defining a <code>restricted-name</code> as per RFC 6838. */
	public static final Pattern RESTRICTED_NAME_WITHOUT_SPECIAL_CHARACTERS_PATTERN = Pattern
			.compile(String.format("%s%s{0,%d}+", characterClassOf(RESTRICTED_NAME_FIRST_CHARACTERS),
					characterClassOf(RESTRICTED_NAME_CHARACTERS.remove(SPECIAL_CHARACTERS)), RESTRICTED_NAME_CHARS_MAX_LENGTH));
	/** The characters of RFC 2046 which are considered illegal in tokens; control characters and non-ASCII characters are not included. */
	public static final Characters ILLEGAL_TOKEN_CHARACTERS = SPECIAL_CHARACTERS.add(SPACE_CHAR);

	/**
	 * A pattern for checking the basic form of a parameter, <em>including</em> the {@value #PARAMETER_DELIMITER_CHAR} delimiter that precedes and separates each
	 * parameter. The pattern may be repeated. The two matching groups are the name and value.
	 * @see #PARAMETER_PATTERN_NAME_GROUP
	 * @see #PARAMETER_PATTERN_VALUE_GROUP
	 */
	public static final Pattern PARAMETER_PATTERN = Pattern.compile(String.format("%s\\s*(%s)=(%s|%s)", PARAMETER_DELIMITER_CHAR, RESTRICTED_NAME_PATTERN,
			RESTRICTED_NAME_WITHOUT_SPECIAL_CHARACTERS_PATTERN, QUOTED_STRING_ALLOWING_ESCAPE_QUOTE));
	/**
	 * A pattern for checking the basic form of a parameter, <em>including</em> the {@value #PARAMETER_DELIMITER_CHAR} delimiter that precedes and separates each
	 * parameter. Suitable for iterating through parameters using {@link Matcher#find()}. The two matching groups are the name and value.
	 * @see #PARAMETER_PATTERN_NAME_GROUP
	 * @see #PARAMETER_PATTERN_VALUE_GROUP
	 */
	public static final Pattern PARAMETER_ITERATE_PATTERN = Pattern.compile("\\G" + PARAMETER_PATTERN);
	/**
	 * The parameters pattern matching group for the parameter name.
	 * @see #PARAMETER_PATTERN
	 */
	public static final int PARAMETER_PATTERN_NAME_GROUP = 1;
	/**
	 * The parameters pattern matching group for the parameter name.
	 * @see #PARAMETER_PATTERN
	 */
	public static final int PARAMETER_PATTERN_VALUE_GROUP = 2;

	/**
	 * A pattern for checking the basic form of regular expressions. This pattern does not take into account all aspects of a regular expression, e.g. special
	 * characters. The parameters group will be <code>null</code> if there are no parameters at all.
	 */
	public static final Pattern PATTERN = Pattern
			.compile(String.format("(%s)/(%s)((?:%s)+)?", RESTRICTED_NAME_PATTERN, RESTRICTED_NAME_PATTERN, PARAMETER_PATTERN));
	/**
	 * The pattern matching group for the primary type.
	 * @see #PATTERN
	 */
	public static final int PATTERN_PRIMARY_TYPE_GROUP = 1;
	/**
	 * The pattern matching group for the subtype.
	 * @see #PATTERN
	 */
	public static final int PATTERN_SUBTYPE_GROUP = 2;
	/**
	 * The pattern matching group for all the parameters, with delimiters.
	 * @see #PATTERN
	 */
	public static final int PATTERN_PARAMETERS_GROUP = 3;

	//discrete top-level media types
	public static final String TEXT_PRIMARY_TYPE = "text";
	public static final String IMAGE_PRIMARY_TYPE = "image";
	public static final String AUDIO_PRIMARY_TYPE = "audio";
	public static final String VIDEO_PRIMARY_TYPE = "video";
	public static final String APPLICATION_PRIMARY_TYPE = "application";
	//composite top-level media types
	public static final String MULTIPART_PRIMARY_TYPE = "multipart";
	public static final String MESSAGE_PRIMARY_TYPE = "message";
	/** The pseudo top-level type used by Java {@link URLConnection} to indicate unknown content by <code>content/unknown</code>. */
	public static final String CONTENT_PRIMARY_TYPE = "content";

	//parameters
	/** The character set parameters. */
	public static final String CHARSET_PARAMETER = "charset";

	//content media types
	/** The pseudo subtype used by Java {@link URLConnection} to indicate unknown content by <code>content/unknown</code>. */
	public static final String UNKNOWN_SUBTYPE = "unknown";

	//application media types
	/** A stream of bytes. */
	public static final String OCTET_STREAM_SUBTYPE = "octet-stream";

	/** A Java object. */
	public static final String X_JAVA_OBJECT = SUBTYPE_EXTENSION_PREFIX + "java-object";

	/** The shared <code>application/octet-stream</code> content type. */
	public static final ContentType APPLICATION_OCTET_STREAM_CONTENT_TYPE = create(APPLICATION_PRIMARY_TYPE, OCTET_STREAM_SUBTYPE);

	/**
	 * Determines if the given character sequence is a content type token, that is, consisting only of non-control ASCII characters with no special characters or
	 * spaces.
	 * @param charSequence The character sequence to check.
	 * @return <code>true</code> if the given characters sequence contains only ASCII characters with no control characters, special characters, or spaces.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @see #ILLEGAL_TOKEN_CHARACTERS
	 */
	public static final boolean isToken(final CharSequence charSequence) {
		return ASCII.isASCIINonControl(charSequence) && notContains(charSequence, ILLEGAL_TOKEN_CHARACTERS);
	}

	/**
	 * Checks to ensure that the given character sequence is a content type token, that is, consisting only of non-control ASCII characters with no special
	 * characters or spaces.
	 * @param <CS> The type of the char sequence.
	 * @param charSequence The character sequence to check.
	 * @return The given character sequence.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 * @throws ArgumentSyntaxException if the given character sequence is not a content type token.
	 * @see #isToken(CharSequence)
	 */
	protected static final <CS extends CharSequence> CS checkToken(final CS charSequence) {
		if(!isToken(charSequence)) {
			throw new ArgumentSyntaxException("Content type token " + charSequence + " must consist only of non-space and non-control ASCII characters.");
		}
		return charSequence;
	}

	private final String primaryType;

	/** @return The primary type of the content type. */
	public String getPrimaryType() {
		return primaryType;
	}

	private final String subType;

	/** @return The subtype of the content type. */
	public String getSubType() {
		return subType;
	}

	private final Set<Parameter> parameters;

	/** @return The set of parameters, which may be empty, but will never be <code>null</code>. */
	public Set<Parameter> getParameters() {
		return parameters;
	}

	/**
	 * Primary type and subtype constructor.
	 * @implNote This private constructor assumes that the given parameter set is immutable and will not be referenced elsewhere, and therefore does not make a
	 *           defensive copy.
	 * @param primaryType The primary type of the content type.
	 * @param subType The subtype of the content type.
	 * @param parameters The content type parameters.
	 * @throws NullPointerException if the given primary type, subtype, and/or parameters is <code>null</code>.
	 * @throws ArgumentSyntaxException if the primary type or subtype does not have the valid syntax.
	 */
	private ContentType(final String primaryType, final String subType, final Set<Parameter> parameters) {
		this.primaryType = checkToken(primaryType);
		this.subType = checkToken(subType);
		this.parameters = requireNonNull(parameters);
	}

	/**
	 * Parses a content type object from a string.
	 * @param charSequence The character sequence representation of the content type.
	 * @return A new content type object parsed from the string.
	 * @throws ArgumentSyntaxException if the string is not a syntactically correct content type.
	 * @deprecated in favor of {@link #parse(CharSequence)}; to be removed in next major version.
	 */
	@Deprecated
	public static ContentType create(final CharSequence charSequence) throws ArgumentSyntaxException {
		return parse(charSequence);
	}

	/**
	 * Parses a content type object from a string.
	 * @param charSequence The character sequence representation of the content type.
	 * @return A new content type object parsed from the string.
	 * @throws ArgumentSyntaxException if the string is not a syntactically correct content type.
	 * @deprecated in favor of {@link #parse(CharSequence)}; to be removed in next major version.
	 */
	@Deprecated
	public static ContentType of(final CharSequence charSequence) throws ArgumentSyntaxException {
		return parse(charSequence);
	}

	/**
	 * Creates a content type object from primary type, a subtype, and optional parameters.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Optional name-value pairs representing parameters of the content type.
	 * @return A new content type object constructed from the given information.
	 * @throws ArgumentSyntaxException if the primary type or subtype does not have the valid syntax.
	 * @deprecated in favor of {@link #of(String, String, Parameter...)}; to be removed in next major version.
	 */
	@Deprecated
	public static ContentType create(final String primaryType, final String subType, final Parameter... parameters) {
		return of(primaryType, subType, parameters);
	}

	/**
	 * Creates a content type object from primary type, a subtype, and optional parameters.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Optional name-value pairs representing parameters of the content type.
	 * @return A new content type object constructed from the given information.
	 * @throws ArgumentSyntaxException if the primary type or subtype does not have the valid syntax.
	 */
	public static ContentType of(final String primaryType, final String subType, final Parameter... parameters) {
		return new ContentType(primaryType, subType, immutableSetOf(parameters)); //create a new content type from the given values, creating an immutable copy of the parameters
	}

	/**
	 * Creates a content type object from primary type, a subtype, and optional parameters.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Zero or more name-value pairs representing parameters of the content type.
	 * @return A new content type object constructed from the given information.
	 * @throws NullPointerException if the given parameters set is <code>null</code>.
	 * @throws ArgumentSyntaxException if the primary type or subtype does not have the valid syntax.
	 */
	public static ContentType of(final String primaryType, final String subType, final Set<Parameter> parameters) {
		return new ContentType(primaryType, subType, immutableSetOf(parameters)); //create a new content type from the given values, creating an immutable copy of the parameters
	}

	/**
	 * Parses a content type object from a string.
	 * @param text The character sequence representation of the content type.
	 * @return A new content type object parsed from the string.
	 * @throws ArgumentSyntaxException if the string is not a syntactically correct content type.
	 */
	public static ContentType parse(final CharSequence text) throws ArgumentSyntaxException {
		final Matcher matcher = PATTERN.matcher(text);
		if(!matcher.matches()) {
			throw new ArgumentSyntaxException("Invalid content type syntax", text);
		}
		final String primaryType = matcher.group(PATTERN_PRIMARY_TYPE_GROUP);
		final String subType = matcher.group(PATTERN_SUBTYPE_GROUP);
		final String parameterString = matcher.group(PATTERN_PARAMETERS_GROUP);
		final Set<Parameter> parameters = parameterString != null ? parseParameters(parameterString) : emptySet();
		return new ContentType(primaryType, subType, parameters);
	}

	/**
	 * Parses parameters of a content type from a string.
	 * @param text The character sequence representing the parameters of the content type, not including the {@value #PARAMETER_DELIMITER_CHAR} delimiter.
	 * @return Content type parameters parsed from the string.
	 * @throws ArgumentSyntaxException if the string is not syntactically correct parameters.
	 */
	public static Set<Parameter> parseParameters(final CharSequence text) throws ArgumentSyntaxException {
		Set<Parameter> parameters = null;
		int lastEnd = 0;
		final Matcher parameterMatcher = PARAMETER_ITERATE_PATTERN.matcher(text);
		while(parameterMatcher.find()) {
			lastEnd = parameterMatcher.end();
			final String parameterName = parameterMatcher.group(PARAMETER_PATTERN_NAME_GROUP);
			String parameterValue = parameterMatcher.group(PARAMETER_PATTERN_VALUE_GROUP);
			if(startsWith(parameterValue, QUOTATION_MARK_CHAR)) { //if this is a quoted value
				assert parameterValue.length() >= 3 && endsWith(parameterValue, QUOTATION_MARK_CHAR) : "Regex expected to ensure matched quotes.";
				parameterValue = parameterValue.substring(1, parameterValue.length() - 1); //remove the surrounding quotes
				parameterValue = parameterValue.replace("\\\"", "\"").replace("\\\\", "\\"); //replace escapes
			}
			if(parameters == null) { //lazily create the parameters
				parameters = new HashSet<>();
			}
			parameters.add(Parameter.of(parameterName, parameterValue));
		}
		if(lastEnd < text.length()) { //if not all the string was matched
			throw new ArgumentSyntaxException("Invalid parameter syntax: " + text);
		}
		return parameters != null ? unmodifiableSet(parameters) : emptySet(); //make the parameters set read-only
	}

	/**
	 * Retrieve the parameter value associated with the given parameter name. Names are comparisons are case-insensitive.
	 * @param name The name of the parameter.
	 * @return The (always unquoted) value associated with the given name, or <code>null</code> if there is no parameter with the given name.
	 * @throws NullPointerException if the given parameter name is <code>null</code>.
	 */
	public String getParameter(final String name) {
		requireNonNull(name);
		for(final Parameter parameter : getParameters()) {
			if(ASCII.equalsIgnoreCase(parameter.getName(), name)) {
				return parameter.getValue();
			}
		}
		return null;
	}

	/**
	 * Matches a content type against a primary type and subtype. Comparisons are case-insensitive. This method supports wildcard subtypes.
	 * @param primaryType The primary type with which to compare the content type.
	 * @param subType The subtype with which to compare the content type.
	 * @return <code>true</code> if the content type has the same primary type and subtype as that given.
	 * @see #WILDCARD_SUBTYPE
	 */
	public boolean match(final String primaryType, final String subType) {
		final String contentTypeSubType = getSubType(); //get the content type's subtype
		return ASCII.equalsIgnoreCase(getPrimaryType(), primaryType)
				&& (ASCII.equalsIgnoreCase(contentTypeSubType, subType) || WILDCARD_SUBTYPE.equals(contentTypeSubType) || WILDCARD_SUBTYPE.equals(subType)); //check the primary type and subtype and wildcards
	}

	/**
	 * Matches a content type against a primary type and subtype, with a class parameter indicating the given object class. This method supports wildcard
	 * subtypes.
	 * @param primaryType The primary type with which to compare the content type.
	 * @param subType The subtype with which to compare the content type.
	 * @param objectClass The class for which to check in the parameters under the key "class".
	 * @return <code>true</code> if the content type has the same primary type and subtype as that given, along with a class parameter.
	 * @see #WILDCARD_SUBTYPE
	 */
	public boolean match(final String primaryType, final String subType, final Class<?> objectClass) {
		return match(primaryType, subType) && objectClass.getName().equals(getParameter("string")); //see if the primary type and subtype match, and that "class" parameter indicates this class TODO use a constant
	}

	/**
	 * Matches a content type against the {@value #APPLICATION_PRIMARY_TYPE} primary type and {@value ContentType#X_JAVA_OBJECT} subtype, with a "class" parameter
	 * indicating the given object class. This method supports wildcard subtypes.
	 * @param objectClass The class for which to check in the parameters under the key "class".
	 * @return <code>true</code> if the content type has the same primary type and subtype as that given, along with a class parameter.
	 * @see #WILDCARD_SUBTYPE
	 */
	public boolean match(final Class<?> objectClass) {
		return match(APPLICATION_PRIMARY_TYPE, X_JAVA_OBJECT); //check for application/x-java-object and class name
	}

	/** @return A string representation of the the base content type, that is, the same primary and subtype as the content type, but with no parameters. */
	@Deprecated
	public String getBaseType() { //for compatibility with javax.activiation.MimeType; remove in favor of getBaseContentType()
		return toString(getPrimaryType(), getSubType());
	}

	/**
	 * Determines the base content type, with no parameters, of the content type. Useful for making comparisons or for storing in canonical form in a hash table.
	 * If this content type is already a base content type, this content type is returned.
	 * @return A content type with the same primary and subtype as the content type, but with no parameters.
	 */
	public ContentType getBaseContentType() {
		return getParameters().isEmpty() ? this : new ContentType(getPrimaryType(), getSubType(), Collections.<Parameter>emptySet()); //if this content type is already just the base type, return that
	}

	/**
	 * Checks to see if the given content type has the same primary type and subtype as this content type. This method does <em>not</em> support wildcards.
	 * @param contentType The content type with which to compare this content type.
	 * @return <code>true</code> if the primary types and base types of the two content types are equal.
	 */
	public boolean hasBaseType(final ContentType contentType) {
		return hasBaseType(contentType.getPrimaryType(), contentType.getSubType());
	}

	/**
	 * Matches a content type against a primary type and subtype with no wildcard support. Comparisons are case-insensitive.
	 * @param primaryType The primary type with which to compare the content type.
	 * @param subType The subtype with which to compare the content type.
	 * @return <code>true</code> if the content type has the same primary type and subtype as that given.
	 * @throws NullPointerException if the primary type and/or subtype is <code>null</code>.
	 */
	public boolean hasBaseType(final String primaryType, final String subType) {
		return ASCII.equalsIgnoreCase(getPrimaryType(), primaryType) && ASCII.equalsIgnoreCase(getSubType(), subType); //check the primary type and subtype
	}

	/**
	 * Determines if the subtype of the content type has the given suffix.
	 * @param suffixes The suffix strings that will be checked, after they are combined into a single suffix, each part prepended with '+'.
	 * @return <code>true</code> if the content type's subtype has the given suffixes.
	 */
	public boolean hasSubTypeSuffix(final String... suffixes) { //TODO implement case insensitivity
		return getSubType().endsWith(createSubTypeSuffix(suffixes)); //see if the content type subtype ends with the given suffixes
	}

	/**
	 * Returns a content type with the given parameter. If this content type already has the given parameter, it will be returned. If this content type has a
	 * parameter with the same name but with a different value, the parameter will be replaced with the one given. Otherwise, the parameter will be added to the
	 * parameters. Parameter name comparisons are case-insensitive.
	 * @param newParameter The new parameter to add or replace.
	 * @return A content type with the given parameter.
	 */
	public ContentType withParameter(final Parameter newParameter) {
		final Set<Parameter> newParameters = new HashSet<ContentType.Parameter>(getParameters().size());
		for(final Parameter parameter : getParameters()) {
			if(ASCII.equalsIgnoreCase(parameter.getName(), newParameter.getName())) { //if this is the same parameter name
				if(parameter.getValue().equals(newParameter.getValue())) { //if the parameter values are the same
					return this; //just use the same content type, because nothing will change; otherwise
				} else { //if the parameter values are different
					continue; //skip this parameter; we'll add it manually afterwards
				}
			}
			newParameters.add(parameter); //add all other parameters normally
		}
		newParameters.add(newParameter); //here we've either ignored a parameter with the same name, or there was no such parameter; either way, add the new one
		return ContentType.of(getPrimaryType(), getSubType(), unmodifiableSet(newParameters)); //create a new content type with the updated parameters
	}

	/**
	 * Returns a content type with the given parameter. If this content type already has the given parameter, it will be returned. If this content type has a
	 * parameter with the same name but with a different value, the parameter will be replaced with the one given. Otherwise, the parameter will be added to the
	 * parameters. Parameter name comparisons are case-insensitive.
	 * @param name The parameter name to add or replace.
	 * @param value The parameter value.
	 * @return A content type with the given parameter.
	 * @throws NullPointerException if the given name and/or value is <code>null</code>.
	 * @throws ArgumentSyntaxException if the name is not a token; or the value contains a space, non-ASCII, or control character.
	 * @see Parameter#of(String, String)
	 */
	public ContentType withParameter(final String name, final String value) {
		return withParameter(Parameter.of(name, value));
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns the hash code of the primary type, the subtype, and the parameters, in a case insensitive manner for the types and
	 *           parameter names.
	 * @return A hash code value for this object.
	 * @see #getPrimaryType()
	 * @see #getSubType()
	 * @see #getParameters()
	 */
	@Override
	public int hashCode() {
		return hash(ASCII.toLowerCase(getPrimaryType()).toString(), ASCII.toLowerCase(getSubType()).toString(), getParameters());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation considers an object equal if it is another {@link ContentType} with the same primary types and subtypes, the same number of
	 *           parameters, and a matching parameter value for every parameter of this content type. Comparisons are case-insensitive for the types and parameter
	 *           names.
	 * @param object The reference object with which to compare.
	 * @see #getPrimaryType()
	 * @see #getSubType()
	 * @see #getParameters()
	 * @see #toString()
	 */
	@Override
	public boolean equals(final Object object) {
		if(this == object) { //if the objects are the same identical object
			return true; //identical objects are always equal
		}
		if(!(object instanceof ContentType)) {
			return false;
		}
		final ContentType contentType = (ContentType)object;
		return ASCII.equalsIgnoreCase(getPrimaryType(), contentType.getPrimaryType()) && ASCII.equalsIgnoreCase(getSubType(), contentType.getSubType())
				&& getParameters().equals(contentType.getParameters());
	}

	/**
	 * Returns a possibly formatted string version of the given content type.
	 * @param formatted Whether the resulting string should be formatted with extra whitespace for human readability.
	 * @return The canonical representation of the content type according to RFC 6838.
	 */
	public String toString(final boolean formatted) {
		return toString(getPrimaryType(), getSubType(), getParameters(), formatted);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns the canonical representation of the content type according to RFC 6838, with no added whitespace.
	 */
	@Override
	public String toString() {
		return toString(false);
	}

	/**
	 * Constructs a string representing a content type.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Optional name-value pairs representing parameters of the content type.
	 * @return A string representing the type in the form "<var>primaryType</var>/<var>subType</var>[;<var>parameters</var>]".
	 */
	public static String toString(final String primaryType, final String subType, final Parameter... parameters) {
		return toString(primaryType, subType, immutableSetOf(parameters));
	}

	/**
	 * Constructs a string representing a content type in canonical form.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Any name-value pairs representing parameters of the content type.
	 * @return A string representing the type in the form "<var>primaryType</var>/<var>subType</var>[;<var>parameters</var>]".
	 * @throws NullPointerException if the given parameters set is <code>null</code>.
	 */
	public static String toString(final String primaryType, final String subType, final Set<Parameter> parameters) {
		return toString(primaryType, subType, parameters, false);
	}

	/**
	 * Constructs a string representing a content type.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Any name-value pairs representing parameters of the content type.
	 * @param formatted Whether the resulting string should be formatted with extra whitespace for human readability.
	 * @return A string representing the type in the form "<var>primaryType</var>/<var>subType</var>[;<var>parameters</var>]".
	 * @throws NullPointerException if the given parameters set is <code>null</code>.
	 */
	public static String toString(final String primaryType, final String subType, final Set<Parameter> parameters, final boolean formatted) {
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(primaryType).append(TYPE_DIVIDER).append(subType); //primaryType/subType
		for(final Parameter parameter : parameters) { //for each parameter
			final String parameterValue = parameter.getValue(); //get the parameter value
			final boolean hasSpecialCharacters = CharSequences.contains(parameterValue, SPECIAL_CHARACTERS); //see if there are any special characters

			stringBuilder.append(PARAMETER_DELIMITER_CHAR); //; name=value
			if(formatted) {
				stringBuilder.append(SPACE_CHAR);
			}
			stringBuilder.append(parameter.getName()).append(PARAMETER_ASSIGNMENT_CHAR);
			if(hasSpecialCharacters) { //quote the value string if necessary
				stringBuilder.append(STRING_QUOTE_CHAR);
			}
			stringBuilder.append(parameterValue);
			if(hasSpecialCharacters) { //quote the value string if necessary
				stringBuilder.append(STRING_QUOTE_CHAR);
			}
		}
		return stringBuilder.toString();
	}

	/**
	 * Creates a content type suffix by prepending '+' to each suffix and concatenating the suffixes.
	 * @param suffixes The suffix strings to combine into a suffix.
	 * @return A suffix composed of the given suffix strings.
	 */
	public static String createSubTypeSuffix(final String... suffixes) {
		final StringBuilder stringBuilder = new StringBuilder();
		for(final String suffix : suffixes) { //for each suffix
			stringBuilder.append(SUBTYPE_SUFFIX_DELIMITER_CHAR).append(suffix); //+suffix
		}
		return stringBuilder.toString(); //return the suffix we constructed
	}

	/**
	 * A content type parameter name/value pair. Neither the name nor the value of a content type parameter can be <code>null</code>.
	 * <p>
	 * The names of parameters are compared in a case-insensitive manner as per RFC 2046.
	 * </p>
	 * <p>
	 * This class considers value quoting a syntax issue of serialization, and thus interprets all values as logical, non-quoted values.
	 * </p>
	 * @author Garret Wilson
	 */
	public static class Parameter extends NameValuePair<String, String> {

		/** The common parameter <code>charset=UTF-8</code>. */
		public static final Parameter CHARSET_UTF_8 = new Parameter(CHARSET_PARAMETER, UTF_8.name());

		/**
		 * Constructor specifying the name and value.
		 * @param name The parameter name.
		 * @param value The parameter value.
		 * @throws NullPointerException if the given name and/or value is <code>null</code>.
		 * @throws ArgumentSyntaxException if the name is not a token; or the value contains a space, non-ASCII, or control character.
		 * @see ContentType#isToken(CharSequence)
		 * @deprecated in favor of {@link #of(String, String)}; to be made private in next major version.
		 */
		@Deprecated
		public Parameter(final String name, final String value) {
			super(checkToken(name), value); //make sure the name is a token
			if(contains(value, SPACE_CHAR) || !ASCII.isASCIINonControl(value)) { //special characters are allowed in the value; but not spaces, non-ASCII, or control characters
				throw new ArgumentSyntaxException("Content type parameter value " + value + " must consist only of non-space and non-control ASCII characters.", value);
			}
		}

		/**
		 * Static factory method specifying the name and value.
		 * @param name The parameter name.
		 * @param value The parameter value.
		 * @return A content type for the indicated name and value.
		 * @throws NullPointerException if the given name and/or value is <code>null</code>.
		 * @throws ArgumentSyntaxException if the name is not a token; or the value contains a space, non-ASCII, or control character.
		 * @see ContentType#isToken(CharSequence)
		 */
		public static Parameter of(@Nonnull final String name, @Nonnull final String value) {
			//often-used parameters
			if(ASCII.equalsIgnoreCase(name, CHARSET_PARAMETER)) { //charset
				if(value.equals(UTF_8.name())) { //charset=UTF-8
					return CHARSET_UTF_8;
				}
			}
			//all other parameters
			return new Parameter(name, value);
		}

		/** {@inheritDoc} This version returns a consistent hash code for all cases of a parameter name. */
		@Override
		public int hashCode() {
			return hash(ASCII.toLowerCase(getName()).toString(), getValue());
		}

		/** {@inheritDoc} This version compares names in a case-insensitive manner. */
		@Override
		public boolean equals(final Object object) {
			if(this == object) {
				return true;
			}
			if(!(object instanceof Parameter)) {
				return false;
			}
			final Parameter parameter = (Parameter)object;
			return ASCII.equalsIgnoreCase(getName(), parameter.getName()) && getValue().equals(parameter.getValue());
		}
	}

}
