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
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.*;
import java.util.stream.Stream;

import javax.annotation.*;

import static java.util.Objects.*;
import static java.util.stream.Collectors.*;

import java.io.IOException;

import static com.globalmentor.collections.Sets.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.SPACE_CHAR;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Characters.QUOTATION_MARK_CHAR;
import static com.globalmentor.text.ABNF.*;
import static com.globalmentor.text.RegularExpressions.*;
import static java.nio.charset.StandardCharsets.*;
import static java.util.Collections.*;

import com.globalmentor.java.*;
import com.globalmentor.model.NameValuePair;
import com.globalmentor.text.*;

/**
 * An encapsulation of an Internet media content type as previously defined in <a href="https://tools.ietf.org/html/rfc2046"><cite>RFC 2046: MIME Part 2: Media
 * Types</cite></a>; and most recently in <a href="https://tools.ietf.org/html/rfc6838"><cite>RFC 6838: Media Type Specifications and Registration
 * Procedures</cite></a>. The full syntax for a content type and its parameters are found in <a href="https://tools.ietf.org/html/rfc2045"><cite>RFC 2045: MIME
 * Part 1: Format of Internet Message Bodies</cite></a>
 * @apiNote Neither <code>javax.activation.MimeType</code> nor <code>javax.mail.internet.ContentType</code> correctly implements {@link Object#equals(Object)}
 *          and therefore cannot reliably be used in sets and maps. Furthermore, <code>javax.mail.internet.ContentType</code> as of JDK 6 is not included in
 *          default JDK distributions. <code>javax.activation.MimeType</code> was recently added to JDK distributions, so in an earlier implementation this
 *          class provided appropriate factory methods, to provide special <code>javax.activation.MimeType</code> instances that provide correct equality
 *          checking. Because the <code>javax.activation</code> package is not included in the Android Development Kit, however, and seeing that neither
 *          <code>javax.activation.MimeType</code> nor <code>javax.mail.internet.ContentType</code> are in common use, the current implementation provides a
 *          fully independent version.
 * @apiNote Internet media types are currently governed by <a href="https://tools.ietf.org/html/rfc6838"><cite>RFC 6838: Media Type Specifications and
 *          Registration Procedures</cite></a>. <a href="https://tools.ietf.org/html/rfc6532"><cite>RFC 6532: Internationalized Email Headers § 3.2. Syntax
 *          Extensions to RFC 5322</cite></a> extends extends the syntax to support UTF-8. According to the
 *          <a href="https://mimesniff.spec.whatwg.org/#http-quoted-string-token-code-point">WhatWG</a>, a quoted string follows
 *          <a href="https://tools.ietf.org/html/rfc7230#section-3.2.6">RFC 7230: Hypertext Transfer Protocol (HTTP/1.1): Message Syntax and Routing § 3.2.6.
 *          Field Value Components</a>.
 * @implSpec This class normalizes type, subtype, and parameter names, which are ASCII case-insensitive, to ASCII lowercase; along with the <code>charset</code>
 *           parameter value. All other parameter values are left as-is. All other parameter values that are case-insensitive should be passed as lowercase to
 *           ensure correct equality comparisons.
 * @implSpec This implementation does not support parameter values containing control characters except for the horizontal tab character, permitted by
 *           <a href="https://tools.ietf.org/html/rfc7230#section-3.2.6">RFC 7230 § 3.2.6.</a>.
 * @implSpec This implementation supports empty parameter values if they are quoted.
 * @implNote Compare this implementation to that of Guava's
 *           <a href="https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/net/MediaType.html"><code>com.google.common.net.MediaType</code></a>
 *           which, in addition to normalizing type, subtype, and parameter names to lowercase, also normalizes the value of the <code>charset</code> attribute
 *           to lowercase. Note also that <a href="https://tools.ietf.org/html/rfc7231">RFC 7231 § 3.1.1.1. Media Type</a> indicates that a lowercase form of
 *           <code>charset</code> value is preferred, e.g.<code>text/html;charset=utf-8</code>. In addition <a href="https://tools.ietf.org/html/rfc2046">RFC
 *           2046 § 4.1.2. Charset Parameter</a> indicates that if non-<code>text</code> types specify a <code>charset</code> value, "the same syntax and values
 *           should be used".
 * @author Garret Wilson
 * @see <a href="https://tools.ietf.org/html/rfc2045">RFC 2045</a>
 * @see <a href="https://tools.ietf.org/html/rfc2046">RFC 2046</a>
 * @see <a href="https://tools.ietf.org/html/rfc6838">RFC 6838</a>
 * @see <a href="https://tools.ietf.org/html/rfc7231">RFC 7231 § 3.1.1.1. Media Type</a>
 * @see <a href="https://www.iana.org/assignments/media-types/media-types.xhtml">IANA Media Types</a>
 * @see <a href="https://www.w3.org/TR/xhtml-media-types/">XHTML Media Types</a>
 */
public final class ContentType { //TODO major version: rename to MediaType

	/** The divider character for media type strings. */
	public static final char TYPE_DIVIDER = '/';
	/** The delimiter character separating parameters from the base content type and from each other. */
	public static final char PARAMETER_DELIMITER_CHAR = ';';
	/** The character used to assign parameter values. */
	public static final char PARAMETER_ASSIGNMENT_CHAR = '=';
	/** The character for quoting a string, such as a parameter value with special characters. */
	public static final char STRING_QUOTE_CHAR = '"';
	/** The character for escaping a quoted string. */
	public static final char STRING_ESCAPE_CHAR = '\\';
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
	/**
	 * The characters of RFC 2045 which are considered illegal in tokens such as non-quoted strings.
	 * @implSpec This currently does not include the control characters other than the horizontal tab. A future version may detect this using the
	 *           <code>\p{Cntrl}</code> character class; for now the implementation checks manually and forbids control characters except for tab.
	 * @see ABNF#CTL_CHARACTERS
	 */
	public static final Characters ILLEGAL_TOKEN_CHARACTERS = SPECIAL_CHARACTERS.add(SPACE_CHAR).add(HTAB);
	/**
	 * The control characters not allowed in a quoted string as per RFC 7230.
	 * @see <a href="https://tools.ietf.org/html/rfc7230#section-3.2.6">RFC 7230 § 3.2.6.</a>
	 */
	public static final Characters QUOTED_STRING_PROHIBITED_CONTROL_CHARACTERS = CTL_CHARACTERS.remove(HTAB);
	/** Characters required to be escaped in a quoted string. */
	public static final Characters QUOTED_STRING_REQUIRED_ESCAPED_CHARACTERS = Characters.of(STRING_ESCAPE_CHAR, STRING_QUOTE_CHAR);

	/**
	 * Confirms that the given input conforms to the rules for <code>restricted-name</code> according to RFC 6838, returning the given input.
	 * @apiNote This method is useful for checking a type, subtype, or parameter name.
	 * @param <C> The type of character sequence input.
	 * @param input The character sequence to check.
	 * @return The given input.
	 * @throws NullPointerException if the given input is <code>null</code>.
	 * @throws IllegalArgumentException if the given input does not conform to the rules for <code>restricted-name</code>.
	 * @see #RESTRICTED_NAME_PATTERN
	 */
	public static <C extends CharSequence> C checkArgumentRestrictedName(final C input) {
		checkArgumentMatches(input, RESTRICTED_NAME_PATTERN, "Invalid restricted name `%s`.", input);
		return input;
	}

	/**
	 * A pattern for checking the basic form of a parameter, <em>including</em> the {@value #PARAMETER_DELIMITER_CHAR} delimiter that precedes and separates each
	 * parameter. The pattern may be repeated. The two matching groups are the name and value.
	 * @see #PARAMETER_PATTERN_NAME_GROUP
	 * @see #PARAMETER_PATTERN_VALUE_GROUP
	 */
	public static final Pattern PARAMETER_PATTERN = Pattern.compile(String.format("%s\\s*(%s)=(%s+|%s)", PARAMETER_DELIMITER_CHAR, RESTRICTED_NAME_PATTERN,
			characterClassNotOf(ILLEGAL_TOKEN_CHARACTERS), "\"(?:[^\\\\\"]++|\\\\.)*+\""));
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
	public static final ContentType APPLICATION_OCTET_STREAM_CONTENT_TYPE = ContentType.of(APPLICATION_PRIMARY_TYPE, OCTET_STREAM_SUBTYPE);

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
	 * @implSpec The primary type and subtype are each normalized to lowercase.
	 * @implNote This private constructor assumes that the given parameter set is immutable and will not be referenced elsewhere, and therefore does not make a
	 *           defensive copy.
	 * @param primaryType The primary type of the content type.
	 * @param subType The subtype of the content type.
	 * @param parameters The content type parameters.
	 * @throws NullPointerException if the given primary type, subtype, and/or parameters is <code>null</code>.
	 * @throws IllegalArgumentException if the primary type and/or subtype does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 */
	private ContentType(final String primaryType, final String subType, final Set<Parameter> parameters) {
		this.primaryType = ASCII.toLowerCase(checkArgumentRestrictedName(primaryType)).toString();
		this.subType = ASCII.toLowerCase(checkArgumentRestrictedName(subType)).toString();
		this.parameters = requireNonNull(parameters);
	}

	/**
	 * Parses a content type object from a sequence of characters.
	 * @implSpec The primary type, subtype, and parameter names, if any, are each normalized to lowercase. The value of the {@value ContentType#CHARSET_PARAMETER}
	 *           parameter, if present, is normalized to lowercase.
	 * @param charSequence The character sequence representation of the content type.
	 * @return A new content type object parsed from the string.
	 * @throws IllegalArgumentException if the primary type, subtype, and/or a parameter name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN}
	 *           pattern.
	 * @deprecated in favor of {@link #parse(CharSequence)}; to be removed in next major version.
	 */
	@Deprecated
	public static ContentType create(final CharSequence charSequence) {
		return parse(charSequence);
	}

	/**
	 * Parses a content type object from a sequence of characters.
	 * @implSpec The primary type, subtype, and parameter names, if any, are each normalized to lowercase. The value of the {@value ContentType#CHARSET_PARAMETER}
	 *           parameter, if present, is normalized to lowercase.
	 * @param charSequence The character sequence representation of the content type.
	 * @return A new content type object parsed from the string.
	 * @throws IllegalArgumentException if the name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 * @throws IllegalArgumentException if the primary type, subtype, and/or a parameter name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN}
	 *           pattern.
	 * @deprecated in favor of {@link #parse(CharSequence)}; to be removed in next major version.
	 */
	@Deprecated
	public static ContentType of(final CharSequence charSequence) {
		return parse(charSequence);
	}

	/**
	 * Creates a content type object from primary type, a subtype, and optional parameters.
	 * @implSpec The primary type and subtype are each normalized to lowercase.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Optional name-value pairs representing parameters of the content type.
	 * @return A new content type object constructed from the given information.
	 * @throws IllegalArgumentException if the primary type and/or subtype does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 * @deprecated in favor of {@link #of(String, String, Parameter...)}; to be removed in next major version.
	 */
	@Deprecated
	public static ContentType create(final String primaryType, final String subType, final Parameter... parameters) {
		return of(primaryType, subType, parameters);
	}

	/**
	 * Returns a content type object from primary type, a subtype, and optional parameters.
	 * @implSpec The primary type and subtype are each normalized to lowercase.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Optional name-value pairs representing parameters of the content type.
	 * @return A new content type object constructed from the given information.
	 * @throws IllegalArgumentException if the primary type and/or subtype does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 */
	public static ContentType of(final String primaryType, final String subType, final Parameter... parameters) {
		return new ContentType(primaryType, subType, immutableSetOf(parameters)); //create a new content type from the given values, creating an immutable copy of the parameters
	}

	/**
	 * Returns a content type object from primary type, a subtype, and optional parameters.
	 * @implSpec The primary type and subtype are each normalized to lowercase.
	 * @param primaryType The primary type.
	 * @param subType The subtype.
	 * @param parameters Zero or more name-value pairs representing parameters of the content type.
	 * @return A new content type object constructed from the given information.
	 * @throws NullPointerException if the given parameters set is <code>null</code>.
	 * @throws IllegalArgumentException if the primary type and/or subtype does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 */
	public static ContentType of(final String primaryType, final String subType, final Set<Parameter> parameters) {
		return new ContentType(primaryType, subType, immutableSetOf(parameters)); //create a new content type from the given values, creating an immutable copy of the parameters
	}

	/**
	 * Parses a content type object from a sequence of characters.
	 * @implSpec The primary type, subtype, and parameter names, if any, are each normalized to lowercase. The value of the {@value ContentType#CHARSET_PARAMETER}
	 *           parameter, if present, is normalized to lowercase.
	 * @param text The character sequence representation of the content type.
	 * @return A new content type object parsed from the string.
	 * @throws IllegalArgumentException if the primary type, subtype, and/or a parameter name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN}
	 *           pattern.
	 */
	public static ContentType parse(final CharSequence text) {
		final Matcher matcher = checkArgumentMatches(text, PATTERN, "Invalid content type syntax for `%s`.", text);
		final String primaryType = matcher.group(PATTERN_PRIMARY_TYPE_GROUP);
		final String subType = matcher.group(PATTERN_SUBTYPE_GROUP);
		final String parameterString = matcher.group(PATTERN_PARAMETERS_GROUP);
		final Set<Parameter> parameters = parameterString != null ? parseParameters(parameterString) : emptySet();
		return new ContentType(primaryType, subType, parameters);
	}

	/**
	 * Parses parameters of a content type from a sequence of characters.
	 * @implSpec The parameter names are each normalized to lowercase. The value of the {@value ContentType#CHARSET_PARAMETER} parameter, if present, is
	 *           normalized to lowercase.
	 * @param text The character sequence representing the parameters of the content type, not including the {@value #PARAMETER_DELIMITER_CHAR} delimiter.
	 * @return Content type parameters parsed from the string.
	 * @throws IllegalArgumentException if the string is not syntactically correct parameters, or if a parameter name does not conform to the
	 *           {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 */
	public static Set<Parameter> parseParameters(final CharSequence text) {
		Set<Parameter> parameters = null;
		int lastEnd = 0;
		final Matcher parameterMatcher = PARAMETER_ITERATE_PATTERN.matcher(text);
		while(parameterMatcher.find()) {
			lastEnd = parameterMatcher.end();
			final String parameterName = parameterMatcher.group(PARAMETER_PATTERN_NAME_GROUP);
			final String parameterValue = Parameter.parseValue(parameterMatcher.group(PARAMETER_PATTERN_VALUE_GROUP));
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
	 * Retrieve the parameter value associated with the given parameter name. Names are comparisons are ASCII case-insensitive.
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
	 * Matches a content type against a primary type and subtype. Comparisons are ASCII case-insensitive. This method supports wildcard subtypes.
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
	 * Matches a content type against a primary type and subtype with no wildcard support. Comparisons are ASCII case-insensitive.
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
	 * @implSpec Suffixes are compared on an ASCII case-insensitive basis.
	 * @implNote This implementation restricts each suffix to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern, the same pattern applicable to a subtype,
	 *           although this requirement is not explicit in RFC 6838.
	 * @param suffixes The suffix strings that will be checked, after they are combined into a single suffix, each part prepended with '+'.
	 * @return <code>true</code> if the content type's subtype has the given suffixes.
	 * @throws IllegalArgumentException if a suffix does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern, including if it already begins
	 *           with the {@value #SUBTYPE_SUFFIX_DELIMITER_CHAR} delimiter.
	 */
	public boolean hasSubTypeSuffix(final String... suffixes) {
		return getSubType().endsWith(createSubTypeSuffix(suffixes));
	}

	/**
	 * Returns a content type with the given parameter, replacing any existing parameter with the same name. If this content type already has the given parameter,
	 * this content type will be returned. If this content type has a parameter with the same name but with a different value, the parameter will be replaced with
	 * the one given. Otherwise, the parameter will be added to the parameters.
	 * @implSpec Parameter name comparisons are ASCII case-insensitive. The comparison of the {@value #CHARSET_PARAMETER} parameter value is performed in an ASCII
	 *           case-insensitive manner. All other parameter values are compared with case sensitivity.
	 * @param newParameter The new parameter to add or replace.
	 * @return A content type with the given parameter.
	 */
	public ContentType withParameter(final Parameter newParameter) {
		//As the given parameter names and value has already been normalized to lowercase as needed,
		//comparisons can be performed normally. However we can't short-circuit with `getParameters().contains(newParameter)`,
		//because there may be multiple parameters with the same name, and we need to remove the others.
		final String parameterName = newParameter.getName();
		//the parameter wasn't found; see if there is one we need to remove
		final Set<Parameter> newParameters = Stream.concat(
				//remove any parameter with the same name
				getParameters().stream().filter(param -> !param.getName().equals(parameterName)),
				//add the new parameter
				Stream.of(newParameter))
				//collect into an immutable set
				.collect(collectingAndThen(toSet(), Collections::unmodifiableSet));
		if(getParameters().equals(newParameters)) { //if our change didn't actually result in changes
			return this; //just return this instance, as there would be no difference
		}
		return ContentType.of(getPrimaryType(), getSubType(), newParameters); //create a new content type with the updated parameters
	}

	/**
	 * Returns a content type with the given parameter, replacing any existing parameter with the same name. If this content type already has the given parameter,
	 * this content type will be returned. If this content type has a parameter with the same name but with a different value, the parameter will be replaced with
	 * the one given. Otherwise, the parameter will be added to the parameters.
	 * @implSpec Parameter name comparisons are ASCII case-insensitive. The comparison of the {@value #CHARSET_PARAMETER} parameter value is performed in an ASCII
	 *           case-insensitive manner. All other parameter values are compared with case sensitivity.
	 * @implSpec This implementation delegates to {@link #withParameter(Parameter)}.
	 * @param name The parameter name to add or replace.
	 * @param value The parameter value.
	 * @return A content type with the given parameter.
	 * @throws NullPointerException if the given name and/or value is <code>null</code>.
	 * @throws IllegalArgumentException if the parameter name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
	 * @see Parameter#of(String, String)
	 */
	public ContentType withParameter(final String name, final String value) {
		return withParameter(Parameter.of(name, value));
	}

	/**
	 * Returns a content type with the given charset as a {@value #CHARSET_PARAMETER} parameter, replacing any existing parameter with the same name. If this
	 * content type already has the given parameter, this content type will be returned. If this content type has a parameter with the same name but with a
	 * different value, the parameter will be replaced with the one given. Otherwise, the parameter will be added to the parameters.
	 * @implSpec Parameter name comparisons are ASCII case-insensitive. The comparison of the {@value #CHARSET_PARAMETER} parameter value is performed in an ASCII
	 *           case-insensitive manner.
	 * @implSpec This implementation delegates to {@link #withParameter(String, String)}.
	 * @param charset The charset value to add or replace.
	 * @return A content type with the given parameter.
	 * @throws NullPointerException if the given charset is <code>null</code>.
	 * @see #withParameter(String, String)
	 * @see #CHARSET_PARAMETER
	 * @see Charset#name()
	 */
	public ContentType withCharset(@Nonnull final Charset charset) {
		return withParameter(CHARSET_PARAMETER, charset.name());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns the hash code of the primary type, the subtype, the parameter names, and the {@value #CHARSET_PARAMETER} parameter
	 *           value in a case insensitive manner, as these items have already been normalized by this class.
	 * @return A hash code value for this object.
	 * @see #getPrimaryType()
	 * @see #getSubType()
	 * @see #getParameters()
	 */
	@Override
	public int hashCode() {
		return hash(getPrimaryType(), getSubType(), getParameters());
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation considers an object equal if it is another {@link ContentType} with the same primary types and subtypes, the same number of
	 *           parameters, and a matching parameter value for every parameter of this content type. Comparisons are ASCII case-insensitive for the primary type,
	 *           the subtype, the parameter names, and the {@value #CHARSET_PARAMETER} parameter value, as these items have already been normalized by this class.
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
		return getPrimaryType().equals(contentType.getPrimaryType()) && getSubType().equals(contentType.getSubType())
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
	 * @see Parameter#toValueString()
	 */
	public static String toString(final String primaryType, final String subType, final Set<Parameter> parameters, final boolean formatted) {
		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append(primaryType).append(TYPE_DIVIDER).append(subType); //primaryType/subType
		for(final Parameter parameter : parameters) { //for each parameter
			stringBuilder.append(PARAMETER_DELIMITER_CHAR); //; name=value
			if(formatted) {
				stringBuilder.append(SPACE_CHAR);
			}
			stringBuilder.append(parameter.getName()).append(PARAMETER_ASSIGNMENT_CHAR);
			final String parameterValue = parameter.getValue(); //get the parameter value
			try {
				Parameter.appendValueTo(stringBuilder, parameterValue);
			} catch(final IOException ioException) {
				throw new AssertionError(ioException); //string builders do not throw I/O exceptions when appending
			}
		}
		return stringBuilder.toString();
	}

	/**
	 * Creates a content type suffix by prepending '+' to each suffix and concatenating the suffixes.
	 * @implSpec Each suffix is normalized to lowercase.
	 * @implNote This implementation restricts each suffix to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern, the same pattern applicable to a subtype,
	 *           although this requirement is not explicit in RFC 6838.
	 * @param suffixes The suffix strings to combine into a suffix.
	 * @return A suffix composed of the given suffix strings.
	 * @throws IllegalArgumentException if a suffix does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern, including if it already begins
	 *           with the {@value #SUBTYPE_SUFFIX_DELIMITER_CHAR} delimiter.
	 * @see #createSubTypeSuffix(String...)
	 */
	public static String createSubTypeSuffix(final String... suffixes) {
		final StringBuilder stringBuilder = new StringBuilder();
		for(final String suffix : suffixes) { //for each suffix
			stringBuilder.append(SUBTYPE_SUFFIX_DELIMITER_CHAR).append(ASCII.toLowerCase(checkArgumentRestrictedName(suffix))); //+suffix
		}
		return stringBuilder.toString(); //return the suffix we constructed
	}

	/**
	 * A content type parameter name/value pair. Neither the name nor the value of a content type parameter can be <code>null</code>.
	 * @implSpec This class normalizes parameter names, which are ASCII case-insensitive, to lowercase; along with the <code>charset</code> parameter value. All
	 *           other parameter values are left as-is. All other parameter values that are case-insensitive should be passed as lowercase to ensure correct
	 *           equality comparisons.
	 * @implSpec This implementation does not allow control characters.
	 * @implNote Compare this implementation to that of Guava's
	 *           <a href="https://guava.dev/releases/snapshot-jre/api/docs/com/google/common/net/MediaType.html"><code>com.google.common.net.MediaType</code></a>
	 *           which, in addition to normalizing type, subtype, and parameter names to lowercase, also normalizes the value of the <code>charset</code>
	 *           attribute to lowercase. Note also that <a href="https://tools.ietf.org/html/rfc7231">RFC 7231 § 3.1.1.1. Media Type</a> indicates that a
	 *           lowercase form of <code>charset</code> value is preferred, e.g.<code>text/html;charset=utf-8</code>. In addition
	 *           <a href="https://tools.ietf.org/html/rfc2046">RFC 2046 § 4.1.2. Charset Parameter</a> indicates that if non-<code>text</code> types specify a
	 *           <code>charset</code> value, "the same syntax and values should be used".
	 * @implNote This class considers value quoting a syntax issue of serialization, and thus interprets all values as logical, non-quoted values.
	 * @author Garret Wilson
	 */
	public static final class Parameter extends NameValuePair<String, String> {

		/** The common parameter <code>charset=UTF-8</code>. */
		public static final Parameter CHARSET_UTF_8 = new Parameter(CHARSET_PARAMETER, UTF_8.name());

		/**
		 * Constructor specifying the name and value.
		 * @implSpec The parameter name is normalized to lowercase. The value of the {@value ContentType#CHARSET_PARAMETER} parameter is normalized to lowercase.
		 * @param name The parameter name.
		 * @param value The parameter value.
		 * @throws NullPointerException if the given name and/or value is <code>null</code>.
		 * @throws IllegalArgumentException if the name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
		 * @throws IllegalArgumentException if the parameter value includes control characters other than horizontal tab.
		 * @deprecated in favor of {@link #of(String, String)}; to be made private in next major version.
		 */
		@Deprecated
		public Parameter(final String name, final String value) {
			super(ASCII.toLowerCase(checkArgumentRestrictedName(name)).toString(),
					ASCII.equalsIgnoreCase(name, CHARSET_PARAMETER) ? ASCII.toLowerCase(value).toString() : value);
			checkArgument(!contains(value, QUOTED_STRING_PROHIBITED_CONTROL_CHARACTERS), "Parameter value `%s` containing non-tab control characters not supported.",
					value);
		}

		/**
		 * Static factory method specifying the name and value.
		 * @implSpec The parameter name is normalized to lowercase. The value of the {@value ContentType#CHARSET_PARAMETER} parameter is normalized to lowercase.
		 * @param name The parameter name.
		 * @param value The parameter value.
		 * @return A content type for the indicated name and value.
		 * @throws NullPointerException if the given name and/or value is <code>null</code>.
		 * @throws IllegalArgumentException if the name does not conform to the {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
		 * @throws IllegalArgumentException if the parameter value includes control characters.
		 */
		public static Parameter of(@Nonnull final String name, @Nonnull final String value) {
			//often-used parameters
			if(ASCII.equalsIgnoreCase(name, CHARSET_PARAMETER)) { //charset
				if(ASCII.equalsIgnoreCase(value, UTF_8.name())) { //charset=UTF-8 (without regard to case)
					return CHARSET_UTF_8;
				}
			}
			//all other parameters
			return new Parameter(name, value);
		}

		/**
		 * Parses a parameter of a content type from a sequence of characters. Quoted/escaped values are supported and decoded, but otherwise no normalization is
		 * performed.
		 * @implSpec This implementation supports an empty string value only if it is quoted.
		 * @implSpec This implementation allows any character except control characters to be escaped.
		 * @implSpec This implementation does not allow control characters other than horizontal tab.
		 * @param text The character sequence representing the parameters of the value.
		 * @return The parsed value.
		 * @throws IllegalArgumentException if the string is not syntactically correct parameters, or if a parameter name does not conform to the
		 *           {@link ContentType#RESTRICTED_NAME_PATTERN} pattern.
		 */
		public static String parseValue(CharSequence text) {
			checkArgument(text.length() > 0, "Unquoted empty parameter value not supported.");
			checkArgument(!contains(text, QUOTED_STRING_PROHIBITED_CONTROL_CHARACTERS), "Parameter value `%s` containing non-tab control characters not supported.",
					text);
			if(startsWith(text, QUOTATION_MARK_CHAR)) { //if this is a quoted value
				final int length = text.length();
				checkArgument(length >= 2 && endsWith(text, QUOTATION_MARK_CHAR), "Parameter value `%s` missing ending quote.", text);
				final StringBuilder stringBuilder = new StringBuilder(length - 2); //we'll leave off the quotes
				for(int i = 1; i < length - 1; i++) {
					char c = text.charAt(i);
					if(c == STRING_ESCAPE_CHAR) {
						i++; //skip the escape character
						if(i == length - 1) {
							throw new ArgumentSyntaxException("Incomplete ending escape sequence in parameter values `%`.", text);
						}
						c = text.charAt(i);
					}
					stringBuilder.append(c);
				}
				return stringBuilder.toString(); //we decoded the string manually, so short-circuit and return the value
			}
			return text.toString();
		}

		/**
		 * Returns a string representation of the value, quoting and escaping it as necessary.
		 * @implSpec This implementation delegates to {@link #appendValueTo(Appendable, CharSequence)}.
		 * @apiNote This method differs from {@link #getValue()}, which returns the raw value with no quoting or escaping.
		 * @implSpec This implementation does not allow control characters.
		 * @return The string version of the value.
		 */
		public String toValueString() {
			try {
				return appendValueTo(new StringBuilder(), getValue()).toString();
			} catch(final IOException ioException) {
				throw new AssertionError(ioException); //string builders do not throw I/O exceptions when appending
			}
		}

		/**
		 * Appends a string representing a parameter value to an appendable, quoting and escaping it as necessary.
		 * @implSpec This implementation does not allow control characters other than horizontal tab.
		 * @implSpec This implementation quotes empty strings.
		 * @param <A> The type of appendable being used.
		 * @param appendable The appendable, such as a {@link StringBuilder}, to which the value should be appended.
		 * @param parameterValue The parameter value.
		 * @return The given appendable.
		 * @throws IllegalArgumentException if the parameter value includes control characters.
		 * @throws IOException If an I/O error occurs appending the value.
		 */
		public static <A extends Appendable> A appendValueTo(@Nonnull final A appendable, @Nonnull CharSequence parameterValue) throws IOException {
			checkArgument(!contains(parameterValue, QUOTED_STRING_PROHIBITED_CONTROL_CHARACTERS),
					"Parameter value `%s` containing non-tab control characters not supported.", parameterValue);
			final boolean needsQuotes = contains(parameterValue, ILLEGAL_TOKEN_CHARACTERS) || parameterValue.length() == 0; //see if there are any characters requiring quoting
			if(needsQuotes) { //quote the value if necessary
				appendable.append(STRING_QUOTE_CHAR);
				if(contains(parameterValue, QUOTED_STRING_REQUIRED_ESCAPED_CHARACTERS)) { //if there are characters needing escaping, append one at a time to be more efficient
					for(int i = 0, length = parameterValue.length(); i < length; i++) {
						final char c = parameterValue.charAt(i);
						if(QUOTED_STRING_REQUIRED_ESCAPED_CHARACTERS.contains(c)) { //escape if needed
							appendable.append(STRING_ESCAPE_CHAR);
						}
						appendable.append(c);
					}
					appendable.append(STRING_QUOTE_CHAR);
					return appendable; //we appended manually and are finished
				}
			}
			appendable.append(parameterValue); //no escaping was needed; just append normally
			if(needsQuotes) {
				appendable.append(STRING_QUOTE_CHAR);
			}
			return appendable;
		}
	}

}
