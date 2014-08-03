/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.rdf.dicto;

import java.net.URI;
import java.util.Locale;

import com.globalmentor.model.Locales;
import com.globalmentor.rdf.*;

/**
 * Constants and utilities used in Dictionary Ontology (Dicto) processing.
 * <p>
 * This class also serves as a resource factory that knows how to create RDF resources for Dicto resource descriptions.
 * </p>
 * @author Garret Wilson
 */
public class Dicto implements RDFResourceFactory {

	/** The name extension for Dictionary Ontology (Dicto) dictionaries. */
	public final static String DICTO_NAME_EXTENSION = "dicto";

	/** The recommended prefix to the Dicto ontology namespace. */
	public final static String DICTO_NAMESPACE_PREFIX = "dicto";
	/** The URI to the Dicto namespace. */
	public final static URI DICTO_NAMESPACE_URI = URI.create("http://globalmentor.com/namespaces/2003/dicto#");

	//Dicto class names
	/** The local name of dicto:Dictionary. */
	public final static String DICTIONARY_CLASS_NAME = "Dictionary";
	/** The local name of dicto:Word. */
	public final static String WORD_CLASS_NAME = "Word";

	//Dicto property names
	/** The definition of an entry. The local name of dicto:definition. */
	public final static String DEFINITION_PROPERTY_NAME = "definition";
	/** The entries of a dictionary. The local name of dicto:entries. */
	public final static String ENTRIES_PROPERTY_NAME = "entries";
	/** The gender of a word. The local name of dicto:gender. */
	public final static String GENDER_PROPERTY_NAME = "gender";
	/** The source language of a dictionary. The local name of dicto:language. */
	public final static String LANGUAGE_PROPERTY_NAME = "language";
	/** The number of an entry. The local name of dicto:number. */
	public final static String NUMBER_PROPERTY_NAME = "number";
	/** The orthography of an entry. The local name of dicto:orthography. */
	public final static String ORTHOGRAPHY_PROPERTY_NAME = "orthography";
	/** The pronunciation of an entry. The local name of dicto:pronunciation. */
	public final static String PRONUNCIATION_PROPERTY_NAME = "pronunciation";
	/** The part of speech of an entry. The local name of dicto:speechPart. */
	public final static String SPEECH_PART_PROPERTY_NAME = "speechPart";
	/** The translation of an entry. The local name of dicto:translation. */
	public final static String TRANSLATION_PROPERTY_NAME = "translation";
	/** The transliteration of an entry. The local name of dicto:transliteration. */
	public final static String TRANSLITERATION_PROPERTY_NAME = "transliteration";

	/**
	 * Adds a <code>dicto:language</code> property with the given value to the resource.
	 * @param resource The resource to which the property should be added.
	 * @param locale The property value to add.
	 * @return The added literal property value.
	 */
	public static RDFLiteral addLanguage(final RDFResource resource, final Locale locale) {
		//add the literal language tag for this locale
		return resource.addProperty(DICTO_NAMESPACE_URI, LANGUAGE_PROPERTY_NAME, Locales.getLanguageTag(locale));
	}

	/**
	 * Returns the value of the first <code>dicto:Language</code> property.
	 * @param resource The resource the property of which should be located.
	 * @return The value of the first <code>dicto:language</code> property, or <code>null</code> if no such property exists or the property value does not contain
	 *         a literal language tag.
	 */
	public static Locale getLanguage(final RDFResource resource) {
		final RDFObject languageObject = resource.getPropertyValue(DICTO_NAMESPACE_URI, LANGUAGE_PROPERTY_NAME);
		if(languageObject instanceof RDFLiteral) { //if this is a literal value
			return Locales.createLocale(((RDFLiteral)languageObject).getLexicalForm()); //create a locale from the literal's lexical form
		} else { //if there is no literal language tag
			return null; //show that we can't create a language locale from a non-literal
		}
	}

	/**
	 * Sets the <code>dicto:language</code> property with the given value to the resource.
	 * @param resource The resource to which the property should be set.
	 * @param locale The property value to set.
	 * @return The added literal property value.
	 */
	public static RDFLiteral setLanguage(final RDFResource resource, final Locale locale) {
		//set the literal language tag for this locale
		return resource.setProperty(DICTO_NAMESPACE_URI, LANGUAGE_PROPERTY_NAME, Locales.getLanguageTag(locale));
	}

	/**
	 * Creates a resource with the provided reference URI based upon the type reference URI composed of the given XML serialization type namespace and local name.
	 * A type property derived from the specified type namespace URI and local name will be added to the resource.
	 * <p>
	 * This implementation creates Dicto-specific resources.
	 * </p>
	 * @param referenceURI The reference URI of the resource to create, or <code>null</code> if the resource created should be represented by a blank node.
	 * @param typeNamespaceURI The XML namespace used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @param typeLocalName The XML local name used in the serialization of the type URI, or <code>null</code> if the type is not known.
	 * @return The resource created with this reference URI, with the given type added if a type was given, or <code>null</code> if no suitable resource can be
	 *         created.
	 */
	public RDFResource createResource(final URI referenceURI, final URI typeNamespaceURI, final String typeLocalName) {
		if(DICTO_NAMESPACE_URI.equals(typeNamespaceURI)) { //if this resource is a Dicto resource
			if(DICTIONARY_CLASS_NAME.equals(typeLocalName)) { //dicto:Dictionary
				return new Dictionary(referenceURI); //create and return a new dictionary
			} else if(WORD_CLASS_NAME.equals(typeLocalName)) { //dicto:Word
				return new Word(referenceURI); //create and return a new dictionary word
			}
		}
		return null; //show that we couldn't create a resource
	}

}