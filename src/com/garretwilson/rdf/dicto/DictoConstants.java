package com.garretwilson.rdf.dicto;

import java.net.URI;

/**Constants used in Dictionary Ontology (Dicto) processing.
@author Garret Wilson
*/
public interface DictoConstants
{

	/**The recommended prefix to the Dicto ontology namespace.*/
	public final static String DICTO_NAMESPACE_PREFIX="dicto";
	/**The URI to the Dicto namespace.*/
	public final static URI DICTO_NAMESPACE_URI=URI.create("http://globalmentor.com/namespaces/2003/dicto#");

		//Dicto class names
	/**The local name of dicto:Dictionary.*/
	public final static String DICTIONARY_CLASS_NAME="Dictionary";
	/**The local name of dicto:Word.*/
	public final static String WORD_CLASS_NAME="Word";

		//Dicto property names
	/**The form of an entry. The local name of dicto:form.*/
//G***del if not needed	public final static String FORM_PROPERTY_NAME="form";
	/**The definition of an entry. The local name of dicto:definition.*/
	public final static String DEFINITION_PROPERTY_NAME="definition";
	/**The entries of a dictionary. The local name of dicto:entries.*/
	public final static String ENTRIES_PROPERTY_NAME="entries";
	/**The gender of a word. The local name of dicto:gender.*/
	public final static String GENDER_PROPERTY_NAME="gender";
	/**The source language of a dictionary. The local name of dicto:language.*/
	public final static String LANGUAGE_PROPERTY_NAME="language";
	/**The number of an entry. The local name of dicto:number.*/
	public final static String NUMBER_PROPERTY_NAME="number";
	/**The orthography of an entry. The local name of dicto:orthography.*/
	public final static String ORTHOGRAPHY_PROPERTY_NAME="orthography";
	/**The pronunciation of an entry. The local name of dicto:pronunciation.*/
	public final static String PRONUNCIATION_PROPERTY_NAME="pronunciation";
	/**The part of speech of an entry. The local name of dicto:speechPart.*/
	public final static String SPEECH_PART_PROPERTY_NAME="speechPart";
	/**The translation of an entry. The local name of dicto:translation.*/
	public final static String TRANSLATION_PROPERTY_NAME="translation";
	/**The transliteration of an entry. The local name of dicto:transliteration.*/
	public final static String TRANSLITERATION_PROPERTY_NAME="transliteration";

}