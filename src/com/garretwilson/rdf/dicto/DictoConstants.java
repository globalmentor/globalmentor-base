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
	/**The local name of dicto:Activity.*/
	public final static String WORD_CLASS_NAME="Word";

		//Dicto property names
	/**The form of an entry. The local name of dicto:form.*/
	public final static String FORM_PROPERTY_NAME="form";
	/**The number of an entry. The local name of dicto:number.*/
	public final static String NUMBER_PROPERTY_NAME="number";
	/**The orthography of an entry. The local name of dicto:number.*/
	public final static String ORTHOGRAPHY_PROPERTY_NAME="orthography";
	/**The part of speech of an entry. The local name of dicto:speechPart.*/
	public final static String SPEECH_PART_PROPERTY_NAME="speechPart";
	/**The translation of an entry. The local name of dicto:translation.*/
	public final static String TRANSLATION_PROPERTY_NAME="translation";
	/**The transliteration of an entry. The local name of dicto:transliteration.*/
	public final static String TRANSLITERATION_PROPERTY_NAME="transliteration";

}