package com.garretwilson.rdf;

import java.util.Locale;

/**Represents a plain RDF literal.
@author Garret Wilson
*/
public class RDFPlainLiteral extends RDFLiteral
{

	/**The lexical form, a Unicode string in Normal Form C.*/
	private final String lexicalForm;

		/**@return The lexical form, a Unicode string in Normal Form C.*/
		public String getLexicalForm() {return lexicalForm;}

	/**The language of the plain literal, or <code>null</code> if there is no
		language specified.
	*/
	private Locale language;

		/**The language of the plain literal, or <code>null</code> if there is no
			language specified.
		*/
		public Locale getLanguage() {return language;}

	/**Constructs a plain literal with a lexical value.
	@param lexicalValue The lexical form of the literal.
	*/
	public RDFPlainLiteral(final String lexicalValue)
	{
		this(lexicalValue, null);	//construct a plain literal with no language specified
	}

	/**Constructs a plain literal with a lexical value and a language.
	@param lexicalValue The lexical form of the literal.
	@param languageLocale A locale representing the language, or <code>null</code>
		if no language should be specified.
	*/
	public RDFPlainLiteral(final String lexicalValue, final Locale languageLocale)
	{
		lexicalForm=lexicalValue; //set the lexical form
		language=languageLocale;	//set the language locale
	}
}