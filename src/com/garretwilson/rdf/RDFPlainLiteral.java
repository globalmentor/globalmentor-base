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

	/**If <code>object</code> is another <code>RDFLiteral</code>, compares the
		literal values. If <code>object</code> is a <code>String</code>
		compares the string with this literal's value. Otherwise, compares the
		objects using the superclass functionality.
	@param object The object with which to compare this literal; should be
		another literal or a Java string.
	@return <code>true<code> if this literal equals that specified in
		<code>object</code>.
	@see #getLexicalForm
	*/
	public boolean equals(Object object)	//G***do we really want to compare this with a string?
	{
		if(object instanceof RDFLiteral)	//if we're being compared with another literal
		{
			return getLexicalForm().equals(((RDFLiteral)object).getLexicalForm()); //compare values
		}
		else if(object instanceof String)	//if we're being compared with a string
		{
			return getLexicalForm().equals((String)object); //compare our value with the string
		}
		else	//if we're being compared with anything else
			return super.equals(object);	//use the default compare
	}

}