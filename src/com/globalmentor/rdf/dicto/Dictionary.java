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

import com.globalmentor.rdf.*;
import static com.globalmentor.rdf.dicto.Dicto.*;
import com.globalmentor.rdf.dublincore.RDFDublinCore;

/**Class representing a Dictionary Ontology (Dicto) dictionary.
@author Garret Wilson
*/
public class Dictionary extends TypedRDFResource
{

	/**@return The namespace URI of the ontology defining the default type of this resource.*/
	public URI getDefaultTypeNamespaceURI() {return DICTO_NAMESPACE_URI;}

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return DICTIONARY_CLASS_NAME;}

	/**Default constructor.*/
	public Dictionary()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new publication.
	*/
	public Dictionary(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The list of entries for this dictionary.
	@exception ClassCastException if the value of the interactions property
		is not a list resource.
	*/
	public RDFListResource getEntries()
	{
		return (RDFListResource)getPropertyValue(DICTO_NAMESPACE_URI, ENTRIES_PROPERTY_NAME);	//get the dicto:entries property value		
	}

	/**@return The language of the dictionary entries, or <code>null</code> if
		dictionary language is not indicated.
	*/
	public Locale getDictionaryLanguage()
	{
		return Dicto.getLanguage(this);	//return the dictionary language
	}

	/**@return The language of any translations, or <code>null</code> if
		no translation language is indicated. If there are no translations, this
		property should return the same as <code>getDictionaryLanguage()</code>.
	*/
	public Locale getLanguage()
	{
		return RDFDublinCore.getLanguage(this);	//return the language
	}

}
