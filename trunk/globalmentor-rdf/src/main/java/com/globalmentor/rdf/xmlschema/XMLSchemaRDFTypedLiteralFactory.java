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

package com.globalmentor.rdf.xmlschema;

import java.net.*;

import com.globalmentor.rdf.*;

import static com.globalmentor.text.xml.schema.XMLSchema.*;

/**
 * A factory to create typed literals within the XML schema namespace.
 * @author Garret Wilson
 */
public class XMLSchemaRDFTypedLiteralFactory implements RDFTypedLiteralFactory {

	/**
	 * Creates a typed literal from the provided lexical form based upon the datatype reference URI.
	 * <p>
	 * This implementation supports the following XML Schema data types, creating the indicated RDF typed literal classes:
	 * </p>
	 * <dl>
	 * <dt><code>http://www.w3.org/2001/XMLSchema#boolean</code></dt>
	 * <dd><code>BooleanLiteral</code></dd>
	 * </dl>
	 * @param lexicalForm The lexical form of the resource.
	 * @param datatypeURI The datatype reference URI of the datatype.
	 * @return A typed literal containing a value object mapped from the given lexical form and datatype
	 * @throws IllegalArgumentException if the given lexical form is not in the correct format to be interpreted as the given type.
	 */
	public RDFTypedLiteral<?> createTypedLiteral(final String lexicalForm, final URI datatypeURI) {
		if(BASE64_BINARY_DATATYPE_URI.equals(datatypeURI)) { //base64-encoded binary
			return new Base64BinaryLiteral(lexicalForm); //create and return a base64-encoded binary literal
		} else if(BOOLEAN_DATATYPE_URI.equals(datatypeURI)) { //boolean
			return new BooleanLiteral(lexicalForm); //create and return a boolean literal
		}
		//TODO add support for decimal
		else if(DOUBLE_DATATYPE_URI.equals(datatypeURI)) { //double
			return new DoubleLiteral(lexicalForm); //create and return a double literal
		} else if(FLOAT_DATATYPE_URI.equals(datatypeURI)) { //float
			return new FloatLiteral(lexicalForm); //create and return a float literal
		} else if(INTEGER_DATATYPE_URI.equals(datatypeURI)) { //integer
			return new IntegerLiteral(lexicalForm); //create and return an integer literal
		} else if(STRING_DATATYPE_URI.equals(datatypeURI)) { //string
			return new StringLiteral(lexicalForm); //create and return a string literal
		} else if(URI_DATATYPE_URI.equals(datatypeURI)) { //URI
			try {
				return new URILiteral(lexicalForm); //create and return a URI literal
			} catch(final URISyntaxException uriSyntaxException) { //if the URI is not syntactically correct
				return null; //we can't create the typed literal TODO provide better error
			}
		} else { //if we don't recognize the type
			return null; //show that we don't recognize the datatype URI 
		}
	}
}