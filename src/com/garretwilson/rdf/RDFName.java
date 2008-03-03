package com.garretwilson.rdf;

import java.net.URI;

import com.garretwilson.net.DefaultResource;
import com.garretwilson.net.URIConstants;
import com.globalmentor.text.xml.XML;
import com.globalmentor.util.IDed;

import static com.garretwilson.rdf.RDFUtilities.*;

/**The name of an RDF entity, consisting of a namespace URI and a local name.
@author Garret Wilson
*/
public class RDFName extends DefaultResource implements IDed<URI>
{

	/**The namespace URI.*/
	private final URI namespaceURI;

		/**@return The namespace URI.*/
		public URI getNamespaceURI() {return namespaceURI;}

		/**@return The unique identifier of the object.*/
		public URI getID() {return getURI();}

	/**The local name.*/
	private final String localName;

		/**@return The local name.*/
		public String getLocalName() {return localName;}

	/**Namespace URI and local name constructor.
	@param namespaceURI The namespace URI.
	@param localName The local name.
	@exception NullPointerException if the given namespace URI and/or local name is <code>null</code>.
	*/	
	public RDFName(final URI namespaceURI, final String localName)
	{
		super(createReferenceURI(namespaceURI, localName));	//construct the parent class with the reference URI
		this.namespaceURI=namespaceURI;
		this.localName=localName;
	}

	/**Creates an RDF name from the given reference URI.
	For most reference URIs, the namespace URI formed by all the the reference URI characters
	up to and including the last character that is not a valid XML name character,
	and the local name is formed by all the characters after that character.
	If all characters are valid XML name characters, the last non-alphanumeric character is used as a delimiter.
	@param referenceURI The reference URI for which an RDF name should be determined.
	@return The RDF name for the reference URI, or <code>null</code> if the namespace URI and local name could not be determined.
	*/
	public static RDFName getRDFName(final URI referenceURI)
	{
			//TODO do something special for certain namespaces such as for XLink that do not follow the rules
		final String referenceURIString=referenceURI.toString();	//get a string version of the reference URI
		final int referenceURILength=referenceURI.toString().length();	//get the length of the URi string
		int delimiterIndex=-1;	//we'll determine the index of the delimiter; so far, we don't know what it is
		for(int i=referenceURILength-1; i>=0; --i)	//look at each character in the reference URI, starting at the end
		{
			final char character=referenceURIString.charAt(i);	//get this character
			if(!XML.isNameChar(character) && character!=URIConstants.ESCAPE_CHAR)	//if this is not a name character (but it isn't the URI escape character, either)
			{
				delimiterIndex=i;	//record the delimiter index
				break;	//stop looking for a delimtier
			}
		}
		if(delimiterIndex<0)	//if we still don't know the delimiter, look for the last non-alphanumeric character
		{
			for(int i=referenceURILength-1; i>=0; --i)	//look at each character in the reference URI, starting at the end
			{
				if(!Character.isLetter(referenceURIString.charAt(i)) && !Character.isDigit(referenceURIString.charAt(i)))	//if this is not a letter or a number
				{
					delimiterIndex=i;	//record the delimiter index
					break;	//stop looking for a delimtier
				}
			}
		}
		//TODO correctly check for a URI syntax error here
		return delimiterIndex>=0 ? new RDFName(URI.create(referenceURIString.substring(0, delimiterIndex+1)), referenceURIString.substring(delimiterIndex+1)) : null;	//if we found a delimiter, create an RDF name
	}
}
