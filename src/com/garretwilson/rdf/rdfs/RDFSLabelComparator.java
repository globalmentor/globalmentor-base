package com.garretwilson.rdf.rdfs;

import com.garretwilson.rdf.*;

import java.text.Collator;
import java.util.*;

/**A comparator that compares resources based upon labels, if available.
@author Garret Wilson
*/
public class RDFSLabelComparator implements Comparator
{

	/**Compares two resources to sorts them based upon <code>rdfs:label</code>
		properties, if present.
	@param object1 The first object to compare. This must be a
		<code>RDFResource</code> object.
	@param object2 The second object to compare. This must be a
		<code>RDFResource</code> object.
	@return A negative integer, zero, or a positive integer if the first resource
		label is less than, equal to, or greater than the label of the second
		resource, respectively.
	@exception ClassCastException Thrown if the specified objects' types are not
		<code>RDFResource</code>.
	@see RDFSUtilities#getLabel(RDFResource)
	*/
	public int compare(final Object object1, final Object object2) throws ClassCastException
	{
		final RDFResource resource1=(RDFResource)object1;	//cast the objects to resources
		final RDFResource resource2=(RDFResource)object2;
		final RDFLiteral label1=RDFSUtilities.getLabel(resource1);	//get the labels, if any
		final RDFLiteral label2=RDFSUtilities.getLabel(resource2);
		if(label1!=null && label2!=null)	//if both resources have labels
		{
			if(label1 instanceof RDFPlainLiteral && label2 instanceof RDFPlainLiteral)	//if both labels are plain literals
			{
				Collator collator;	//we'll create a collator to compare the labels, preferably using something locale-specific			
				final RDFPlainLiteral plainLiteral1=(RDFPlainLiteral)label1;	//cast the labels to plain literals
				final RDFPlainLiteral plainLiteral2=(RDFPlainLiteral)label2;
				final Locale language1=plainLiteral1.getLanguage();	//get the language of both literals
				final Locale language2=plainLiteral2.getLanguage();
				if(language1.equals(language2))	//if the languages match, we'll use a collator for that language
				{
					collator=Collator.getInstance(language1);	//get a collator tuned to the language
				}
				else	//if the languages don't match
				{
					collator=Collator.getInstance();	//get a collator using whatever locale we're in (this will ensure that all non-same-language literals will get sorted the same way, regardless of order
				}
				collator.setStrength(Collator.PRIMARY);	//only consider primary differences---not accents, and not case
				collator.setDecomposition(Collator.FULL_DECOMPOSITION);	//fully decompose Unicode characters to get the best comparison
				return collator.compare(plainLiteral1.getLexicalForm(), plainLiteral2.getLexicalForm());	//compare the lexical forms of the two plain literals 
			}
			else	//if both labels are not plain literals
			{
				return label1.getLexicalForm().compareTo(label2.getLexicalForm());	//sort the lexical forms TODO probably use a collator here as well
			}
		}
		else	//if both resources don't have labels
		{
		//G***fix		final Resource otherResource=(Resource)object;	//cast the object to a resource
			if(resource1.getReferenceURI()!=null && resource2.getReferenceURI()!=null)	//if both resources have reference URIs				
				return resource1.getReferenceURI().compareTo(resource2.getReferenceURI()); //compare reference URIs
			else	//if one of the two resources doesn't have a reference URI
				return resource1.hashCode()-resource2.hashCode();	//make an arbitrary comparison G***fix; maybe allow comparison if the objects are Comparable, even if they aren't RDFResources
		}
	}

}
