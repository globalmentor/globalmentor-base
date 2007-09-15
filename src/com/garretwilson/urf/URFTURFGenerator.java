package com.garretwilson.urf;

import java.io.IOException;
import java.io.Writer;
import java.net.URI;
import java.util.*;

import static com.garretwilson.lang.ObjectUtilities.*;

import com.garretwilson.lang.IntegerUtilities;
import com.garretwilson.net.NamespacePrefixManager;
import com.garretwilson.net.Resource;
import com.garretwilson.rdf.RDFResource;

import static com.garretwilson.text.CharacterConstants.*;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;

import com.garretwilson.util.Debug;
import com.garretwilson.util.IdentityHashSet;
import com.garretwilson.util.NameValuePair;

/**Generates TURF from URF.
@author Garret Wilson
*/
public class URFTURFGenerator
{

	/**The base URI of the URF data model, or <code>null</code> if the base URI is unknown.*/
	private final URI baseURI;
	
		/**@return The base URI of the URF data model, or <code>null</code> if the base URI is unknown.*/
		public URI getBaseURI() {return baseURI;}

	/**The namespace prefix manager.*/
	private final NamespacePrefixManager namespacePrefixManager;

		/**@return The namespace prefix manager.*/
		public NamespacePrefixManager getNamespacePrefixManager() {return namespacePrefixManager;}

	/**The set of resources that have been generated, using identity rather than equality for equivalence, as required be generation.*/
	private final Set<URFResource> generatedResourceSet;

		/**Determines if the given resource has been generated.
		@param resource The resource to check.
		@return <code>true</code> if the resource has already been generated, else <code>false</code>.
		*/
		protected final boolean isGenerated(final URFResource resource)
		{
			return generatedResourceSet.contains(resource);	//see if the resource is in our set
		}

		/**Sets the serialized status of a given resource.
		@param resource The resource for which the generated status should be set.
		@param generated <code>true</code> if the resource has been generated, or <code>false</code> if not.
		*/
		protected final void setGenerated(final URFResource resource, final boolean generated)
		{
			if(generated)	//if the resource has been generated
			{
				generatedResourceSet.add(resource);	//add the resource to the set of generated resources
			}
			else	//if the resource has not been generated
			{
				generatedResourceSet.remove(resource);	//remove the resource from the set of generated resources
			} 
		}

	/**A map that associates, for each resource, a set of all resources that reference the that resource, using identity rather than equality for equivalence.*/
	private final Map<URFResource, Set<URFResource>> resourceReferenceMap;

		/**Returns the set of resources that reference this resource as already calculated.
		@param resource The resource for which references should be returned.
		@return A set of all references to the resource that have been gathered at an earlier time, or <code>null</code> if no references have been gathered for the given resource.
		*/
		protected Set<URFResource> getReferenceSet(final RDFResource resource)	//TODO change to a set map to get rid of the null case
		{
			return resourceReferenceMap.get(resource);	//get the set of references, if any, associated with the resource
		}

	/**A map of label strings keyed to the resource they represent, using identity rather than equality for equivalence for comparing resources.*/
	private final Map<URFResource, String> resourceLabelMap;

		/**Retrieves a label appropriate for the given resource.
		If the resource has already been assigned a label, it will be returned; otherwise, a new label will be generated.
		@param resource The resource for which a node ID should be returned.
		@return A label to represent the given resource
		*/
		protected String getLabel(final URFResource resource)
		{
			String label=resourceLabelMap.get(resource);	//get a label for the given resource
			if(label==null)	//if there is no label for this resource
			{
				label="resource"+generatedResourceSet.size()+1;	//generate a label for the resource, based upon the number of resources already generated
				resourceLabelMap.put(resource, label);	//associate the label with the resource
			}
			return label;	//return the retrieved or generated label
		} 

	/**Whether output is formatted.*/
	private boolean formatted;

		/**@return Whether output is formatted.*/
		public boolean isFormatted() {return formatted;}

		/**Sets whether output is formatted.
		@param formatted Whether output is formatted.
		*/
		public void setFormatted(final boolean formatted) {this.formatted=formatted;}

	/**Whether types are generated as short forms.*/
	private boolean shortTypesGenerated=true;

		/**@return Whether types are generated as short forms.*/
		public boolean isShortTypesGenerated() {return shortTypesGenerated;}

		/**Sets whether types should be generated as short forms.
		@param shortTypesGenerated Whether types should be generated as short forms.
		*/
		public void setShortTypesGenerated(final boolean shortTypesGenerated) {this.shortTypesGenerated=shortTypesGenerated;}

	/**Whether arrays are generated as short forms.*/
	private boolean shortArraysGenerated=true;

		/**@return Whether arrays are generated as short forms.*/
		public boolean isShortArraysGenerated() {return shortArraysGenerated;}

		/**Sets whether arrays should be generated as short forms.
		@param shortArraysGenerated Whether arrays should be generated as short forms.
		*/
		public void setShortArraysGenerated(final boolean shortArraysGenerated) {this.shortArraysGenerated=shortArraysGenerated;}

	/**The zero-based level of text indentation.*/
	private int indentLevel=0;

		/**@return The zero-based level of text indentation.*/
		public int getIndentLevel() {return indentLevel;}

		/**Sets the level of text indentation.
		@param newIndentLevel The new zero-based level of text indention.
		*/
		public void setIndentLevel(final int newIndentLevel)
		{
			indentLevel=newIndentLevel;	//actually change the value
		}

		/**Changes the indent level by the given amount.
		@param indentDelta The amount by which to increase or decrease the indent level.
		@see #getIndentLevel()
		@see #setIndentLevel(int)
		*/
		public void indent(final int indentDelta)
		{
			setIndentLevel(getIndentLevel()+indentDelta);	//change the indention amount by the given delta
		}

		/**Increments the indent level.
		@see #indent(int)
		*/
		public void indent()
		{
			indent(1);	//indent by 1
		}

		/**Decrements the indent level.
		@see #indent(int)
		*/
		public void unindent()
		{
			indent(-1);	//indent by -1
		}

	/**Default constructor with no base URI and formatted serialization.*/
	public URFTURFGenerator()
	{
		this(null);	//construct the class with no base URI
	}

	/**Base URI constructor with formatted generation.
	@param baseURI The base URI of the URF data model, or <code>null</code> if the base URI is unknown.
	*/
	public URFTURFGenerator(final URI baseURI)
	{
		this(baseURI, true);	//construct a formatted generator
	}

	/**Base URI and formatted constructor.
	@param baseURI The base URI of the RDF data model, or <code>null</code> if the base URI is unknown.
	@param formatted Whether output is formatted.
	*/
	public URFTURFGenerator(final URI baseURI, final boolean formatted)
	{
		this(baseURI, formatted, new NamespacePrefixManager());	//create the class with a default namespace prefix manager
	}

	/**Base URI, formatted, and namespace prefix manager constructor.
	@param baseURI The base URI of the RDF data model, or <code>null</code> if the base URI is unknown.
	@param formatted Whether output is formatted.
	@param namespacePrefixManager The manager of namespaces and prefixes.
	@excepion NullPointerException if the given namespace prefix manager is <code>null</code>.
	*/
	public URFTURFGenerator(final URI baseURI, final boolean formatted, final NamespacePrefixManager namespacePrefixManager)
	{
		this.baseURI=baseURI;
		this.formatted=formatted;
		this.namespacePrefixManager=checkInstance(namespacePrefixManager, "Namespace prefix manager cannot be null.");
		generatedResourceSet=new IdentityHashSet<URFResource>();	//create a map that will determine whether resources have been generated, based upon the identity of resources
		resourceReferenceMap=new IdentityHashMap<URFResource, Set<URFResource>>();	//create a map of sets of referring resources for each referant resource, using identity rather than equality for equivalence
		resourceLabelMap=new IdentityHashMap<URFResource, String>();	//create a map of node IDs keyed to resources, using identity rather than equality to determine associated resource
	}

	/**Initializes the generator by resetting values and initializing the namespace prefixes.
	@see #reset()
	*/ 
	protected void initialize()
	{
		reset();	//reset the generator
	}

	/**Releases memory by clearing all internal maps and sets of resources.*/ 
	public void reset()
	{
		generatedResourceSet.clear();	//show that we've not generated any resources
		resourceReferenceMap.clear();	//clear all our references to resources
		resourceLabelMap.clear();	//clear our map of node IDs
	}

	/*
	@param scopeSubject The base resource of the current scope, or <code>null</code> if the current resource is not in an object context.
	@param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value, or <code>null</code> if there is no current scope.
	@param scopePredicate The predicate for which the new resource is a value, or <code>null</code> if the current resource is not in an object context.
*/
	public Writer generate(final Writer writer, final URFResource resource) throws IOException
	{
		return generate(writer, null, null, resource, false);
	}

	/*
	@param scopeSubject The base resource of the current scope, or <code>null</code> if the current resource is not in an object context.
	@param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value, or <code>null</code> if there is no current scope.
	@param scopePredicate The predicate for which the new resource is a value, or <code>null</code> if the current resource is not in an object context.
	@param inSequence Whether the resource being generated is in a sequence and its scoped order properties should therefore not be generated.
*/
	public Writer generate(final Writer writer, final URFScope scopeSubject, final URI scopePredicateURI, final URFResource resource, final boolean inSequence) throws IOException
	{
Debug.trace("generating resource with scope", scopeSubject, "predicate URI", scopePredicateURI, "and resource", resource);
		boolean generatedComponent=false;	//we haven't generated any components, yet
		URI lexicalTypeURI=null;	//the lexical namespace type URI, if any
		final boolean isArrayShortForm=isShortArraysGenerated() && resource.hasType(ARRAY_CLASS_URI);	//see if this is an array to be generated in short form
		final boolean isShortTypesGenerated=isShortTypesGenerated();	//see if we should generate types in the short form
			//reference
		final URI uri=resource.getURI();	//get the resource URI
		if(uri!=null)
		{
Debug.trace("got resource URI:", uri);
			if(isLexicalNamespaceURI(uri))	//if this URI is in a lexical namespace
			{
				lexicalTypeURI=getLexicalNamespaceTypeURI(uri);	//get the lexical type of the URI so that we don't generate it again
			}
			writeReference(writer, uri);	//write a reference for the resource
			generatedComponent=true;	//indicate that we generated a component
		}
			//type
		if(isShortTypesGenerated)	//if we should generate type short forms
		{
			int shortTypeCount=0;	//keep track of how many short-form types we've generated
			for(final URFResource type:resource.getTypes())	//look at each type
			{
				final URI typeURI=type.getURI();	//get the URI of this type
				if(type!=null)	//if a type was given
				{
					if(typeURI.equals(lexicalTypeURI)	//if this is the same type URI as the URI included in the lexical namespace type URI, if any
							|| (isArrayShortForm && ARRAY_CLASS_URI.equals(typeURI)))	//or if we're using an array short form and this is the array type
					{
						continue;	//skip this type
					}
				}
				if(shortTypeCount>0)	//if we've already generated a type
				{
					writer.append(LIST_DELIMITER);	//separate the types
				}
				else	//if we haven't yet started the properties section
				{
					writer.write(TYPE_BEGIN);	//start the type declaration
				}
				generate(writer, resource, TYPE_PROPERTY_URI, type, false);	//write the type
				++shortTypeCount;	//show that we've got another short type
			}
			if(shortTypeCount>0)	//if we generated any short form types
			{
				writer.write(TYPE_END);	//end the type declaration
				generatedComponent=true;	//indicate that we generated a component			
			}
		}
			//array
		if(isArrayShortForm)	//if we should generate an array short form
		{
			final Iterator<URFProperty> elementPropertyIterator=resource.getNamespaceProperties(INTEGER_NAMESPACE_URI).iterator();	//get an iterator to all the integer properties
			if(elementPropertyIterator.hasNext())	//if there are array elements
			{
				writeNewLine(writer);
				writer.write(ARRAY_BEGIN);	//start the array
				indent();	//indent the array
				writeNewLine(writer);
				int elementCount=0;	//keep track of how many elements we have
				while(elementPropertyIterator.hasNext())	//while there are elements
				{
					final URFProperty elementProperty=elementPropertyIterator.next();	//get the next element property
					if(elementCount>0)	//if we've already generated an element
					{
						writer.append(LIST_DELIMITER);	//separate the properties
						writeNewLine(writer);
					}
					generate(writer, elementProperty.getSubjectScope(), elementProperty.getPropertyURI(), elementProperty.getValue(), false);	//generate the element
					++elementCount;	//show that we generated another array element
				}
				unindent();
				writeNewLine(writer);
				writer.write(ARRAY_END);	//end the array
			}
			else	//if there are no elements in the array
			{
				writer.append(ARRAY_BEGIN).append(ARRAY_END);	//show the empty array on the same line
			}
			generatedComponent=true;	//indicate that we generated a component			
		}
			//properties
		int propertyCount=0;	//start with no properties being generating
		propertyCount=generateProperties(writer, resource, PROPERTY_VALUE_DELIMITER, propertyCount, !isShortTypesGenerated, !isArrayShortForm, true);	//generate properties
		if(scopeSubject!=null && scopePredicateURI!=null)	//if this resource is the value of a property
		{
			final URFScope scope=scopeSubject.getScope(scopePredicateURI, resource);	//get the scope for this value
			if(scope==null)	//if there is no such scope
			{
				throw new IllegalArgumentException("No scope for given subject "+scopeSubject+" and predicate URI "+scopePredicateURI);
			}
			propertyCount=generateProperties(writer, scope, SCOPED_PROPERTY_VALUE_DELIMITER, propertyCount, !isShortTypesGenerated, !isArrayShortForm, !inSequence);	//generate scoped properties, suppressing generation of scoped order if we are in a sequence
		}
		if(propertyCount>0)	//if we started the properties section
		{
			unindent();
			writeNewLine(writer);
			writer.write(PROPERTIES_END);	//end the properties declaration
		}
		else if(!generatedComponent)	//if we haven't generated any components, have a properties section even if it's empty
		{
			writer.append(PROPERTIES_BEGIN).append(PROPERTIES_END);	//write an empty properties declaration
		}
		return writer;	//return the writer
	}

	/**Generates the properties, if any, of a given scope.
	@param writer The writer used for generating the information.
	@param scope The scope the properties of which should be generated.
	@param propertyValueDelimiter The delimiter to use to separate properties and values.
	@param propertyCount the number of properties already generated; used to determine whether a new properties section should be generated.
	@param generateTypes Whether type properties should be generated.
	@param generateIntegers Whether properties in the integer namespace should be generated.
	@param generateOrder Whether the order property should be generated.
	@return The new total number of properties generated, including the properties already generated before this method was called.
	@exception NullPointerException if the given writer and/or scope is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	*/
	protected int generateProperties(final Writer writer, final URFScope scope, final char propertyValueDelimiter, int propertyCount, final boolean generateTypes, final boolean generateIntegers, final boolean generateOrder) throws IOException
	{

		URI sequencePropertyURI=null;	//this will indicate when we're in the middle of a sequence for a particular property
		
		for(final URFProperty property:scope.getProperties())	//look at each property
		{
			final URFResource value=property.getValue();	//get the property value
			final URI valueURI=value.getURI();	//get the URI of the value, if any
			final URI propertyURI=property.getPropertyURI();	//get the property URI
			if((!generateTypes && TYPE_PROPERTY_URI.equals(propertyURI))	//if we shouldn't generate types and this is a type
				|| (!generateIntegers && valueURI!=null && INTEGER_NAMESPACE_URI.equals(getNamespaceURI(valueURI)))	//or if we shouldn't generate integers and this is an integer value
				|| (!generateOrder && ORDER_PROPERTY_URI.equals(propertyURI)))	//or if we shouldn't generate order and this is an order property
			{
				continue;	//skip this property
			}
				//that this implementatoin automatically sorts scoped ordered properties with the ordered properties ahead of non-ordered properties
				//allows this algorithm to ignore actual sequence values and rest assured that a property will not contain two incompatible sequences separated by non-ordered properties
			if(sequencePropertyURI!=null)	//if we're in the middle of a sequence, see if we should end it
			{
				if(!sequencePropertyURI.equals(propertyURI)	//we're changing to another property
						|| property.getScope().getOrder()==null)	//or if we're still looking at the same property but we no longer have a scoped order
				{
					writer.write(SEQUENCE_END);	//end the sequence for this property
					sequencePropertyURI=null;	//indicate that we are no longer in a sequence for a property					
				}
			}
			if(sequencePropertyURI==null)	//if we're not in the middle of a sequence
			{
				if(propertyCount>0)	//if we've already generated a property
				{
					writer.append(LIST_DELIMITER);	//separate the properties
					writeNewLine(writer);
				}
				else	//if we haven't yet started the properties section
				{
					writeNewLine(writer);
					writer.write(PROPERTIES_BEGIN);	//start the properties declaration
					indent();	//indent the properties
					writeNewLine(writer);
				}
				writeReference(writer, propertyURI);	//generate the reference of the property
				writer.append(propertyValueDelimiter);	//=/~
				if(property.getScope().getOrder()!=null)	//if this property has a sequence
				{
					writer.write(SEQUENCE_BEGIN);	//start a sequence for this property
					sequencePropertyURI=propertyURI;	//indicate that we should have a sequence for this property
					generate(writer, scope, propertyURI, value, true);	//generate the property value, indicating that the value is an element in a sequence
				}
				else	//if this property has no sequence
				{
					generate(writer, scope, propertyURI, value, false);	//generate the property value normally
				}
			}
			else	//if we are still in the middle of a sequence
			{
				writer.append(LIST_DELIMITER);	//separate the values in the sequence
				generate(writer, scope, propertyURI, value, true);	//generate the property value, indicating that the value is an element in a sequence
			}
			++propertyCount;	//show that we generated another property
		}
		return propertyCount;	//return the new property count
	}

	/**Writes a reference to a resource with the given URI.
	A short form will be used if appropriate.
	@param writer The writer used for generating the information.
	@param uri The URI of the resource.
	@exception NullPointerException if the given writer and/or URI is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	*/
	public static void writeReference(final Writer writer, final URI uri) throws IOException
	{
		if(isLexicalNamespaceURI(uri))	//if this URI is in a lexical namespace
		{
			final URI lexicalTypeURI=getLexicalNamespaceTypeURI(uri);	//get the lexical type of the URI so that we don't generate it again
Debug.trace("lexical resource with type:", lexicalTypeURI);
			if(BOOLEAN_CLASS_URI.equals(lexicalTypeURI))	//boolean
			{
				writer.append(BOOLEAN_BEGIN).append(uri.getFragment()).append(BOOLEAN_END);	//write the boolean short form
			}
			else if(INTEGER_CLASS_URI.equals(lexicalTypeURI) || REAL_CLASS_URI.equals(lexicalTypeURI))	//integer or real
			{
				writer.append(NUMBER_BEGIN).append(uri.getFragment()).append(NUMBER_END);	//write the number short form
			}
			else if(STRING_CLASS_URI.equals(lexicalTypeURI))	//if this is a string
			{
				writeString(writer, uri.getFragment(), STRING_BEGIN, STRING_END);	//write the string short form
			}
			else	//if this is not a short-form lexical namespace
			{
				writeURIReference(writer, uri);	//generate the URI reference normally
			}
		}
		else	//for all other resources
		{
			writeURIReference(writer, uri);	//generate the URI reference
		}		
	}

	/**Writes a URI reference to a resource with the given URI.
	@param writer The writer used for generating the information.
	@param uri The URI of the resource.
	@exception NullPointerException if the given writer and/or URI is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	*/
	public static void writeURIReference(final Writer writer, final URI uri) throws IOException
	{
		writer.append(REFERENCE_BEGIN).append(uri.toString()).append(REFERENCE_END);	//write the reference URI
	}

	/**Writes a string surrounded by string delimiters.
	The usual reserved string characters, along with the string delimiters, will be escaped.
	@param writer The writer used for generating the information.
	@param string The string to write.
	@param stringBegin The beginning string delimiter.
	@param stringEnd The ending string delimiter.
	@exception NullPointerException if the given writer and/or string is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	*/
	public static void writeString(final Writer writer, final String string, final char stringBegin, final char stringEnd) throws IOException
	{
		writer.write(stringBegin);	//write the string beginning delimiter
		final int length=string.length();	//get the length of the string
		for(int i=0; i<length; ++i)	//for each character index
		{
			char c=string.charAt(i);	//get this character
			boolean escaped=false;	//see if the character needs to be escaped
			switch(c)	//see which character this is TODO check for surrogates
			{
				case STRING_ESCAPE:	//escape character
					escaped=true;	//show that we should escape the character
					break;
				case BACKSPACE_CHAR:	//backspace
					escaped=true;	//show that we should escape the character
					c=ESCAPED_BACKSPACE;	//use \b for backspace
					break;
				case FORM_FEED_CHAR:	//form feed
					escaped=true;	//show that we should escape the character
					c=ESCAPED_FORM_FEED;	//use \f for form feed
					break;
				case LINE_FEED_CHAR:	//line feed
					escaped=true;	//show that we should escape the character
					c=ESCAPED_LINE_FEED;	//use \n for line feed
					break;
				case CARRIAGE_RETURN_CHAR:	//carriage return
					escaped=true;	//show that we should escape the character
					c=ESCAPED_CARRIAGE_RETURN;	//use \r for carriage return
					break;
				case HORIZONTAL_TABULATION_CHAR:	//tab
					escaped=true;	//show that we should escape the character
					c=ESCAPED_TAB;	//use \t for tab
					break;
				default:
					if(c==stringBegin || c==stringEnd)	//if this is one of the string delimiters
					{
						escaped=true;	//show that we should escape the character
					}
					break;
			}
			if(escaped)	//if we should escape this character
			{
				writer.write(STRING_ESCAPE);	//write an escape character
				writer.write(c);	//write the character normally
			}
			else if(Character.isISOControl(c))	//if this is a control character
			{
				writer.append(STRING_ESCAPE).append(ESCAPED_UNICODE).append(IntegerUtilities.toHexString(c, 4));	//append a Unicode escaped version of the character
			}
			else	//if this character is not escaped and not a control character
			{
				writer.write(c);	//write the character normally					
			}
		}
		writer.write(stringEnd);	//write the string ending delimiter
	}

	/**Writes the end of a line and then indents at the current indent level.
	If the generator is not formatted, no action occurs.
	@param writer The writer used for generating the information.
	@exception NullPointerException if the given writer is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	@see #isFormatted()
	@see #getIndentLevel()
	*/
	public void writeNewLine(final Writer writer) throws IOException
	{
		if(isFormatted())	//if we should format the output
		{
			writer.write(LINE_FEED_CHAR);	//newline
			for(int i=getIndentLevel(); i>0; --i)	//for each indention
			{
				writer.write(HORIZONTAL_TABULATION_CHAR);	//write a tab
			}
		}
	}

}
