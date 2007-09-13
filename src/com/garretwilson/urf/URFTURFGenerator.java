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
		return generate(writer, null, null, resource);
	}

	/*
	@param scopeSubject The base resource of the current scope, or <code>null</code> if the current resource is not in an object context.
	@param scopeChain The chain of scope, each element representing a property and value to serve as scope for the subsequent property and value, or <code>null</code> if there is no current scope.
	@param scopePredicate The predicate for which the new resource is a value, or <code>null</code> if the current resource is not in an object context.
*/
	public Writer generate(final Writer writer, final URFScope scopeSubject, final URI scopePredicateURI, final URFResource resource) throws IOException
	{
Debug.trace("generating resource with scope", scopeSubject, "predicate URI", scopePredicateURI, "and resource", resource);
		boolean generatedComponent=false;	//we haven't generated any components, yet
		URI lexicalTypeURI=null;	//the lexical namespace type URI, if any
//TODO del		URI shortTypeURI=null;	//the short form type URI, if any, that has been generated separate from any properties declaration
			//reference
		final URI uri=resource.getURI();	//get the resource URI
		if(uri!=null)
		{
			if(isLexicalNamespaceURI(uri))	//if this URI is in a lexical namespace
			{
				lexicalTypeURI=getLexicalNamespaceTypeURI(uri);	//get the lexical type of the URI so that we don't generate it again
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
					writeReference(writer, uri);	//generate the reference URI normally
				}
			}
			else	//for all other resources
			{
				writeReference(writer, uri);	//generate the reference URI
			}
			generatedComponent=true;	//indicate that we generated a component
		}
			//type
		boolean startedTypes=false;	//keep track of whether we have started types
		int shortTypeCount=0;	//keep track of how many short-form types we've generated
		for(final URFResource type:resource.getTypes())	//look at each type
		{
			final URI typeURI=type.getURI();	//get the URI of this type
			if(typeURI==null || !typeURI.equals(lexicalTypeURI))	//if this type doesn't have a URI, or it's not the same URI included in the lexical namespace type URI, if any
			{
				if(shortTypeCount>0)	//if we've already generated a type
				{
					writer.append(LIST_DELIMITER);	//separate the types
				}
				else	//if we haven't yet started the properties section
				{
					writer.write(TYPE_BEGIN);	//start the type declaration
				}
				generate(writer, resource, TYPE_PROPERTY_URI, type);	//write the type
				++shortTypeCount;	//show that we've got another short type
			}
		}
		if(shortTypeCount>0)	//if we generated any short form types
		{
			writer.write(TYPE_END);	//end the type declaration
			generatedComponent=true;	//indicate that we generated a component			
		}
			//properties
		int propertyCount=0;	//start with no properties being generating
		propertyCount=generateProperties(writer, resource, PROPERTY_VALUE_DELIMITER, propertyCount, false);	//generate properties
		if(scopeSubject!=null && scopePredicateURI!=null)
		{
			final URFScope scope=scopeSubject.getScope(scopePredicateURI, resource);
			if(scope==null)
			{
				throw new IllegalArgumentException("No scope for given subject "+scopeSubject+" and predicate URI "+scopePredicateURI);
			}
			propertyCount=generateProperties(writer, scope, SCOPED_PROPERTY_VALUE_DELIMITER, propertyCount, false);	//generate scoped properties
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
	@return The new total number of properties generated, including the properties already generated before this method was called.
	@exception NullPointerException if the given writer and/or scope is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	*/
	public int generateProperties(final Writer writer, final URFScope scope, final char propertyValueDelimiter, int propertyCount, final boolean generateTypes) throws IOException
	{
		for(final URI propertyURI:scope.getPropertyURIs())	//look at each property URI
		{
			if(generateTypes || !TYPE_PROPERTY_URI.equals(propertyURI))	//if we should generate types or this isn't a type
			{
				for(final URFResource propertyValue:scope.getPropertyValues(propertyURI))	//look at each property value
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
					writeReference(writer, propertyURI);	//generate the reference URI of the property
					writer.append(propertyValueDelimiter);	//=/~
					generate(writer, scope, propertyURI, propertyValue);	//generate the property value
					++propertyCount;	//show that we generated another property
				}
			}
		}
		return propertyCount;	//return the new property count
	}

	/**Writes a reference to a resource with the given URI.
	@param writer The writer used for generating the information.
	@param uri The URI of the resource.
	@exception NullPointerException if the given writer and/or URI is <code>null</code>. 
	@exception IOException if there was an error writing to the writer.
	*/
	public static void writeReference(final Writer writer, final URI uri) throws IOException
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
	public void writeString(final Writer writer, final String string, final char stringBegin, final char stringEnd) throws IOException
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
