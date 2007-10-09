package com.garretwilson.urf;

import java.io.IOException;
import java.io.Writer;
import java.net.URI;
import java.util.*;
import static java.util.Collections.*;
import java.util.concurrent.atomic.AtomicLong;

import static com.garretwilson.lang.ObjectUtilities.*;
import com.garretwilson.lang.IntegerUtilities;
import com.garretwilson.net.NamespacePrefixManager;
import static com.garretwilson.net.URIConstants.*;
import static com.garretwilson.net.URIUtilities.*;
import static com.garretwilson.text.CharacterConstants.*;
import static com.garretwilson.urf.URF.*;
import static com.garretwilson.urf.TURF.*;
import com.garretwilson.util.*;
import static com.garretwilson.util.CollectionUtilities.*;

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

	/**The set of resources that have been generated.*/
	private final Set<URFResource> generatedResourceSet;

		/**Determines the number of resources that have been generated.
		@return The number of resources that have been generated.
		*/
		protected final int getGeneratedResourceCount()
		{
			return generatedResourceSet.size();	//return the size of the generated resource set
		}

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

	/**A map of label strings keyed to the resource they represent.*/
	private final Map<URFResource, String> resourceLabelMap;

	/**The atomic variable used to generate labels.*/
	private final AtomicLong generatedLabelCount=new AtomicLong(0);

		/**Generates a new label unique to this generator.
		@return A new label unique to this generator.
		*/
		private String generateLabel()
		{
			return "resource"+generatedLabelCount.incrementAndGet();	//atomically get the next counter value and use it in generating a new label
		}

		/**Retrieves the label associated with the given resource.
		@param resource The resource for which a label should be returned.
		@return A label to represent the given resource, or <code>null</code> if no label has been associated with the given resource.
		*/
		protected String getLabel(final URFResource resource)
		{
			return resourceLabelMap.get(resource);	//get a label, if any, for the given resource
		}

		/**Retrieves a label appropriate for the given resource, creating one if necessary.
		If the resource has already been assigned a label, it will be returned; otherwise, a new label will be generated.
		For namespace URI resources, a namespace prefix will be generated as the label.
		@param resource The resource for which a label should be returned.
		@return A label to represent the given resource.
		*/
		protected String determineLabel(final URFResource resource)
		{
			String label=getLabel(resource);	//get a label, if any, for the given resource
			if(label==null)	//if there is no label for this resource
			{
/*TODO del when works
				final URI uri=asURI(resource);	//get the resource as a URI if possible
				if(uri!=null && getLocalName(uri)==null)	//if the resource is a namespace URI
				{
					label=getNamespacePrefixManager().getNamespacePrefix(uri);	//get a namespace prefix for the URI
				}
				else	//if this is not a namespace URI
				{
*/
					label=generateLabel();	//generate a label for the resource
//TODO del when works				}
				resourceLabelMap.put(resource, label);	//associate the label with the resource
			}
			return label;	//return the retrieved or generated label
		}

		/**Retrieves a label appropriate for the given namespace URI resource, creating one if necessary.
		If the resource has already been assigned a label, it will be returned; otherwise, a new label will be generated.
		@param resource The resource for which a label should be returned.
		@return A label to represent the given namespace URI resource.
		@exception IllegalArgumentException if the given resource is not a URI.
		*/
		protected String determineNamespaceURILabel(final URFResource resource)
		{
			final URI uri=asURI(resource);	//get the resource as a URI if possible
			if(uri==null)	//if the resource is not a URI
			{
				throw new IllegalArgumentException("Resource "+resource+" is not a URI.");
			}
			String label=getLabel(resource);	//get a label, if any, for the given resource
			if(label==null)	//if there is no label for this resource
			{
				label=getNamespacePrefixManager().getNamespacePrefix(uri);	//get a namespace prefix for the URI
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

	/**Whether prefixes are suppressed for inherited namespaces.*/
	private boolean inheritedNamespacePrefixesSuppressed=true;

		/**@return Whether prefixes are suppressed for inherited namespaces.*/
		public boolean isInheritedNamespacePrefixesSuppressed() {return inheritedNamespacePrefixesSuppressed;}

		/**Sets whether prefixes are suppressed for inherited namespaces.
		@param inheritedNamespacePrefixesSuppressed Whether prefixes should be suppressed for inherited namespaces.
		*/
		public void setInheritedNamespacePrefixes(final boolean inheritedNamespacePrefixesSuppressed) {this.inheritedNamespacePrefixesSuppressed=inheritedNamespacePrefixesSuppressed;}

	/**Whether types are generated as short forms.*/
	private boolean shortTypesGenerated=true;

		/**@return Whether types are generated as short forms.*/
		public boolean isShortTypesGenerated() {return shortTypesGenerated;}

		/**Sets whether types should be generated as short forms.
		@param shortTypesGenerated Whether types should be generated as short forms.
		*/
		public void setShortTypesGenerated(final boolean shortTypesGenerated) {this.shortTypesGenerated=shortTypesGenerated;}

	/**Whether lists are generated as short forms.*/
	private boolean shortListsGenerated=true;

		/**@return Whether lists are generated as short forms.*/
		public boolean isShortListsGenerated() {return shortListsGenerated;}

		/**Sets whether lists should be generated as short forms.
		@param shortListsGenerated Whether lists should be generated as short forms.
		*/
		public void setShortListsGenerated(final boolean shortListsGenerated) {this.shortListsGenerated=shortListsGenerated;}

	/**Whether sets are generated as short forms.*/
	private boolean shortSetsGenerated=true;

		/**@return Whether sets are generated as short forms.*/
		public boolean isShortSetsGenerated() {return shortSetsGenerated;}

		/**Sets whether sets should be generated as short forms.
		@param shortSetsGenerated Whether sets should be generated as short forms.
		*/
		public void setShortSetsGenerated(final boolean shortSetsGenerated) {this.shortSetsGenerated=shortSetsGenerated;}

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
		generatedResourceSet=new HashSet<URFResource>();	//create a map that will determine whether resources have been generated
		resourceLabelMap=new HashMap<URFResource, String>();	//create a map of node IDs keyed to resources
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
		resourceLabelMap.clear();	//clear our map of node IDs
	}

	/**Generates all the resources within a given data model.
	@param writer The writer used for generating the information.
	@param urf The data model of which resources should be generated.
	@return The writer.
	@exception NullPointerException if the given writer and/or URF data model is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	public Writer generateResources(final Writer writer, final URF urf) throws IOException
	{
		return generateResources(writer, urf, null);	//generate all URF resources with no particular resource as the primary resource
	}

	/**Generates the given resources and all related resources.
	@param writer The writer used for generating the information.
	@param resources The resources to generate, with the first resource, if any, being the resource to appear first or immediately after namespace descriptions
	@return The writer.
	@exception NullPointerException if the given writer and/or resources is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	public Writer generateResources(final Writer writer, final URFResource... resources) throws IOException
	{
		final URF urf=new URF();	//create a new URF data model
		for(final URFResource resource:resources)	//for each given resource
		{
			urf.addResource(resource);	//add the resource to the data model
		}
		return generateResources(writer, urf, resources.length>0 ? resources[0] : null);	//generate all resources related to the given resources
	}

	/**Generates all the resources within a given data model, indicating an optional resource that should appear first or immediately after namespace descriptions.
	@param writer The writer used for generating the information.
	@param urf The data model of which resources should be generated.
	@param primaryResource The main resource which should appear first or immediately after namespace descriptions, or <code>null</code> if there is no primary resource.
	@return The writer.
	@exception NullPointerException if the given writer and/or URF data model is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	public Writer generateResources(final Writer writer, final URF urf, final URFResource primaryResource) throws IOException
	{
		final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap=urf.getReferences();	//get a map of sets of all references to each resource
			//gather namespace URIs used
		final Map<URI, Boolean> namespaceURIMultipleMap=new HashMap<URI, Boolean>();	//create a hash map with namespace URI keys to keep track if a namespace is used multiple times
		for(final URI resourceURI:urf.getResourceURIs())	//look at each resource URI
		{
			final URI namespaceURI=getNamespaceURI(resourceURI);	//get the namespace URI of this resource URI
			if(namespaceURI!=null && !isLexicalNamespaceURI(namespaceURI))	//if this resource URI has a namespace that isn't a lexical namespace (URIs in lexical namespaces have their own short forms)
			{
				final Boolean hasMultipleReferences=namespaceURIMultipleMap.get(namespaceURI);	//see whether there are multiple references to this namespace
				if(hasMultipleReferences==null)	//if we haven't seen this namespace URI before
				{
					namespaceURIMultipleMap.put(namespaceURI, Boolean.FALSE);	//show that we've seen this namespace URI, but there are not yet multiple references
				}
				else if(hasMultipleReferences.booleanValue()==false)	//if we've only seen this namespace URI once
				{
					namespaceURIMultipleMap.put(namespaceURI, Boolean.TRUE);	//show that we've seen this namespace URI multiple times
				}
			}
		}
			//generate beginning labeled namespace URIs
		for(final Map.Entry<URI, Boolean> namespaceURIMultipleEntry:namespaceURIMultipleMap.entrySet())	//for each namespace URI entry
		{
			final URI namespaceURI=namespaceURIMultipleEntry.getKey();	//get the namespace URI
			if(Boolean.TRUE.equals(namespaceURIMultipleEntry.getValue()) || getNamespacePrefixManager().isRecognized(namespaceURI))	//if this namespace URI is used more than one time, or if this is a namespace URI we specifically know is a namespace URI
			{
				final URFResource namespaceURIResource=urf.locateResource(createLexicalURI(URI_CLASS_URI, namespaceURI.toString()));	//look up a resource for the namespace URI itself
				determineNamespaceURILabel(namespaceURIResource);	//make sure there is a label for the namespace URI
				generateRootResource(writer, urf, referenceMap, namespaceURIResource);	//generate the namespace URI resource
			}
		}
			//generate the primary resource
		if(primaryResource!=null)	//if there is a primary resource to generate
		{
			generateRootResource(writer, urf, referenceMap, primaryResource);	//generate the primary resource
		}
		final List<URFResource> resourceList=new ArrayList<URFResource>();	//create a list of resources
		addAll(resourceList, urf.getResources());	//add all the resources in the data model to the list
		sort(resourceList, reverseOrder(RESOURCE_PROPERTY_COUNT_COMPARATOR));	//sort the resources in reverse order of their number of properties, so that we'll have a bigger chance of inlining resources
		for(final URFResource resource:resourceList)	//iterate over all the resources, generating only those with no references (i.e. root resources)
		{
			if(!isGenerated(resource) && !referenceMap.hasItems(resource))	//if this resource has not yet been generated and it has no references to it
			{
				generateRootResource(writer, urf, referenceMap, resource);	//generate this root resource
			}
		}
		for(final URFResource resource:resourceList)	//iterate over all the remaining resources, generating any that have not yet been generated
		{
			if(!isGenerated(resource))	//if this resource has not yet been generated
			{
				generateRootResource(writer, urf, referenceMap, resource);	//generate this root resource
			}
		}
		return writer;	//return the writer
	}

	/**Generates a single top-level resource, prepending a list delimiter if appropriate.
	The resource is noted as having been generated.
	@param writer The writer used for generating the information.
	@param urf The URF data model.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource value.
	@param resource The resource to generate.
	@return The writer.
	@exception NullPointerException if the given writer, URF data model, reference map, and/or resource is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	protected Writer generateRootResource(final Writer writer, final URF urf, final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap, final URFResource resource) throws IOException
	{
		if(getGeneratedResourceCount()>0)	//if we've already generated a least one resource
		{
			writer.write(LIST_DELIMITER);	//write a list delimiter
			writeNewLine(writer);	//go to the next line
		}
		return generateResource(writer, urf, referenceMap, resource);	//generate the resource
	}

	/**Generates a single resource.
	The resource is noted as having been generated.
	@param writer The writer used for generating the information.
	@param urf The URF data model.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource value.
	@param resource The resource to generate.
	@return The writer.
	@exception NullPointerException if the given writer, URF data model, reference map, and/or resource is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	protected Writer generateResource(final Writer writer, final URF urf, final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap, final URFResource resource) throws IOException
	{
		return generateResource(writer, urf, referenceMap, null, null, resource, false, null);	//generate the single resource; there is no context URI
	}

	/**Generates a single resource.
	The resource is noted as having been generated.
	@param writer The writer used for generating the information.
	@param urf The URF data model.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource value.
	@param scopeSubject The scope to which the property and resource belongs, or <code>null</code> if the current resource is not in an object context.
	@param scopePredicateURI The predicate for which the resource is a value, or <code>null</code> if the current resource is not in an object context.
	@param resource The resource to generate.
	@param contextURI The URI serving as context so that a default namespace can be determined, or <code>null</code> if there is no context; for properties, this is the URI of the first type short form; for objects, this is the URI of the predicate resource.
	@param inSequence Whether the resource being generated is in a sequence and its scoped order properties should therefore not be generated.
	@return The writer.
	@exception NullPointerException if the given writer, URF data model, reference map, and/or resource is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	protected Writer generateResource(final Writer writer, final URF urf, final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap, final URFScope scopeSubject, final URI scopePredicateURI, final URFResource resource, final boolean inSequence, final URI contextURI) throws IOException
	{
		final boolean isGenerated=isGenerated(resource);	//see if this resource has already been generated
		if(!isGenerated)	//if the resource hasn't been generated, yet
		{
			setGenerated(resource, true);	//note that the resource has been generated so that any recursive references won't regenerate the entire resoure again
		}
//Debug.trace("generating resource with scope", scopeSubject, "predicate URI", scopePredicateURI, "and resource", resource);
		boolean generatedComponent=false;	//we haven't generated any components, yet
		URI lexicalTypeURI=null;	//the lexical namespace type URI, if any
/*TODO del
		final boolean isList=resource.hasTypeURI(LIST_CLASS_URI);	//see if this resource is a list
		boolean isListShortForm=isShortListsGenerated() && (isList || resource.hasNamespaceProperty(ORDINAL_NAMESPACE_URI));	//generate a list short form if this is a list or it has properties in the ordinal namespace
		final boolean isSet=resource.hasTypeURI(SET_CLASS_URI);	//see if this resource is a set
		boolean isSetShortForm=isShortSetsGenerated() && (isSet || resource.hasProperty(ELEMENT_PROPERTY_URI));	//generate a set short form if this is a set or it has properties in the element namespace
*/
		final URI uri=resource.getURI();	//get the resource URI
		String label=getLabel(resource);	//see if there is a label for this resource
		if(label==null && uri==null && !isGenerated)	//if there is no label or URI for this resource and the resource hasn't yet been generated
		{
			final Set<URFScope> referringScopes=referenceMap.get(resource);	//see how many scopes reference this resource
			if(referringScopes!=null && !referringScopes.isEmpty() && !(referringScopes.size()==1 && referringScopes.iterator().next()==scopeSubject))	//if there are multiple referring scopes, or just a single referring scope that isn't the scope subject in this context
			{
				label=determineLabel(resource);	//locate a label for this resource, because it has references
			}
		}
		if(label!=null)	//if there is a label
		{
			writeLabel(writer, label);	//write the label
			if(isGenerated)	//if we've already generated this resource, there's no reason to generate it again---the label will suffice
			{
				return writer;	//there's no need to generate the resource again, because the label is representing it
			}
		}
			//reference
		if(uri!=null)
		{
//Debug.trace("got resource URI:", uri);
			if(isLexicalURI(uri))	//if this URI is in a lexical namespace
			{
				lexicalTypeURI=getLexicalTypeURI(uri);	//get the lexical type of the URI so that we don't generate it again
			}
			generateReference(writer, urf, uri, contextURI);	//write a reference for the resource
			generatedComponent=true;	//indicate that we generated a component
			if(isGenerated)	//if we've already generated this resource, there's no reason to generate it again---the reference will suffice
			{
				return writer;	//there's no need to generate the resource again, because the reference is representing it
			}
		}
		boolean isList=false;	//we'll see if this is a list
		boolean hasNonListType=lexicalTypeURI!=null && !LIST_CLASS_URI.equals(lexicalTypeURI);	//we'll see if there is a type that is not a list
		boolean isSet=false;	//we'll see if this is a set
		boolean hasNonSetType=lexicalTypeURI!=null && !SET_CLASS_URI.equals(lexicalTypeURI);	//we'll see if there is a type that is not a set
		for(final URFResource type:resource.getTypes())	//look through all the types to determine
		{
			final URI typeURI=type.getURI();	//get the type URI
			if(SET_CLASS_URI.equals(typeURI))	//if this is the set type
			{
				isSet=true;	//this is a set
				hasNonListType=true;	//there is a non-set type
			}
			else	//if this is not a set type
			{
				hasNonSetType=true;	//there is a non-set type
				if(LIST_CLASS_URI.equals(typeURI))	//if this is the list type
				{
					isList=true;	//this is a list
				}
				else	//if this is not a list type
				{
					hasNonListType=true;	//there is a non-list type
				}
			}
		}
/*TODO del
if(isList || isSet)
{
	
	Debug.trace("is list", isList, "is set", isSet, "hasNonListNonSetType", hasNonListNonSetType, "hasNonSetType", hasNonSetType);
}
*/
		boolean isListShortForm=isShortListsGenerated() && (isList || (resource.hasNamespaceProperty(ORDINAL_NAMESPACE_URI) && hasNonListType));	//generate a list short form if this is a list or it has properties in the ordinal namespace (but only if there is some other type already present; otherwise, it would introduce a list type where none was specified)
		boolean isSetShortForm=isShortSetsGenerated() && (isSet || (resource.hasProperty(ELEMENT_PROPERTY_URI) && (hasNonSetType || isListShortForm)));	//generate a set short form if this is a set or it has properties in the element namespace (but only if there is some other type; otherwise, it would introduce a set type where none was specified)
/*TODO del
		boolean isListShortForm=isShortListsGenerated() && (isList || (resource.hasNamespaceProperty(ORDINAL_NAMESPACE_URI) && !hasNonListNonSetType));	//generate a list short form if this is a list or it has properties in the ordinal namespace (but only if there are no other types; it would introduce a list type where none was specified)
		boolean isSetShortForm=isShortSetsGenerated() && (isSet || (resource.hasProperty(ELEMENT_PROPERTY_URI) && (!hasNonSetType || (isListShortForm && !hasNonListNonSetType))));	//generate a set short form if this is a set or it has properties in the element namespace (but only if there are no other types; it would introduce a set type where none was specified)
*/
		URI propertyContextURI=null;	//we'll find the first type URI and use it for a context URI for properties
			//type
		final boolean isShortTypesGenerated=isShortTypesGenerated();	//see if we should generate types in the short form
		if(isShortTypesGenerated)	//if we should generate type short forms
		{
			long shortTypeCount=0;	//keep track of how many short-form types we've generated, if any
			markReferenceGenerated(urf, TYPE_PROPERTY_URI);	//mark that the type property was generated unless it has some other quality needed to be generated separately
			for(final URFResource type:resource.getTypes())	//look at each type
			{
				final URI typeURI=type.getURI();	//get the URI of this type
				if(typeURI!=null)	//if the given type has a URI
				{
					if(typeURI.equals(lexicalTypeURI))	//if this is the same type URI as the URI included in the lexical namespace type URI, if any
					{
						continue;	//skip this type
					}
					else if(isListShortForm && LIST_CLASS_URI.equals(typeURI))	//if we're using a list short form and this is the list type, skip the type
					{
						continue;	//skip this type
					}
					else if(isSetShortForm && !isListShortForm && SET_CLASS_URI.equals(typeURI))	//if we're using a set short form and this is the set type, skip the type, unless there is a list short form, which would negate the set short type, making us need to show it explicitly
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
					propertyContextURI=typeURI;	//use this first type URI as the context URI for properties
				}
				generateResource(writer, urf, referenceMap, resource, TYPE_PROPERTY_URI, type, false, null);	//write the type; don't allow namespace inheritance of name references
				++shortTypeCount;	//show that we've got another short type
			}
			if(shortTypeCount>0)	//if we generated any short form types
			{
				writer.write(TYPE_END);	//end the type declaration
				generatedComponent=true;	//indicate that we generated a component
			}
		}
/*TODO del when works
			//list
		if(isListShortForm)	//if we should generate a list short form
		{
			if(lexicalTypeURI==null && shortTypeCount==0 && !isList)	//if there have been no non-list types indicated, and this is not really a list
			{
				isListShortForm=false;	//change our minds about showing the list short form; otherwise, it would introduce a list type where none was specified
			}
			else	//if we're still a go for generating a list short form, do it
			{
				markReferenceGenerated(urf, LIST_CLASS_URI);	//mark that the list type was generated unless it has some other quality needed to be generated separately
				generateCollection(writer, urf, referenceMap, resource.getNamespaceProperties(ORDINAL_NAMESPACE_URI).iterator(), LIST_BEGIN, LIST_END);	//generate all the values of ordinal properties in the list
				generatedComponent=true;	//indicate that we generated a component
			}
		}
			//set
		if(isSetShortForm)	//if we should generate a set short form
		{
			if(lexicalTypeURI==null && shortTypeCount==0 && !isListShortForm && !isSet)	//if there have been no non-set types indicated, and this is not really a set
			{
				isSetShortForm=false;	//change our minds about showing the set short form; otherwise, it would introduce a set type where none was specified
			}
			else	//if we're still a go for generating a set short form, do it
			{
				markReferenceGenerated(urf, SET_CLASS_URI);	//mark that the set type was generated unless it has some other quality needed to be generated separately
				generateCollection(writer, urf, referenceMap, resource.getProperties(ELEMENT_PROPERTY_URI).iterator(), SET_BEGIN, SET_END);	//generate all the values of element properties in the set
				generatedComponent=true;	//indicate that we generated a component
			}
		}
*/
			//properties
		int propertyCount=0;	//start with no properties being generating
		propertyCount=generateProperties(writer, urf, referenceMap, resource, PROPERTY_VALUE_DELIMITER, propertyCount, !isShortTypesGenerated, !isListShortForm, !isSetShortForm, true, propertyContextURI);	//generate properties
		if(scopeSubject!=null && scopePredicateURI!=null)	//if this resource is the value of a property
		{
			final URFScope scope=scopeSubject.getScope(scopePredicateURI, resource);	//get the scope for this value
			if(scope==null)	//if there is no such scope
			{
				throw new IllegalArgumentException("No scope for given subject "+scopeSubject+" and predicate URI "+scopePredicateURI);
			}
			propertyCount=generateProperties(writer, urf, referenceMap, scope, SCOPED_PROPERTY_VALUE_DELIMITER, propertyCount, true, true, true, !inSequence, propertyContextURI);	//generate all scoped properties, suppressing generation of scoped order if we are in a sequence
		}
		if(propertyCount>0)	//if we started the properties section
		{
			unindent();
			writeNewLine(writer);
			writer.write(PROPERTIES_END);	//end the properties declaration
		}
		else if(!generatedComponent && !isListShortForm && !isSetShortForm)	//if we haven't generated any components, and there will be no list and/or set short form, have a properties section even if it's empty
		{
			writer.append(PROPERTIES_BEGIN).append(PROPERTIES_END);	//write an empty properties declaration
		}
			//list
		if(isListShortForm)	//if we should generate a list short form
		{
			markReferenceGenerated(urf, LIST_CLASS_URI);	//mark that the list type was generated unless it has some other quality needed to be generated separately
			generateCollection(writer, urf, referenceMap, resource.getNamespaceProperties(ORDINAL_NAMESPACE_URI).iterator(), LIST_BEGIN, LIST_END);	//generate all the values of ordinal properties in the list
		}
			//set
		if(isSetShortForm)	//if we should generate a set short form
		{
			markReferenceGenerated(urf, SET_CLASS_URI);	//mark that the set type was generated unless it has some other quality needed to be generated separately
			generateCollection(writer, urf, referenceMap, resource.getProperties(ELEMENT_PROPERTY_URI).iterator(), SET_BEGIN, SET_END);	//generate all the values of element properties in the set
		}
		return writer;	//return the writer
	}


	/**Generates the elements of a list or set.
	@param writer The writer used for generating the information.
	@param urf The URF data model.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource value.
	@param elementPropertyIterator An iterator to the elements of the resource.
	@param collectionBegin The beginning delimiter of the collection.
	@param collectionEnd The end delimiter of the collection.
	@return The writer.
	@exception NullPointerException if the given writer, URF data model, reference map, and/or element property iterator is <code>null</code>.
	@exception IOException if there is an error writing to the writer.
	*/
	protected Writer generateCollection(final Writer writer, final URF urf, final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap, final Iterator<URFProperty> elementPropertyIterator, final char collectionBegin, final char collectionEnd) throws IOException
	{
		if(elementPropertyIterator.hasNext())	//if there are elements in the collection
		{
			writeNewLine(writer);
			writer.write(collectionBegin);	//start the collection
			indent();	//indent the collection
			writeNewLine(writer);
			int elementCount=0;	//keep track of how many elements we have
			while(elementPropertyIterator.hasNext())	//while there are elements
			{
				final URFProperty elementProperty=elementPropertyIterator.next();	//get the next element property
				final URI elementPropertyURI=elementProperty.getPropertyURI();	//get the element property URI in case we need to use it as a context
				if(elementCount>0)	//if we've already generated an element
				{
					writer.append(LIST_DELIMITER);	//separate the properties
					writeNewLine(writer);
				}
				generateResource(writer, urf, referenceMap, elementProperty.getSubjectScope(), elementProperty.getPropertyURI(), elementProperty.getValue(), false, elementPropertyURI);	//generate the element; each element value is an object of the element predicate, so use the element property URI as the context
				++elementCount;	//show that we generated another list element
			}
			unindent();
			writeNewLine(writer);
			writer.write(collectionEnd);	//end the collection
		}
		else	//if there are no elements in the collection
		{
			writer.append(collectionBegin).append(collectionEnd);	//show the empty collection on the same line
		}
		return writer;	//return the writer
	}

	/**Generates the properties, if any, of a given scope, without property section delimiters.
	@param writer The writer used for generating the information.
	@param urf The URF data model.
	@param referenceMap A map that associates, for each resource, a set of all scopes that reference that resource value.
	@param scope The scope the properties of which should be generated.
	@param propertyValueDelimiter The delimiter to use to separate properties and values.
	@param propertyCount the number of properties already generated; used to determine whether a new properties section should be generated.
	@param generateTypes Whether type properties should be generated.
	@param generateOrdinals Whether properties in the ordinal namespace should be generated.
	@param generateElements Whether the element property should be generated.
	@param generateOrder Whether the order property should be generated.
	@param contextURI The URI serving as context so that a default namespace can be determined, or <code>null</code> if there is no context; for properties, this is the URI of the first type short form; for objects, this is the URI of the predicate resource.
	@return The new total number of properties generated, including the properties already generated before this method was called.
	@exception NullPointerException if the given writer, URF data model, reference map, and/or scope is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	*/
	protected int generateProperties(final Writer writer, final URF urf, final CollectionMap<URFResource, URFScope, Set<URFScope>> referenceMap, final URFScope scope, final char propertyValueDelimiter, int propertyCount, final boolean generateTypes, final boolean generateOrdinals, final boolean generateElements, final boolean generateOrder, final URI contextURI) throws IOException
	{
		URI sequencePropertyURI=null;	//this will indicate when we're in the middle of a sequence for a particular property
		for(final URFProperty property:scope.getProperties())	//look at each property
		{
			final URFResource value=property.getValue();	//get the property value
			final URI propertyURI=property.getPropertyURI();	//get the property URI
			if((!generateTypes && TYPE_PROPERTY_URI.equals(propertyURI))	//if we shouldn't generate types and this is a type
				|| (!generateOrdinals && propertyURI!=null && ORDINAL_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//or if we shouldn't generate ordinals and this property is an ordinal
				|| (!generateElements && propertyURI!=null && ELEMENT_PROPERTY_URI.equals(propertyURI))	//or if we shouldn't generate elements and this property is an element property
				|| (!generateOrder && ORDER_PROPERTY_URI.equals(propertyURI)))	//or if we shouldn't generate order and this is an order property
			{
				markReferenceGenerated(urf, propertyURI);	//mark that this property was generated unless it has some other quality needed to be generated separately
				final URI valueURI=value.getURI();	//get the URI of the value, if any
				if(valueURI!=null)	//if there is a value URI
				{
					markReferenceGenerated(urf, valueURI);	//mark that the value was generated unless it has some other quality needed to be generated separately
				}
				continue;	//skip this property
			}
				//that this implementation automatically sorts scoped ordered properties with the ordered properties ahead of non-ordered properties
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
					writer.write(PROPERTIES_BEGIN);	//start the properties declaration
					indent();	//indent the properties
					writeNewLine(writer);
				}
				generateReference(writer, urf, propertyURI, contextURI);	//generate the reference of the property
				markReferenceGenerated(urf, propertyURI);	//mark that this property was generated unless it has some other quality needed to be generated separately
				writer.append(propertyValueDelimiter);	//=/~
				if(property.getScope().getOrder()!=null)	//if this property has a sequence
				{
					writer.write(SEQUENCE_BEGIN);	//start a sequence for this property
					sequencePropertyURI=propertyURI;	//indicate that we should have a sequence for this property
					generateResource(writer, urf, referenceMap, scope, propertyURI, value, true, sequencePropertyURI);	//generate the property value, indicating that the value is an element in a sequence; use the property URI as the context URI
				}
				else	//if this property has no sequence
				{
					generateResource(writer, urf, referenceMap, scope, propertyURI, value, false, propertyURI);	//generate the property value normally, using the property URI as the context URI
				}
			}
			else	//if we are still in the middle of a sequence
			{
				writer.append(LIST_DELIMITER);	//separate the values in the sequence
				generateResource(writer, urf, referenceMap, scope, propertyURI, value, true, sequencePropertyURI);	//generate the property value, indicating that the value is an element in a sequence, and using the property URI as the context URI
			}
			++propertyCount;	//show that we generated another property
		}
		if(sequencePropertyURI!=null)	//if we were in the middle of a sequence
		{
			writer.write(SEQUENCE_END);	//end the sequence for this property, as we're out of properties
		}
		return propertyCount;	//return the new property count
	}

	/**Generates a reference to a resource with the given URI.
	A name reference or short form will be used if appropriate.
	@param writer The writer used for generating the information.
	@param urf The URF data model.
	@param uri The URI of the resource.
	@param contextURI The URI serving as context so that a default namespace can be determined, or <code>null</code> if there is no context; for properties, this is the URI of the first type short form; for objects, this is the URI of the predicate resource.
	@exception NullPointerException if the given writer and/or URI is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	@see #generateURIReference(Writer, URI)
	*/
	public void generateReference(final Writer writer, final URF urf, final URI uri, final URI contextURI) throws IOException
	{
//Debug.trace("ready to write reference for URI", uri);
		if(isLexicalURI(uri))	//if this URI is in a lexical namespace
		{
//Debug.trace("is lexical URI");
			final URI lexicalTypeURI=getLexicalTypeURI(uri);	//get the lexical type of the URI so that we don't generate it again
//Debug.trace("is lexical URI with lexical type", lexicalTypeURI);
			markReferenceGenerated(urf, lexicalTypeURI);	//mark that this lexical type was generated unless it has some other quality needed to be generated separately
			final String lexicalForm=getLocalName(uri);	//get the lexical form of the lexical type
			assert lexicalForm!=null : "A lexical namespace URI should always have a lexical form.";
//Debug.trace("lexical resource with type:", lexicalTypeURI);
			if(BINARY_CLASS_URI.equals(lexicalTypeURI))	//binary
			{
				writer.append(BINARY_BEGIN).append(lexicalForm).append(BINARY_END);	//write the binary short form
				return;
			}
			else if(BOOLEAN_CLASS_URI.equals(lexicalTypeURI))	//boolean
			{
				writer.append(BOOLEAN_BEGIN).append(lexicalForm);	//write the boolean short form
				return;
			}
			else if(CHARACTER_CLASS_URI.equals(lexicalTypeURI))	//character
			{
				writer.append(CHARACTER_BEGIN).append(lexicalForm).append(CHARACTER_END);	//write the character short form
				return;
			}
			else if(INTEGER_CLASS_URI.equals(lexicalTypeURI) || REAL_CLASS_URI.equals(lexicalTypeURI))	//integer or real
			{
				writer.append(NUMBER_BEGIN).append(lexicalForm);	//write the number short form
				return;
			}
			else if(ORDINAL_CLASS_URI.equals(lexicalTypeURI))	//ordinal
			{
				writer.append(ORDINAL_BEGIN).append(lexicalForm);	//write the ordinal short form
				return;
			}
			else if(REGULAR_EXPRESSION_CLASS_URI.equals(lexicalTypeURI))	//if this is a regular expression
			{
				writeRegularExpression(writer, lexicalForm);	//write the regular expression short form
				return;
			}
			else if(STRING_CLASS_URI.equals(lexicalTypeURI))	//if this is a string
			{
				writeString(writer, lexicalForm);	//write the string short form
				return;
			}
			//TODO fix timestamp
			else if(URI_CLASS_URI.equals(lexicalTypeURI))	//if this is a URI
			{
				writeURI(writer, lexicalForm);	//write the URI short form
				return;
			}
		}
		final URI namespaceURI=getNamespaceURI(uri);	//see if the URI has a namespace
		if(namespaceURI!=null)	//if there is a namespace
		{
			final boolean suppressPrefix=isInheritedNamespacePrefixesSuppressed() && contextURI!=null && namespaceURI.equals(getNamespaceURI(contextURI));	//if we should suppress prefixes for inherited namespaces, see if this context URI has the same namespace as this reference
			final String prefix=suppressPrefix ? null : getNamespacePrefixManager().get(namespaceURI);	//see if we have a prefix for this namespace, but only if we shouldn't suppress prefixes
			if(prefix!=null || suppressPrefix)	//if we have a prefix, or we're suppressing this prefix
			{
				final String localName=getLocalName(uri);	//get the local name of the URI
				assert localName!=null : "If a URI has a namespace, it should have a local name as well.";
				if(prefix!=null)	//if there is a prefix
				{
					writer.append(prefix).append(NAME_PREFIX_DELIMITER);	//prefix:					
				}
				writer.append(localName);	//prefix:localName
				return;
			}
		}
		generateURIReference(writer, uri);	//generate the URI reference normally by default
	}

	/**Marks a reference as generated if appropriate.
	If a reference is to a resource with no properties, or the reference is a lexical URI and there is only a single type of the lexical type,
	there will be no need to generate the resource in full form, so it will be marked as generated.
	This method is useful for marking property URI references as generated if they do not need to stand on their own.
	@param urf The URF data model.
	@param resourceURI The URI of the generated reference.
	*/
	protected void markReferenceGenerated(final URF urf, final URI resourceURI)
	{
		final URFResource resource=urf.getResource(resourceURI);	//see if there is a resource for this URI in the data model
		if(resource!=null)	//if there is such a resource
		{
			resource.readLock().lock();	//lock the resource for reading
			try
			{
				final long propertyCount=resource.getPropertyCount();	//find out how many properties the resource has
				if(propertyCount>0)	//if there is more than one property, make sure it's not just a type of a lexical URI
				{
					if(propertyCount!=1 || !isLexicalURI(resourceURI))	//if there's not just one property, or this is not a lexical URI
					{
						return;	//we shouldn't mark this resource as generated
					}
					final URI lexicalTypeURI=getLexicalTypeURI(resourceURI);	//find out the lexical type of the URI
					if(!resource.hasTypeURI(lexicalTypeURI))	//if there is another type besides the lexical type, don't mark the resource as generated; otherwise, the type is redundance
					{
						return;	//we shouldn't mark this resource as generated
					}
				}
				setGenerated(resource, true);	//make the resource is generated
			}
			finally
			{
				resource.readLock().unlock();	//always release the read lock
			}
		}
	}

	/**Writes a URI reference to a resource with the given URI.
	@param writer The writer used for generating the information.
	@param uri The URI of the resource.
	@exception NullPointerException if the given writer and/or URI is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	*/
	public void generateURIReference(final Writer writer, URI uri) throws IOException
	{
		writer.write(REFERENCE_BEGIN);	//start the URI reference
		if(isLexicalURI(uri))	//if this URI is in a lexical namespace
		{
//Debug.trace("is lexical URI");
			final URI lexicalTypeURI=getLexicalTypeURI(uri);	//get the lexical type of the URI
//Debug.trace("is lexical URI with lexical type", lexicalTypeURI);
			final String lexicalForm=getLocalName(uri);	//get the lexical form of the lexical type
			assert lexicalForm!=null : "A lexical namespace URI should always have a lexical form.";
			writeString(writer, lexicalForm);	//write the string lexical form
			uri=lexicalTypeURI;	//the lexical type is now the URI to write
		}
		final URI baseURI=getBaseURI();	//get the base URI
		if(baseURI!=null)	//if there is a base URI
		{
			uri=baseURI.relativize(uri);	//relativize the URI to the base URI if possible TODO check for labeled URIs
		}
		final URI namespaceURI=getNamespaceURI(uri);	//see if the URI has a namespace
		if(namespaceURI!=null)	//if there is a namespace
		{
			final String prefix=getNamespacePrefixManager().get(namespaceURI);	//see if we have a prefix for this namespace
			if(prefix!=null)	//if we have a prefix
			{
				final String localName=getLocalName(uri);	//get the local name of the URI
				assert localName!=null : "If a URI has a namespace, it should have a local name as well.";
				writeLabel(writer, prefix);	//write the prefix as a label
				uri=URI.create(new StringBuilder().append(FRAGMENT_SEPARATOR).append(encodeURI(localName)).toString());	//use the encoded fragment as the remaining URI TODO fix to work with hierarchical namespaces
			}
		}
		writer.write(uri.toString());	//write the relative URI
		writer.write(REFERENCE_END);	//end the URI reference
	}

	/**Writes a label with appropriate delimiters.
	@param writer The writer used for generating the information.
	@param label The label to write.
	@exception NullPointerException if the given writer and/or label is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	*/
	public static void writeLabel(final Writer writer, final String label) throws IOException
	{
		writer.append(LABEL_BEGIN).append(checkInstance(label, "Label cannot be null.")).append(LABEL_END);	//write the label
	}

	/**Writes a regular expression surrounded by the regular expression short form delimiters.
	The usual reserved string characters, along with the regular expression delimiters, will be escaped.
	@param writer The writer used for generating the information.
	@param regularExpression The regular expression to write.
	@exception NullPointerException if the given writer and/or regular expression is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	@see TURF#REGULAR_EXPRESSION_BEGIN
	@see TURF#REGULAR_EXPRESSION_END
	*/
	public static void writeRegularExpression(final Writer writer, final String regularExpression) throws IOException
	{
		writeString(writer, regularExpression, REGULAR_EXPRESSION_BEGIN, REGULAR_EXPRESSION_END);	//write the regular expression using the regular expression short form delimiters
	}

	/**Writes a string surrounded by the string short form delimiters.
	The usual reserved string characters, along with the string delimiters, will be escaped.
	@param writer The writer used for generating the information.
	@param string The string to write.
	@exception NullPointerException if the given writer and/or string is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	@see TURF#STRING_BEGIN
	@see TURF#STRING_END
	*/
	public static void writeString(final Writer writer, final String string) throws IOException
	{
		writeString(writer, string, STRING_BEGIN, STRING_END);	//write the string using the string short form delimiters
	}

	/**Writes a URI surrounded by the URI short form delimiters.
	The usual reserved string characters, along with the URI delimiters, will be escaped.
	@param writer The writer used for generating the information.
	@param uri The URI to write.
	@exception NullPointerException if the given writer and/or URI is <code>null</code>.
	@exception IOException if there was an error writing to the writer.
	@see TURF#URI_BEGIN
	@see TURF#URI_END
	*/
	public static void writeURI(final Writer writer, final String uri) throws IOException
	{
		writeString(writer, uri, URI_BEGIN, URI_END);	//write the URI using the URI short form delimiters
	}

	/**Writes a string surrounded by specified string delimiters.
	The usual reserved string characters, along with the given delimiters, will be escaped.
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
				case START_OF_STRING_CHAR:	//start of string
					escaped=true;	//show that we should escape the character
					c=ESCAPED_START_OF_STRING;	//use \“ for start of string
					break;
				case STRING_TERMINATOR_CHAR:	//string terminator
					escaped=true;	//show that we should escape the character
					c=ESCAPED_STRING_TERMINATOR;	//use \” for string terminator
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
