/*
 * Copyright © 2007-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf;

import java.io.*;
import java.net.URI;
import java.util.*;

import static java.util.Collections.*;
import java.util.concurrent.atomic.AtomicLong;

import com.globalmentor.collections.IdentityHashSet;
import com.globalmentor.java.Integers;

import static com.globalmentor.collections.Collections.*;
import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.TURF.*;
import static com.globalmentor.urf.URF.*;

/**Generates TURF from URF.
@author Garret Wilson
*/
public class URFTURFGenerator
{

	/**The base URI of the URF data model, or <code>null</code> if the base URI is unknown.*/
	private final URI baseURI;

		/**@return The base URI of the URF data model, or <code>null</code> if the base URI is unknown.*/
		public URI getBaseURI() {return baseURI;}

	/**The namespace label manager.*/
	private final TURFNamespaceLabelManager namespaceLabelManager;

		/**@return The namespace label manager.*/
		public TURFNamespaceLabelManager getNamespaceLabelManager() {return namespaceLabelManager;}

	/**The set of URIs of named resources that have been generated; keep separate sets of named and non-named resources because some anonymous resource equals() methods may not use identity, which we need.*/
	private final Set<URI> generatedResourceURISet;
	/**The set of anonymous resources that have been generated.*/
	private final Set<URFResource> generatedAnonymousResourceSet;

		/**Determines the number of resources that have been generated.
		@return The number of resources that have been generated.
		*/
		protected final int getGeneratedResourceCount()
		{
			return generatedResourceURISet.size()+generatedAnonymousResourceSet.size();	//return the combined size of the generated resource sets
		}

		/**Determines if the given resource has been generated.
		@param resource The resource to check.
		@return <code>true</code> if the resource has already been generated, else <code>false</code>.
		*/
		protected final boolean isGenerated(final URFResource resource)
		{
			final URI resourceURI=resource.getURI();	//see if this resource has a URI
			return resourceURI!=null ? generatedResourceURISet.contains(resourceURI) : generatedAnonymousResourceSet.contains(resource);	//see if the resource is in one of our sets
		}

		/**Sets the serialized status of a given resource.
		@param resource The resource for which the generated status should be set.
		@param generated <code>true</code> if the resource has been generated, or <code>false</code> if not.
		*/
		protected final void setGenerated(final URFResource resource, final boolean generated)
		{
			final URI resourceURI=resource.getURI();	//see if this resource has a URI
			if(resourceURI!=null)	//if this resource has a URI
			{
				if(generated)	//if the resource has been generated
				{
					generatedResourceURISet.add(resourceURI);	//add the resource URI to the set of generated resource URIs
				}
				else	//if the resource has not been generated
				{
					generatedResourceURISet.remove(resourceURI);	//remove the resource URI from the set of generated resources URIs
				}
			}
			else	//if this resource has no URI
			{
				if(generated)	//if the resource has been generated
				{
					generatedAnonymousResourceSet.add(resource);	//add the resource to the set of generated resources
				}
				else	//if the resource has not been generated
				{
					generatedAnonymousResourceSet.remove(resource);	//remove the resource from the set of generated resources
				}				
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
		For namespace URI resources, a namespace label will be generated
		@param resource The resource for which a label should be returned.
		@return A label to represent the given resource.
		*/
		protected String determineLabel(final URFResource resource)
		{
			String label=getLabel(resource);	//get a label, if any, for the given resource
			if(label==null)	//if there is no label for this resource
			{
				label=generateLabel();	//generate a label for the resource
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

	/**Whether formatted output uses the list delimiter in addition to newlines.*/
	private boolean formattedListDelimiter=true;

		/**Indicates whether formatted output uses the list delimiter in addition to newlines.
		This option is set to <code>true</code> by default.
		@return Whether formatted output uses the list delimiter in addition to newlines.
		*/
		public boolean isFormattedListDelimiter() {return formattedListDelimiter;}

		/**Sets whether formatted output uses the list delimiter in addition to newlines.
		@param formattedListDelimiter Whether formatted output uses the list delimiter.
		*/
		public void setFormattedListDelimiter(final boolean formattedListDelimiter) {this.formattedListDelimiter=formattedListDelimiter;}

	/**Whether a preamble (required for correct TURF documents) of namespace prefix associations is generated.*/
	//TODO del if not needed	private boolean preambleGenerated=true;

		/**@return Whether a preamble (required for correct TURF documents) of namespaces prefix associations is generated.*/
	//TODO del if not needed		public boolean isPreambleGenerated() {return preambleGenerated;}
	
		/**Sets whether a preamble (required for correct TUF documents) is generated with namespace prefix associations.
		@param preambleGenerated <code>true</code> if a preamble is generated.
		*/
	//TODO del if not needed		public void setPreambleGenerated(final boolean preambleGenerated) {this.preambleGenerated=preambleGenerated;}

	/**Whether namespace prefixes are always used for property resources not in the default namespace.*/
	private boolean propertyNamespacePrefixesForced=false;

		/**@return Whether namespace prefixes are always used for property resources not in the default namespace.*/
		public boolean isPropertyNamespacePrefixesForced() {return propertyNamespacePrefixesForced;}
		
		/**Sets whether namespace prefixes are always used for property resources not in the default namespace.
		If a container is generated, this implies that all property resources will receive a namespace prefix association,
		regardless of the number of times that namespace is used. If a container is generated, this means that property
		resource will be generated using prefixes that have no explicit namespace association; this is usually useful
		only for human consumption.
		@param propertyNamespacePrefixesForced Whether namespace prefixes are always used for property resources not in the default namespace.
		*/
		public void setPropertyNamespacePrefixesForced(final boolean propertyNamespacePrefixesForced) {this.propertyNamespacePrefixesForced=propertyNamespacePrefixesForced;}
		
	/**Whether prefixes are suppressed for inherited namespaces.*/
//TODO del if not needed	private boolean inheritedNamespacePrefixesSuppressed=true;

		/**@return Whether prefixes are suppressed for inherited namespaces.*/
	//TODO del if not needed		public boolean isInheritedNamespacePrefixesSuppressed() {return inheritedNamespacePrefixesSuppressed;}

		/**Sets whether prefixes are suppressed for inherited namespaces.
		@param inheritedNamespacePrefixesSuppressed Whether prefixes should be suppressed for inherited namespaces.
		*/
	//TODO del if not needed		public void setInheritedNamespacePrefixes(final boolean inheritedNamespacePrefixesSuppressed) {this.inheritedNamespacePrefixesSuppressed=inheritedNamespacePrefixesSuppressed;}

	/**Whether interfaces are generated as short forms.*/
	private boolean shortInterfacesGenerated=true;

		/**@return Whether interfaces are generated as short forms.*/
		public boolean isShortInterfacesGenerated() {return shortInterfacesGenerated;}

		/**Sets whether interfaces should be generated as short forms.
		@param shortInterfacesGenerated Whether interfaces should be generated as short forms.
		*/
		public void setShortInterfacesGenerated(final boolean shortInterfacesGenerated) {this.shortInterfacesGenerated=shortInterfacesGenerated;}

	/**Whether types are generated as short forms.*/
	private boolean shortTypesGenerated=true;

		/**@return Whether types are generated as short forms.*/
		public boolean isShortTypesGenerated() {return shortTypesGenerated;}

		/**Sets whether types should be generated as short forms.
		@param shortTypesGenerated Whether types should be generated as short forms.
		*/
		public void setShortTypesGenerated(final boolean shortTypesGenerated) {this.shortTypesGenerated=shortTypesGenerated;}

	/**Whether communities are generated as short forms.*/
	private boolean shortCommunitiesGenerated=true;

		/**@return Whether communities are generated as short forms.*/
		public boolean isShortCommunitiesGenerated() {return shortCommunitiesGenerated;}

		/**Sets whether communities should be generated as short forms.
		@param shortCommunitiesGenerated Whether communities should be generated as short forms.
		*/
		public void setShortCommunitiesGenerated(final boolean shortCommunitiesGenerated) {this.shortCommunitiesGenerated=shortCommunitiesGenerated;}

	/**Whether propositions are generated as short forms.*/
	private boolean shortPropositionsGenerated=true;

		/**@return Whether propositions are generated as short forms.*/
		public boolean isShortPropositionsGenerated() {return shortPropositionsGenerated;}

		/**Sets whether propositions should be generated as short forms.
		@param shortPropositionsGenerated Whether propositions should be generated as short forms.
		*/
		public void setShortPropositionsGenerated(final boolean shortPropositionsGenerated) {this.shortPropositionsGenerated=shortPropositionsGenerated;}

	/**Whether lists are generated as short forms.*/
	private boolean shortListsGenerated=true;

		/**@return Whether lists are generated as short forms.*/
		public boolean isShortListsGenerated() {return shortListsGenerated;}

		/**Sets whether lists should be generated as short forms.
		@param shortListsGenerated Whether lists should be generated as short forms.
		*/
		public void setShortListsGenerated(final boolean shortListsGenerated) {this.shortListsGenerated=shortListsGenerated;}

	/**Whether maps are generated as short forms.*/
	private boolean shortMapsGenerated=true;

		/**@return Whether maps are generated as short forms.*/
		public boolean isShortMapsGenerated() {return shortMapsGenerated;}

		/**Sets whether maps should be generated as short forms.
		@param shortMapsGenerated Whether maps should be generated as short forms.
		*/
		public void setShortMapsGenerated(final boolean shortMapsGenerated) {this.shortMapsGenerated=shortMapsGenerated;}

	/**Whether sets are generated as short forms.*/
	private boolean shortSetsGenerated=true;

		/**@return Whether sets are generated as short forms.*/
		public boolean isShortSetsGenerated() {return shortSetsGenerated;}

		/**Sets whether sets should be generated as short forms.
		@param shortSetsGenerated Whether sets should be generated as short forms.
		*/
		public void setShortSetsGenerated(final boolean shortSetsGenerated) {this.shortSetsGenerated=shortSetsGenerated;}

	/**Whether superclasses are generated as short forms.*/
	private boolean shortSuperclassesGenerated=true;

		/**@return Whether superclasses are generated as short forms.*/
		public boolean isShortSuperclassesGenerated() {return shortSuperclassesGenerated;}

		/**Sets whether superclasses should be generated as short forms.
		@param shortSuperclassesGenerated Whether superclasses should be generated as short forms.
		*/
		public void setShortSuperclassesGenerated(final boolean shortSuperclassesGenerated) {this.shortSuperclassesGenerated=shortSuperclassesGenerated;}

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
		this(baseURI, formatted, new TURFNamespaceLabelManager());	//create the class with a default namespace label manager
	}

	/**Base URI, formatted, and namespace label manager constructor.
	@param baseURI The base URI of the URF data model, or <code>null</code> if the base URI is unknown.
	@param formatted Whether output is formatted.
	@param namespaceLabelManager The manager of namespaces and labels.
	@exception NullPointerException if the given namespace label manager is <code>null</code>.
	*/
	public URFTURFGenerator(final URI baseURI, final boolean formatted, final TURFNamespaceLabelManager namespaceLabelManager)
	{
		this.baseURI=baseURI;
		this.formatted=formatted;
		this.namespaceLabelManager=checkInstance(namespaceLabelManager, "Namespace label manager cannot be null.");
		generatedResourceURISet=new HashSet<URI>();	//create a map that will determine whether named resources have been generated
		generatedAnonymousResourceSet=new IdentityHashSet<URFResource>();	//create a map that will determine whether anonymous resources have been generated, tracking them by identity
		resourceLabelMap=new HashMap<URFResource, String>();	//create a map of node IDs keyed to resources
	}

	/**Initializes the generator by resetting values and initializing the namespace labels.
	@see #reset()
	*/
	protected void initialize()
	{
		reset();	//reset the generator
	}

	/**Releases memory by clearing all internal maps and sets of resources.*/
	protected void reset()
	{
		generatedResourceURISet.clear();	//show that we've not generated any resources
		generatedAnonymousResourceSet.clear();	//show that we've not generated any resources
		resourceLabelMap.clear();	//clear our map of node IDs
	}

	/**Generates all the resources within a given data model.
	A TURF container will be generated to contain the generated resources.
	@param appendable The appendable used for generating the information.
	@param urf The data model of which resources should be generated.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or URF data model is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final URF urf) throws IOException
	{
		return generateResources(appendable, urf, true);	//generate the resources, indicating that a container should be generated
	}

	/**Generates all the resources within a given data model.
	@param appendable The appendable used for generating the information.
	@param urf The data model of which resources should be generated.
	@param generateContainer Whether a TURF signature and instance community should be generated to hold the generated resources, if any.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or URF data model is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final URF urf, final boolean generateContainer) throws IOException
	{
		return generateResources(appendable, urf, generateContainer, null);	//generate all URF resources with no particular resource as the primary resource
	}

	/**Generates the given resources and all related resources.
	A TURF container will be generated to contain the generated resources.
	@param appendable The appendable used for generating the information.
	@param resources The resources to generate, with the first resource, if any, being the resource to appear first.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or resources is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final URFResource... resources) throws IOException
	{
		return generateResources(appendable, true, resources);	//generate the resources, indicating that a container should be generated
	}

	/**Generates the given resources and all related resources.
	@param appendable The appendable used for generating the information.
	@param generateContainer Whether a TURF signature and instance community should be generated to hold the generated resources, if any.
	@param resources The resources to generate, with the first resource, if any, being the resource to appear first.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or resources is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final boolean generateContainer, final URFResource... resources) throws IOException
	{
		final URF urf=new URF();	//create a new URF data model
		for(final URFResource resource:resources)	//for each given resource
		{
			urf.addResource(resource);	//add the resource to the data model
		}
		return generateResources(appendable, urf, generateContainer, resources.length>0 ? resources[0] : null);	//generate all resources related to the given resources
	}

	/**Generates the given resources and all related resources.
	A TURF container will be generated to contain the generated resources.
	@param appendable The appendable used for generating the information.
	@param resources The resources to generate, with the first resource, if any, being the resource to appear first.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or resources is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final Iterable<URFResource> resources) throws IOException
	{
		return generateResources(appendable, true, resources);	//generate the resources, indicating that a container should be generated
	}

	/**Generates the given resources and all related resources.
	@param appendable The appendable used for generating the information.
	@param generateContainer Whether a TURF signature and instance community should be generated to hold the generated resources, if any.
	@param resources The resources to generate, with the first resource, if any, being the resource to appear first.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or resources is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final boolean generateContainer, final Iterable<URFResource> resources) throws IOException
	{
		final URF urf=new URF();	//create a new URF data model
		URFResource firstResource=null;	//we'll note the first resource, if any
		for(final URFResource resource:resources)	//for each given resource
		{
			urf.addResource(resource);	//add the resource to the data model
			if(firstResource==null)	//if we haven't yet found the first resource
			{
				firstResource=resource;	//this is the first resource
			}
		}
		return generateResources(appendable, urf, generateContainer, firstResource);	//generate all resources related to the given resources
	}

	/**Generates all the resources within a given data model, indicating an optional resource that should appear first.
	A TURF container will be generated to contain the generated resources.
	@param appendable The appendable used for generating the information.
	@param urf The data model of which resources should be generated.
	@param primaryResource The main resource which should appear first, or <code>null</code> if there is no primary resource.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or URF data model is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final URF urf, final URFResource primaryResource) throws IOException
	{
		return generateResources(appendable, urf, true, primaryResource);	//generate the resources, indicating that a container should be generated
	}

	/**Generates all the resources within a given data model, indicating an optional resource that should appear first.
	@param appendable The appendable used for generating the information.
	@param urf The data model of which resources should be generated.
	@param generateContainer Whether a TURF signature and instance community should be generated to hold the generated resources, if any.
	@param primaryResource The main resource which should appear first, or <code>null</code> if there is no primary resource.
	@return The appendable.
	@exception NullPointerException if the given appendable and/or URF data model is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	public <A extends Appendable> A generateResources(final A appendable, final URF urf, final boolean generateContainer, final URFResource primaryResource) throws IOException
	{
		initialize();	//initialize the generator
		final URF.ReferenceSummary referenceSummary=urf.getReferenceSummary();	//get a summary of all references to each resource
		final boolean isPropertyNamespacePrefixesForced=isPropertyNamespacePrefixesForced();	//see if we should always have property namespace prefixes
		if(generateContainer)	//if we should generate a container
		{
			appendable.append(SIGNATURE);	//write the TURF signature
			boolean startedProperties=false;	//we'll keep track of whether we started a TURF preamble
				//gather namespace URIs used; namespace may be used more than once by the appearance of multiple resources in the same namespace and/or the same resource referenced multiple times
			final boolean isShortTypesGenerated=isShortTypesGenerated();	//see if short types are generated
			final Map<URI, Boolean> namespaceURIMultipleMap=new HashMap<URI, Boolean>();	//create a hash map with namespace URI keys to keep track if a namespace is used multiple times
				//gather all namespace URIs of resources used as objects
			for(final Map.Entry<URFResource, Set<URFScope>> objectReferenceMapEntry:referenceSummary.objectReferenceMap.entrySet())//look at each resource and the number of references it has
			{
				final URI resourceURI=objectReferenceMapEntry.getKey().getURI();	//get the URI of this resource
				if(resourceURI!=null && !SHORT_REFERENCE_CLASS_URIS.contains(resourceURI))	//if this resource has a URI (if not, it has no namespace) and it doesn't identify a type that will receive a short reference (such URIs will never show up in normal circumstances)
				{
					final URI namespaceURI=getNamespaceURI(resourceURI);	//get the namespace URI of this resource URI
					if(namespaceURI!=null && !isInlineNamespaceURI(namespaceURI))	//if this resource URI has a namespace that isn't an inline namespace (URIs in inline namespaces have their own short forms)
					{
						if(objectReferenceMapEntry.getValue().size()>1)	//if this resource is referenced more than once
						{
							namespaceURIMultipleMap.put(namespaceURI, Boolean.TRUE);	//show that we've seen this namespace URI multiple times, because the resource itself is referenced multiple times
						}
						else	//if this resource is only referenced once or not at all (for the root resource)
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
				}
			}
				//gather all namespace URIs of property URIs
			for(final Map.Entry<URI, Set<URFScope>> propertyURIReferenceMapEntry:referenceSummary.propertyURIReferenceMap.entrySet())//look at each resource and the number of references it has
			{
				final URI propertyURI=propertyURIReferenceMapEntry.getKey();	//get the URI of the property
				if(isShortTypesGenerated && TYPE_PROPERTY_URI.equals(propertyURI))	//if we're generating short types and this is the type URI
				{
					continue;	//ignore the type property URI, as it will never show up in normal circumstances
				}
				final URI namespaceURI=getNamespaceURI(propertyURI);	//get the namespace URI of this property URI
				if(namespaceURI!=null && !isInlineNamespaceURI(namespaceURI))	//if this property URI has a namespace that isn't an inline namespace (URIs in inline namespaces have their own short forms)
				{
					if(isPropertyNamespacePrefixesForced)	//if we should always use prefixes for property namespaces
					{
						namespaceURIMultipleMap.put(namespaceURI, Boolean.TRUE);	//pretend that we've seen this namespace URI multiple times
					}
					else	//if we're not forcing property namespaces, see if there are enough namespace occurrences to warrant a prefix
					{
						final Integer referenceCountInteger=referenceSummary.propertyURIReferenceCountMap.get(propertyURI);	//see how many times this property URI is used as a property URI (either by distinct resources or by a single resource)
						if(referenceCountInteger!=null && referenceCountInteger.intValue()>1)	//if this property URI is used more than once
						{
							namespaceURIMultipleMap.put(namespaceURI, Boolean.TRUE);	//show that we've seen this namespace URI multiple times, because the resource itself is referenced multiple times
						}
						else	//if this property URI is only used once or not at all
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
				}
			}
				//generate beginning labeled namespace URIs
			final TURFNamespaceLabelManager namespaceLabelManager=getNamespaceLabelManager();	//get the namespace prefix manager
			for(final Map.Entry<URI, Boolean> namespaceURIMultipleEntry:namespaceURIMultipleMap.entrySet())	//for each namespace URI entry
			{
				final URI namespaceURI=namespaceURIMultipleEntry.getKey();	//get the namespace URI
				if(DEFAULT_NAMESPACE_URI.equals(namespaceURI))	//don't generate associations for the default namespace
				{
					continue;
				}
				//if this namespace URI is used more than one time, or if this is a namespace URI we specifically know is a namespace URI
				if(Boolean.TRUE.equals(namespaceURIMultipleEntry.getValue()) || namespaceLabelManager.isRecognized(namespaceURI))
				{
					if(startedProperties)	//if we already started the preamble
					{
						writeListSeparator(appendable);	//write a list separator
					}
					else	//if we haven't started the properties, yet
					{
						appendable.append(PROPERTIES_BEGIN);	//start the properties
						indent();	//indent the properties
						writeNewLine(appendable);
						startedProperties=true;	//show that we've started the properties
					}
					final String prefix=namespaceLabelManager.determineNamespaceLabel(namespaceURI);	//get a namespace label for the URI
					assert prefix!=null;	//the only namespace that should have no prefix is the default namespace, which we should already have skipped
					appendString(appendable, prefix);	//write the prefix
					appendable.append(NAMESPACE_ASSOCIATION_DELIMITER);	//write the property-value delimiter
					appendURI(appendable, namespaceURI);	//write the URI
				}
			}
			if(startedProperties)	//if we have a preamble
			{
				unindent();
				writeNewLine(appendable);
				appendable.append(PROPERTIES_END);	//end the properties
			}
			appendable.append(COMMUNITY_BEGIN);	//start the instance community
			indent();	//indent the resources
			writeNewLine(appendable);
		}
		else if(isPropertyNamespacePrefixesForced)	//if we are not generating a container, but we still want property namespace prefixes, generate prefixes for all property namespaces, regardless of how often they are used
		{
			for(final URI propertyURI:referenceSummary.propertyURIReferenceMap.keySet())	//look at all property URIs
			{
				final URI namespaceURI=getNamespaceURI(propertyURI);	//get the namespace URI of this property URI
				if(!DEFAULT_NAMESPACE_URI.equals(namespaceURI))	//ignore the default namespace
				{
					namespaceLabelManager.determineNamespaceLabel(namespaceURI);	//generate a namespace label for the URI
				}
			}
		}
			//generate the primary resource
		if(primaryResource!=null)	//if there is a primary resource to generate
		{
			generateRootResource(appendable, urf, referenceSummary, primaryResource);	//generate the primary resource
		}
		final List<URFResource> resourceList=new ArrayList<URFResource>();	//create a list of resources
		addAll(resourceList, urf.getResources());	//add all the resources in the data model to the list
		sort(resourceList, reverseOrder(RESOURCE_PROPERTY_COUNT_COMPARATOR));	//sort the resources in reverse order of their number of properties, so that we'll have a bigger chance of inlining resources
		for(final URFResource resource:resourceList)	//iterate over all the resources, generating only those with no references (i.e. root resources)
		{
			if(!isGenerated(resource) && !referenceSummary.objectReferenceMap.hasItems(resource))	//if this resource has not yet been generated and the resource has no references to it
			{
				generateRootResource(appendable, urf, referenceSummary, resource);	//generate this root resource
			}
		}
		for(final URFResource resource:resourceList)	//iterate over all the remaining resources, generating any that have not yet been generated
		{
			if(!isGenerated(resource))	//if this resource has not yet been generated
			{
				generateRootResource(appendable, urf, referenceSummary, resource);	//generate this root resource
			}
		}
		if(generateContainer)	//if we generated a signature
		{
			unindent();
			writeNewLine(appendable);
			appendable.append(COMMUNITY_END);	//end the instance community
		}
		reset();	//reset the generator to conserve memory
		return appendable;	//return the appendable
	}

	/**Generates a single top-level resource, prepending a list delimiter if appropriate.
	The resource is noted as having been generated.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param referenceSummary A summary of scope references to resources and property URIs.
	@param resource The resource to generate.
	@return The appendable.
	@exception NullPointerException if the given appendable, URF data model, reference map, and/or resource is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	protected <A extends Appendable> A generateRootResource(final A appendable, final URF urf, final URF.ReferenceSummary referenceSummary, final URFResource resource) throws IOException
	{
		if(getGeneratedResourceCount()>0)	//if we've already generated a least one resource
		{
			writeListSeparator(appendable);	//write a list separator
		}
		return generateResource(appendable, urf, referenceSummary, resource);	//generate the resource
	}

	/**Generates a single resource.
	The resource is noted as having been generated.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param referenceSummary A summary of scope references to resources and property URIs.
	@param resource The resource to generate.
	@return The appendable.
	@exception NullPointerException if the given appendable, URF data model, reference map, and/or resource is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	protected <A extends Appendable> A generateResource(final A appendable, final URF urf, final URF.ReferenceSummary referenceSummary, final URFResource resource) throws IOException
	{
		return generateResource(appendable, urf, referenceSummary, null, null, resource, false);	//generate the single resource; there is no context URI
	}

	/**Generates a single resource with no default namespace URI.
	The resource is noted as having been generated.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param referenceSummary A summary of scope references to resources and property URIs.
	@param subjectScope The scope to which the property and resource belongs, or <code>null</code> if the current resource is not in an object context.
	@param scopePropertyURI The predicate for which the resource is a value, or <code>null</code> if the current resource is not in an object context.
	@param resource The resource to generate.
	@param inSequence Whether the resource being generated is in a sequence and its scoped order properties should therefore not be generated.
	@return The appendable.
	@exception NullPointerException if the given appendable, URF data model, reference map, and/or resource is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	protected <A extends Appendable> A generateResource(final A appendable, final URF urf, final URF.ReferenceSummary referenceSummary, final URFScope subjectScope, final URI scopePropertyURI, final URFResource resource, final boolean inSequence) throws IOException
	{
		final boolean isGenerated=isGenerated(resource);	//see if this resource has already been generated
		if(!isGenerated)	//if the resource hasn't been generated, yet
		{
			setGenerated(resource, true);	//note that the resource has been generated so that any recursive references won't regenerate the entire resoure again
		}
		boolean generatedComponent=false;	//we haven't generated any components, yet
		URI inlineTypeURI=null;	//the inline namespace type URI, if any
		final URI uri=resource.getURI();	//get the resource URI
		String label=getLabel(resource);	//see if there is a label for this resource
		if(label==null && uri==null && !isGenerated)	//if there is no label or URI for this resource and the resource hasn't yet been generated
		{
			final Set<URFScope> referringScopes=referenceSummary.objectReferenceMap.get(resource);	//see how many scopes reference this resource
			if(referringScopes!=null && !referringScopes.isEmpty() && !(referringScopes.size()==1 && referringScopes.iterator().next()==subjectScope))	//if there are multiple referring scopes, or just a single referring scope that isn't the scope subject in this context
			{
				label=determineLabel(resource);	//locate a label for this resource, because it has references
			}
		}
		if(label!=null)	//if there is a label
		{
			appendLabel(appendable, label);	//write the label
		}
			//reference
		if(uri!=null && (!isGenerated || label==null))	//if there is a URI (don't show the URI if the resource is already generated and we already generated a label here)
		{
			if(isInlineURI(uri))	//if this URI is in an inline namespace
			{
				inlineTypeURI=getInlineTypeURI(uri);	//get the inline type of the URI so that we don't generate it again
			}
			generateReference(appendable, urf, uri);	//write a reference for the resource
			generatedComponent=true;	//indicate that we generated a component
		}

		if(isGenerated)	//if we've already generated this resource, see if there are any scoped properties before leaving
		{
			if(subjectScope!=null && scopePropertyURI!=null)	//if this resource is the value of a property
			{
				final URFScope scope=subjectScope.getScope(scopePropertyURI, resource);	//get the scope for this value
				if(scope==null)	//if there is no such scope
				{
					throw new IllegalArgumentException("No scope for given subject "+subjectScope+" and predicate URI "+scopePropertyURI);
				}
				if(generateProperties(appendable, urf, referenceSummary, scope, true, 0, true, true, true, true, true, true, true, !inSequence)>0)	//generate all scoped properties, suppressing generation of scoped order if we are in a sequence; if we generated any scoped properties
				{
					unindent();
					writeNewLine(appendable);
					appendable.append(PROPERTIES_END);	//end the properties declaration
				}
			}
			return appendable;	//there's no need to generate the resource again; we've already generated a label or a URI to represent it
		}
		boolean isProposition=false;	//we'll see if this is a proposition
		boolean isList=false;	//we'll see if this is a list
		boolean hasNonListType=inlineTypeURI!=null && !LIST_CLASS_URI.equals(inlineTypeURI);	//we'll see if there is a type that is not a list
		boolean isSet=false;	//we'll see if this is a set
		boolean hasNonSetType=inlineTypeURI!=null && !SET_CLASS_URI.equals(inlineTypeURI);	//we'll see if there is a type that is not a set
		boolean isMap=false;	//we'll see if this is a map
		boolean hasNonMapType=inlineTypeURI!=null && !MAP_CLASS_URI.equals(inlineTypeURI);	//we'll see if there is a type that is not a map
		for(final URFResource type:resource.getTypes())	//look through all the types
		{
			final URI typeURI=type.getURI();	//get the type URI
			if(PROPOSITION_CLASS_URI.equals(typeURI))	//if this is the proposition type
			{
				isProposition=true;	//this is a prosition
				hasNonListType=true;	//there is a non-list type
				hasNonSetType=true;	//there is a non-set type
				hasNonMapType=true;	//there is a non-map type
			}
			else if(LIST_CLASS_URI.equals(typeURI))	//if this is the list type
			{
				isList=true;	//this is a list
				hasNonSetType=true;	//there is a non-set type
				hasNonMapType=true;	//there is a non-map type
			}
			else if(SET_CLASS_URI.equals(typeURI))	//if this is the set type
			{
				isSet=true;	//this is a set
				hasNonListType=true;	//there is a non-list type
				hasNonMapType=true;	//there is a non-map type
			}
			else if(MAP_CLASS_URI.equals(typeURI))	//if this is the map type
			{
				isMap=true;	//this is a map
				hasNonListType=true;	//there is a non-list type
				hasNonSetType=true;	//there is a non-set type
			}
			else	//if this is not one of the short types addressed here
			{
				hasNonListType=true;	//there is a non-list type
				hasNonSetType=true;	//there is a non-set type
				hasNonMapType=true;	//there is a non-map type
			}
		}
		final URFResource subject;	//the subject property value, if this is a proposition
		final URFResource predicate;	//the predicate property value, if this is a proposition
		final URFResource object;	//the object property value, if this is a proposition
		boolean isPropositionShortForm=isShortPropositionsGenerated() && isProposition;	//generate a proposition short form if this is a proposition
		if(isPropositionShortForm)	//if we think this is a proposition short form, make sure there is a subject, predicate, and object
		{
			subject=resource.getPropertyValue(SUBJECT_PROPERTY_URI);	//get the subject
			predicate=resource.getPropertyValue(PREDICATE_PROPERTY_URI);	//get the predicate
			object=resource.getPropertyValue(OBJECT_PROPERTY_URI);	//get the object
			if(subject==null || predicate==null || object==null)	//if we are missing the subject, predicate, or object
			{
				isPropositionShortForm=false;	//we can't do a proposition short form
			}
		}
		else	//if this is not a proposition short form
		{
			subject=predicate=object=null;	//we don't need to get subject, predicate, and object values now
		}
		final boolean isListShortForm=isShortListsGenerated() && (isList || (resource.hasNamespaceProperty(ORDINAL_NAMESPACE_URI) && hasNonListType));	//generate a list short form if this is a list or it has properties in the ordinal namespace (but only if there is some other type already present; otherwise, it would introduce a list type where none was specified)
		final boolean isSetShortForm=isShortSetsGenerated() && (isSet || (resource.hasProperty(ELEMENT_PROPERTY_URI) && (hasNonSetType || isListShortForm)));	//generate a set short form if this is a set or it has element properties (but only if there is some other type; otherwise, it would introduce a set type where none was specified)
		final boolean isMapShortForm=isShortMapsGenerated() && (isMap || (resource.hasProperty(ENTRY_PROPERTY_URI) && (hasNonMapType || isListShortForm || isSetShortForm)));	//generate a map short form if this is a map or it has entry properties (but only if there is some other type; otherwise, it would introduce a map type where none was specified)
			//type
		final boolean isShortTypesGenerated=isShortTypesGenerated();	//see if we should generate types in the short form
		if(isShortTypesGenerated)	//if we should generate type short forms
		{
			long shortTypeCount=0;	//keep track of how many short-form types we've generated, if any
			for(final URFProperty typeProperty:resource.getProperties(TYPE_PROPERTY_URI))	//look at each type
			{
				final URFResource type=typeProperty.getValue();	//get the type value
				final URI typeURI=type.getURI();	//get the URI of this type
				if(typeURI!=null)	//if the given type has a URI
				{
					if(typeURI.equals(inlineTypeURI))	//if this is the same type URI as the URI included in the inline namespace type URI, if any
					{
						continue;	//skip this type
					}
					else if(isPropositionShortForm && PROPOSITION_CLASS_URI.equals(typeURI))	//if we're using a proposition short form and this is the proposition type, skip the type
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
					else if(isMapShortForm && !isListShortForm && !isSetShortForm && MAP_CLASS_URI.equals(typeURI))	//if we're using a map short form and this is the map type, skip the type, unless there is a list or set short form, which would negate the map short type, making us need to show it explicitly
					{
						continue;	//skip this type
					}
					final URFScope typeScope=typeProperty.getScope();	//get the type scope
					final URFListResource<?> selector=asListInstance(typeScope.getPropertyValue(SELECTOR_PROPERTY_URI));	//get the selector list, if any
					final long selectorCount=typeScope.getPropertyValueCount(SELECTOR_PROPERTY_URI);	//see how many selectors there are for this type
					if((selector==null && selectorCount!=0) || selectorCount>1)	//if the selector is not a list, or there is more than one selector
					{
						continue;	//we cannot generate a short type for this type
					}
					appendable.append(TYPE_BEGIN);	//append the type delimiter
					generateReference(appendable, urf, typeURI);	//generate a reference to the type
					markReferenceGenerated(urf, TYPE_PROPERTY_URI);	//mark that the type property was generated unless it has some other quality needed to be generated separately
					markReferenceGenerated(urf, typeURI);	//mark that the type was generated unless it has some other quality needed to be generated separately
					generatedComponent=true;	//indicate that we generated a component
					++shortTypeCount;	//show that we've got another short type
						//selector
					if(selector!=null)	//if there is a selector for this type
					{
						markReferenceGenerated(urf, SELECTOR_PROPERTY_URI);	//mark that the selector property was generated unless it has some other quality needed to be generated separately
						setGenerated(selector, true);	//note that the selector list has been generated TODO make sure the selector list has no other properties that need to be generated
						generateCollection(appendable, urf, referenceSummary, selector.getNamespaceProperties(ORDINAL_NAMESPACE_URI).iterator(), SELECTOR_BEGIN, SELECTOR_END, false);	//generate all the values of ordinal properties in the selector list on the same line
					}
				}
				//TODO make sure types without URIs get generated in the properties section
			}
		}
			//superclass
		final boolean isShortSuperclassesGenerated=isShortSuperclassesGenerated();	//see if we should generate superclasses in the short form
		if(isShortSuperclassesGenerated)	//if we should generate superclass short forms
		{
			for(final URFProperty superclassProperty:resource.getProperties(SUBCLASS_OF_PROPERTY_URI))	//look at each superclass
			{
				final URFResource superclass=superclassProperty.getValue();	//get the superclass value
				final URI superclassURI=superclass.getURI();	//get the URI of this superclass
				if(superclassURI!=null)	//if the given superclass has a URI
				{
					appendable.append(SUBCLASS_OF_BEGIN);	//append the superclass delimiter
					generateReference(appendable, urf, superclassURI);	//generate a reference to the superclass
					markReferenceGenerated(urf, SUBCLASS_OF_PROPERTY_URI);	//mark that the superclass property was generated unless it has some other quality needed to be generated separately
					markReferenceGenerated(urf, superclassURI);	//mark that the superclass was generated unless it has some other quality needed to be generated separately
					generatedComponent=true;	//indicate that we generated a component
				}
				//TODO make sure superclasses without URIs get generated in the properties section
			}
		}
			//interface
		final boolean isShortInterfacesGenerated=isShortInterfacesGenerated();	//see if we should generate interfaces in the short form
		if(isShortInterfacesGenerated)	//if we should generate interface short forms
		{
			for(final URFProperty interfaceProperty:resource.getProperties(IMPLEMENTATION_OF_PROPERTY_URI))	//look at each interface
			{
				final URFResource interfaceResource=interfaceProperty.getValue();	//get the interface value
				final URI interfaceURI=interfaceResource.getURI();	//get the URI of this interface
				if(interfaceURI!=null)	//if the given interface has a URI
				{
					appendable.append(IMPLEMENTATION_OF_BEGIN);	//append the interface delimiter
					generateReference(appendable, urf, interfaceURI);	//generate a reference to the interface
					markReferenceGenerated(urf, IMPLEMENTATION_OF_PROPERTY_URI);	//mark that the interface property was generated unless it has some other quality needed to be generated separately
					markReferenceGenerated(urf, interfaceURI);	//mark that the interface was generated unless it has some other quality needed to be generated separately
					generatedComponent=true;	//indicate that we generated a component
				}
				//TODO make sure interfaces without URIs get generated in the properties section
			}
		}
			//properties
		int propertyCount=0;	//start with no properties being generating
		propertyCount=generateProperties(appendable, urf, referenceSummary, resource, false, propertyCount, !isShortTypesGenerated, !isShortSuperclassesGenerated, !isShortInterfacesGenerated, !isPropositionShortForm, !isListShortForm, !isSetShortForm, !isMapShortForm, true);	//generate properties
		if(subjectScope!=null && scopePropertyURI!=null)	//if this resource is the value of a property
		{
			final URFScope scope=subjectScope.getScope(scopePropertyURI, resource);	//get the scope for this value
			if(scope==null)	//if there is no such scope
			{
				throw new IllegalArgumentException("No scope for given subject "+subjectScope+" and predicate URI "+scopePropertyURI);
			}
			propertyCount=generateProperties(appendable, urf, referenceSummary, scope, true, propertyCount, true, true, true, true, true, true, true, !inSequence);	//generate all scoped properties, suppressing generation of scoped order if we are in a sequence
		}
		if(propertyCount>0)	//if we started the properties section
		{
			unindent();
			writeNewLine(appendable);
			appendable.append(PROPERTIES_END);	//end the properties declaration
		}
		else if(!generatedComponent && !isPropositionShortForm && !isListShortForm && !isSetShortForm && !isMapShortForm)	//if we haven't generated any components, and there will be no proposition, list, set, and/or map short form, have a properties section even if it's empty
		{
			appendable.append(PROPERTIES_BEGIN).append(PROPERTIES_END);	//write an empty properties declaration
		}
			//TODO implement community short form
			//proposition
		if(isPropositionShortForm)	//if we should generate a proposition short form
		{
			assert subject!=null && predicate!=null && object!=null : "Because this is a proposition short form, we should have already made sure the subject, predicate, and object are all non-null.";
			markReferenceGenerated(urf, PROPOSITION_CLASS_URI);	//mark that the proposition type was generated unless it has some other quality needed to be generated separately
			appendable.append(PROPOSITION_BEGIN);	//start the proposition
			markReferenceGenerated(urf, SUBJECT_PROPERTY_URI);	//mark that the subject property was generated unless it has some other quality needed to be generated separately
			generateResource(appendable, urf, referenceSummary, resource, SUBJECT_PROPERTY_URI, subject, false);	//generate the subject
			appendable.append(LIST_DELIMITER);	//separate the properties
			markReferenceGenerated(urf, PREDICATE_PROPERTY_URI);	//mark that the predicate property was generated unless it has some other quality needed to be generated separately
			generateResource(appendable, urf, referenceSummary, resource, PREDICATE_PROPERTY_URI, predicate, false);	//generate the predicate
			appendable.append(LIST_DELIMITER);	//separate the properties
			markReferenceGenerated(urf, OBJECT_PROPERTY_URI);	//mark that the object property was generated unless it has some other quality needed to be generated separately
			generateResource(appendable, urf, referenceSummary, resource, OBJECT_PROPERTY_URI, object, false);	//generate the object
			appendable.append(PROPOSITION_END);	//end the proposition
		}
			//list
		if(isListShortForm)	//if we should generate a list short form
		{
			markReferenceGenerated(urf, LIST_CLASS_URI);	//mark that the list type was generated unless it has some other quality needed to be generated separately
			generateCollection(appendable, urf, referenceSummary, resource.getNamespaceProperties(ORDINAL_NAMESPACE_URI).iterator(), LIST_BEGIN, LIST_END, true);	//generate all the values of ordinal properties in the list, using multiple lines if there are elements
		}
			//set
		if(isSetShortForm)	//if we should generate a set short form
		{
			markReferenceGenerated(urf, SET_CLASS_URI);	//mark that the set type was generated unless it has some other quality needed to be generated separately
			generateCollection(appendable, urf, referenceSummary, resource.getProperties(ELEMENT_PROPERTY_URI).iterator(), SET_BEGIN, SET_END, true);	//generate all the values of element properties in the set, using multiple lines if there are elements
		}
			//map
		if(isMapShortForm)	//if we should generate a map short form
		{
			markReferenceGenerated(urf, MAP_CLASS_URI);	//mark that the map type was generated unless it has some other quality needed to be generated separately
			generateMap(appendable, urf, referenceSummary, resource.getProperties(ENTRY_PROPERTY_URI).iterator(), MAP_BEGIN, MAP_END, true);	//generate all the values of entry properties in the map, using multiple lines if there are entries
		}
		return appendable;	//return the appendable
	}

	/**Generates the elements of a list or set.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param referenceSummary A summary of scope references to resources and property URIs.
	@param elementPropertyIterator An iterator to the elements of the resource.
	@param collectionBegin The beginning delimiter of the collection.
	@param collectionEnd The end delimiter of the collection.
	@param multipleLines Whether multiple lines should be used if elements are present.
	@return The appendable.
	@exception NullPointerException if the given appendable, URF data model, reference map, and/or element property iterator is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	protected <A extends Appendable> A generateCollection(final A appendable, final URF urf, final URF.ReferenceSummary referenceSummary, final Iterator<URFProperty> elementPropertyIterator, final char collectionBegin, final char collectionEnd, final boolean multipleLines) throws IOException
	{
		if(elementPropertyIterator.hasNext())	//if there are elements in the collection
		{
			appendable.append(collectionBegin);	//start the collection
			if(multipleLines)	//if we should generate multiple lines
			{
				indent();	//indent the collection
				writeNewLine(appendable);
			}
			int elementCount=0;	//keep track of how many elements we have
			while(elementPropertyIterator.hasNext())	//while there are elements
			{
				final URFProperty elementProperty=elementPropertyIterator.next();	//get the next element property
				if(elementCount>0)	//if we've already generated an element
				{
					if(multipleLines)	//if we should generate multiple lines
					{
						writeListSeparator(appendable);	//write a list separator, which will write multiple lines (also using a comma, if so configured), unless formatting is turned off, in which case only a list delimiter will be used
					}
					else
					{
						appendable.append(LIST_DELIMITER);	//separate the elements using a list delimiter
					}
				}
				generateResource(appendable, urf, referenceSummary, elementProperty.getSubjectScope(), elementProperty.getPropertyURI(), elementProperty.getValue(), false);	//generate the element; each element value is an object of the element predicate, so use the element property URI as the context
				++elementCount;	//show that we generated another list element
			}
			if(multipleLines)	//if we should generate multiple lines
			{
				unindent();
				writeNewLine(appendable);
			}
			appendable.append(collectionEnd);	//end the collection
		}
		else	//if there are no elements in the collection
		{
			appendable.append(collectionBegin).append(collectionEnd);	//show the empty collection on the same line
		}
		return appendable;	//return the appendable
	}

	/**Generates the entries of a map.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param referenceSummary A summary of scope references to resources and property URIs.
	@param entryPropertyIterator An iterator to the entries of the map.
	@param mapBegin The beginning delimiter of the map.
	@param mapEnd The end delimiter of the map.
	@param multipleLines Whether multiple lines should be used if entries are present.
	@return The appendable.
	@exception NullPointerException if the given appendable, URF data model, reference map, and/or entry property iterator is <code>null</code>.
	@exception IOException if there is an error writing to the appendable.
	*/
	protected <A extends Appendable> A generateMap(final A appendable, final URF urf, final URF.ReferenceSummary referenceSummary, final Iterator<URFProperty> entryPropertyIterator, final char mapBegin, final char mapEnd, final boolean multipleLines) throws IOException
	{
		if(entryPropertyIterator.hasNext())	//if there are entries in the collection
		{
			appendable.append(mapBegin);	//start the map
			if(multipleLines)	//if we should generate multiple lines
			{
				indent();	//indent the collection
				writeNewLine(appendable);
			}
			int entryCount=0;	//keep track of how many entries we have
			while(entryPropertyIterator.hasNext())	//while there are entries
			{
				final URFProperty entryProperty=entryPropertyIterator.next();	//get the next entry property
				if(entryCount>0)	//if we've already generated an entry
				{
					if(multipleLines)	//if we should generate multiple lines
					{
						writeListSeparator(appendable);	//write a list separator, which will write multiple lines (also using a comma, if so configured), unless formatting is turned off, in which case only a list delimiter will be used
					}
					else
					{
						appendable.append(LIST_DELIMITER);	//separate the entries without newlines
					}
				}
				final URFResource entry=entryProperty.getValue();	//get the entry, which is the subject of the key and value 
				final URFResource key=entry.getPropertyValue(KEY_PROPERTY_URI);	//get the key 
				final URFResource value=entry.getPropertyValue(VALUE_PROPERTY_URI);	//get the value
				if(key!=null && value!=null)	//if the entry has both a key and a value
				{
					generateResource(appendable, urf, referenceSummary, entry, KEY_PROPERTY_URI, key, false);	//generate the key
					markReferenceGenerated(urf, KEY_PROPERTY_URI);	//mark that the key property was generated unless it has some other quality needed to be generated separately
					appendable.append(PROPERTY_VALUE_DELIMITER);	//=
					generateResource(appendable, urf, referenceSummary, entry, VALUE_PROPERTY_URI, value, false);	//generate the value
					setGenerated(entry, true);	//note that the entry has been generated TODO make sure the entry has no other properties that need to be generated
					markReferenceGenerated(urf, VALUE_PROPERTY_URI);	//mark that the value property was generated unless it has some other quality needed to be generated separately
					markReferenceGenerated(urf, TYPE_PROPERTY_URI);	//mark that the type property was generated unless it has some other quality needed to be generated separately
					markReferenceGenerated(urf, MAP_ENTRY_CLASS_URI);	//mark that the map entry class was generated unless it has some other quality needed to be generated separately
					++entryCount;	//show that we generated another map entry
				}
//TODO fix				else	//if the entry isn't complete
			}
			if(multipleLines)	//if we should generate multiple lines
			{
				unindent();
				writeNewLine(appendable);
			}
			appendable.append(mapEnd);	//end the map
		}
		else	//if there are no entries in the map
		{
			appendable.append(mapBegin).append(mapEnd);	//show the empty map on the same line
		}
		return appendable;	//return the appendable
	}

	/**Generates the properties, if any, of a given scope.
	The beginning property section delimiter will be added if needed, but no ending property section delimiter will be generated.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param referenceSummary A summary of scope references to resources and property URIs.
	@param scope The scope the properties of which should be generated.
	@param scoped Whether the given properties are scoped.
	@param propertyCount the number of properties already generated; used to determine whether a new properties section should be generated.
	@param generateTypes Whether type properties should be generated.
	@param generateSuperclasses Whether superclass properties should be generated.
	@param generateInterfaces Whether interface properties should be generated.
	@param generateProposition Whether proposition-related properties (subject, predicate, and object) should be generated.
	@param generateOrdinals Whether properties in the ordinal namespace should be generated.
	@param generateElements Whether the element property should be generated.
	@param generateEntries Whether the entry property should be generated.
	@param generateOrder Whether the order property should be generated; if order properties are not generated, their values will be marked at generated if they have no other properties needed to be generated.
	@return The new total number of properties generated, including the properties already generated before this method was called.
	@exception NullPointerException if the given appendable, URF data model, reference map, and/or scope is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	*/
	protected <A extends Appendable> int generateProperties(final A appendable, final URF urf, final URF.ReferenceSummary referenceSummary, final URFScope scope, final boolean scoped, int propertyCount, final boolean generateTypes, final boolean generateSuperclasses, final boolean generateInterfaces, final boolean generateProposition, final boolean generateOrdinals, final boolean generateElements, final boolean generateEntries, final boolean generateOrder) throws IOException
	{
		URI sequencePropertyURI=null;	//this will indicate when we're in the middle of a sequence for a particular property
		for(final URFProperty property:scope.getProperties())	//look at each property
		{
			final URFResource value=property.getValue();	//get the property value
			final URI propertyURI=property.getPropertyURI();	//get the property URI
			if((!generateTypes && TYPE_PROPERTY_URI.equals(propertyURI))	//if we shouldn't generate types and this is a type
				|| (!generateSuperclasses && SUBCLASS_OF_PROPERTY_URI.equals(propertyURI))	//if we shouldn't generate superclasses and this is a superclass
				|| (!generateInterfaces && IMPLEMENTATION_OF_PROPERTY_URI.equals(propertyURI))	//if we shouldn't generate interfacesand this is a interface
				|| (!generateProposition && (SUBJECT_PROPERTY_URI.equals(propertyURI) || PREDICATE_PROPERTY_URI.equals(propertyURI) || OBJECT_PROPERTY_URI.equals(propertyURI)))	//or if we shouldn't generate proposition properties and this property is a subject, predicate, or object
				|| (!generateOrdinals && propertyURI!=null && ORDINAL_NAMESPACE_URI.equals(getNamespaceURI(propertyURI)))	//or if we shouldn't generate ordinals and this property is an ordinal
				|| (!generateElements && propertyURI!=null && ELEMENT_PROPERTY_URI.equals(propertyURI))	//or if we shouldn't generate elements and this property is an element property
				|| (!generateEntries && propertyURI!=null && ENTRY_PROPERTY_URI.equals(propertyURI))	//or if we shouldn't generate entries and this property is an entry property
				|| (!generateOrder && ORDER_PROPERTY_URI.equals(propertyURI)))	//or if we shouldn't generate order and this is an order property
			{
				markReferenceGenerated(urf, propertyURI);	//mark that this property was generated unless it has some other quality needed to be generated separately
				if(!generateOrder && ORDER_PROPERTY_URI.equals(propertyURI))	//if this was the order property
				{
					final URI valueURI=value.getURI();	//get the URI of the value, if any
					if(valueURI!=null)	//if there is a value URI
					{
						markReferenceGenerated(urf, valueURI);	//mark that the value was generated unless it has some other quality needed to be generated separately
						markReferenceGenerated(urf, INTEGER_CLASS_URI);	//the order value should be an integer; mark that the integer class was generated unless it has some other quality needed to be generated separately
					}
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
					appendable.append(SEQUENCE_END);	//end the sequence for this property
					sequencePropertyURI=null;	//indicate that we are no longer in a sequence for a property
				}
			}
			if(sequencePropertyURI==null)	//if we're not in the middle of a sequence
			{
				if(propertyCount>0)	//if we've already generated a property
				{
					writeListSeparator(appendable);	//write a list separator
				}
				else	//if we haven't yet started the properties section
				{
					appendable.append(PROPERTIES_BEGIN);	//start the properties declaration
					indent();	//indent the properties
					writeNewLine(appendable);
				}
				generateReference(appendable, urf, propertyURI);	//generate the reference of the property
				markReferenceGenerated(urf, propertyURI);	//mark that this property was generated unless it has some other quality needed to be generated separately
				if(scoped)	//if this is a scoped property
				{
					appendable.append(SCOPE_DELIMITER);	//`
				}
				appendable.append(PROPERTY_VALUE_DELIMITER);	//=
				if(property.getScope().getOrder()!=null)	//if this property has a sequence
				{
					appendable.append(SEQUENCE_BEGIN);	//start a sequence for this property
					sequencePropertyURI=propertyURI;	//indicate that we should have a sequence for this property
					generateResource(appendable, urf, referenceSummary, scope, propertyURI, value, true);	//generate the property value, indicating that the value is an element in a sequence
				}
				else	//if this property has no sequence
				{
					generateResource(appendable, urf, referenceSummary, scope, propertyURI, value, false);	//generate the property value normally, using the property URI as the context URI
				}
			}
			else	//if we are still in the middle of a sequence
			{
				appendable.append(LIST_DELIMITER);	//separate the values in the sequence
				generateResource(appendable, urf, referenceSummary, scope, propertyURI, value, true);	//generate the property value, indicating that the value is an element in a sequence, and using the property URI as the context URI
			}
			++propertyCount;	//show that we generated another property
		}
		if(sequencePropertyURI!=null)	//if we were in the middle of a sequence
		{
			appendable.append(SEQUENCE_END);	//end the sequence for this property, as we're out of properties
		}
		return propertyCount;	//return the new property count
	}

	/**Creates and returns a reference string to a resource with the given URI.
	A name reference or short form will be used if appropriate.
	No prefix will be determined if one is not already available for the namespace URI, if any, of the given resource URI.
	@param uri The URI of the resource.
	@param namespaceLabelManager The manager responsible for generating namespace labels if needed.
	@return A string reference to the resource with the given URI.
	@exception NullPointerException if the given URI and/or namespace label manager is <code>null</code>.
	@see #generateURIReference(Appendable, URI, TURFNamespaceLabelManager, URI, boolean)
	*/
	public static String createReferenceString(final URI uri, final TURFNamespaceLabelManager namespaceLabelManager)
	{
		return createReferenceString(uri, namespaceLabelManager, null);	//create a reference string with no base URI
	}

	/**Creates and returns a reference string to a resource with the given URI.
	A name reference or short form will be used if appropriate.
	No prefix will be determined if one is not already available for the namespace URI, if any, of the given resource URI.
	@param uri The URI of the resource.
	@param namespaceLabelManager The manager responsible for generating namespace labels if needed.
	@param baseURI The base URI of the URF data model, or <code>null</code> if the base URI is unknown.
	@return A string reference to the resource with the given URI.
	@exception NullPointerException if the given URI and/or namespace label manager is <code>null</code>.
	@see #generateURIReference(Appendable, URI, TURFNamespaceLabelManager, URI, URI)
	*/
	public static String createReferenceString(final URI uri, final TURFNamespaceLabelManager namespaceLabelManager, final URI baseURI)
	{
		return createReferenceString(uri, namespaceLabelManager, baseURI, false);	//create a reference string without generating any prefixes
	}

	/**Creates and returns a reference string to a resource with the given URI.
	A name reference or short form will be used if appropriate.
	@param uri The URI of the resource.
	@param namespaceLabelManager The manager responsible for generating namespace labels if needed.
	@param baseURI The base URI of the URF data model, or <code>null</code> if the base URI is unknown.
	@param determinePrefix <code>true</code> if a prefix should be determined if one is not available for the namespace URI, if any, of the given resource URI.
	@return A string reference to the resource with the given URI.
	@exception NullPointerException if the given URI and/or namespace label manager is <code>null</code>.
	@see #generateURIReference(Appendable, URI, TURFNamespaceLabelManager, URI, URI)
	*/
	public static String createReferenceString(final URI uri, final TURFNamespaceLabelManager namespaceLabelManager, final URI baseURI, final boolean determinePrefix)
	{
		try
		{
			return generateReference(new StringBuilder(), uri, namespaceLabelManager, baseURI, determinePrefix).toString();	//generate a reference using a string builder
		}
		catch(final IOException ioException)	//we should never get an I/O exception writing to a string builder
		{
			throw unexpected(ioException);
		}
	}

	/**Generates a reference to a resource with the given URI with no default namespace URI.
	A name reference or short form will be used if appropriate.
	If a reference to an inline URI is generated, the corresponding inline type URI will be marked as generated if it has no other qualities needed to be generated separately.
	No prefix will be determined if one is not already available for the namespace URI, if any, of the given resource URI.
	@param appendable The appendable used for generating the information.
	@param urf The URF data model.
	@param uri The URI of the resource.
	@exception NullPointerException if the given appendable and/or URI is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	@see #generateURIReference(Appendable, URI, TURFNamespaceLabelManager, URI, URI)
	@see #markReferenceGenerated(URF, URI)
	*/
	public <A extends Appendable> void generateReference(final A appendable, final URF urf, final URI uri) throws IOException
	{
		final URI inlineTypeURI=generateReference(appendable, uri, namespaceLabelManager, baseURI, false);	//generate a reference, keeping track of the inline type URI generated, if any
		if(inlineTypeURI!=null)	//if an inline URI was generated
		{
			markReferenceGenerated(urf, inlineTypeURI);	//mark that this inline type was generated unless it has some other quality needed to be generated separately
		}
	}

	/**Generates a reference to a resource with the given URI.
	A name reference or short form will be used if appropriate.
	@param appendable The appendable used for generating the information.
	@param uri The URI of the resource.
	@param namespaceLabelManager The manager responsible for generating namespace labels if needed.
	@param baseURI The base URI of the URF data model, or <code>null</code> if the base URI is unknown.
	@param determinePrefix <code>true</code> if a prefix should be determined if one is not available for the namespace URI, if any, of the given resource URI.
	@return The inline type URI, if a reference to an inline URI was generated, or <code>null</code> if no inline resource URI reference was generated.
	@exception NullPointerException if the given appendable URI, and/or namespace label manager is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	@see #generateURIReference(Appendable, URI, TURFNamespaceLabelManager, URI, URI)
	*/
	public static <A extends Appendable> URI generateReference(final A appendable, final URI uri, final TURFNamespaceLabelManager namespaceLabelManager, final URI baseURI, final boolean determinePrefix) throws IOException
	{
		URI inlineTypeURI=null;	//keep track of whether an inline type URI was generated
		if(isInlineURI(uri))	//if this URI is in an inline namespace
		{
			inlineTypeURI=getInlineTypeURI(uri);	//get the inline type of the URI so that we don't generate it again
			final String lexicalForm=getInlineLexicalForm(uri);	//get the lexical form of the inline type
			assert lexicalForm!=null : "An inline namespace URI should always have a lexical form.";
			if(BINARY_CLASS_URI.equals(inlineTypeURI))	//binary
			{
				appendable.append(BINARY_BEGIN).append(lexicalForm).append(BINARY_END);	//write the binary short form
				return inlineTypeURI;
			}
			else if(BOOLEAN_CLASS_URI.equals(inlineTypeURI))	//boolean
			{
				appendable.append(BOOLEAN_BEGIN).append(lexicalForm).append(BOOLEAN_END);	//write the boolean short form
				return inlineTypeURI;
			}
			else if(CHARACTER_CLASS_URI.equals(inlineTypeURI))	//character
			{
				appendable.append(CHARACTER_BEGIN).append(lexicalForm).append(CHARACTER_END);	//write the character short form
				return inlineTypeURI;
			}
			else if(DATE_CLASS_URI.equals(inlineTypeURI) || DATE_TIME_CLASS_URI.equals(inlineTypeURI)
					|| DURATION_CLASS_URI.equals(inlineTypeURI) || TIME_CLASS_URI.equals(inlineTypeURI)
					|| UTC_OFFSET_CLASS_URI.equals(inlineTypeURI))	//if this is a temporal
			{
				appendable.append(TEMPORAL_BEGIN).append(lexicalForm).append(TEMPORAL_END);	//write a temporal short form
				return inlineTypeURI;
			}
			else if(INTEGER_CLASS_URI.equals(inlineTypeURI) || RATIONAL_CLASS_URI.equals(inlineTypeURI))	//integer or rational
			{
				appendable.append(NUMBER_BEGIN).append(lexicalForm).append(NUMBER_END);	//write the number short form
				return inlineTypeURI;
			}
			else if(ORDINAL_CLASS_URI.equals(inlineTypeURI))	//ordinal
			{
				appendable.append(ORDINAL_BEGIN).append(lexicalForm).append(ORDINAL_END);	//write the ordinal short form
				return inlineTypeURI;
			}
			else if(REGULAR_EXPRESSION_CLASS_URI.equals(inlineTypeURI))	//if this is a regular expression
			{
				appendRegularExpression(appendable, lexicalForm);	//write the regular expression short form
				return inlineTypeURI;
			}
			else if(STRING_CLASS_URI.equals(inlineTypeURI))	//if this is a string
			{
				appendString(appendable, lexicalForm);	//write the string short form
				return inlineTypeURI;
			}
			else if(URI_CLASS_URI.equals(inlineTypeURI))	//if this is a URI
			{
				appendURI(appendable, lexicalForm);	//write the URI short form
				return inlineTypeURI;
			}
		}
		final URI namespaceURI=getNamespaceURI(uri);	//see if the URI has a namespace
		if(namespaceURI!=null)	//if there is a namespace
		{
			final boolean suppressPrefix=DEFAULT_NAMESPACE_URI.equals(namespaceURI);	//we could suppress the prefix if the namespace is the same as the default namespace
			final String prefix=suppressPrefix ? null : (determinePrefix ? namespaceLabelManager.determineNamespaceLabel(namespaceURI) : namespaceLabelManager.get(namespaceURI));	//see if we have a prefix for this namespace, but only if we shouldn't suppress prefixes
			if(prefix!=null || suppressPrefix)	//if we have a prefix, or we're suppressing this prefix
			{
				final String localName=getLocalName(uri);	//get the local name of the URI
				assert localName!=null : "If a URI has a namespace, it should have a local name as well.";
				if(prefix!=null)	//if there is a prefix
				{
					appendable.append(prefix).append(NAME_PREFIX_DELIMITER);	//prefix.
				}
				appendable.append(localName);	//prefix.localName
				return inlineTypeURI;	//return the inline type URI, if any
			}
		}
		generateURIReference(appendable, uri, namespaceLabelManager, baseURI, determinePrefix);	//generate the URI reference normally by default
		return inlineTypeURI;	//return the inline type URI, if any
	}

	/**Writes a URI reference to a resource with the given URI.
	@param appendable The appendable used for generating the information.
	@param uri The URI of the resource.
	No prefix will be determined if one is not already available for the namespace URI, if any, of the given resource URI.
	@exception NullPointerException if the given appendable and/or URI is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	@see #generateURIReference(Appendable, URI, TURFNamespaceLabelManager, URI)
	*/
	public <A extends Appendable> void generateURIReference(final A appendable, URI uri) throws IOException
	{
		generateURIReference(appendable, uri, getNamespaceLabelManager(), getBaseURI(), false);	//generate the URI reference using our own namespace label manager and base URI
	}

	/**Writes a URI reference to a resource with the given URI.
	@param appendable The appendable used for generating the information.
	@param uri The URI of the resource.
	@param namespaceLabelManager The manager responsible for generating namespace labels if needed.
	@param baseURI The base URI of the URF data model, or <code>null</code> if the base URI is unknown.
	@param determinePrefix <code>true</code> if a prefix should be determined if one is not available for the namespace URI, if any, of the given resource URI.
	@exception NullPointerException if the given appendable, URI, and/or namespace label manager is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	*/
	public static <A extends Appendable> void generateURIReference(final A appendable, URI uri, final TURFNamespaceLabelManager namespaceLabelManager, final URI baseURI, final boolean determinePrefix) throws IOException
	{
		appendable.append(REFERENCE_BEGIN);	//start the URI reference
		if(isInlineURI(uri))	//if this URI is in an inline namespace
		{
			final URI inlineTypeURI=getInlineTypeURI(uri);	//get the inline type of the URI
			final String lexicalForm=getInlineLexicalForm(uri);	//get the lexical form of the inline type
			assert lexicalForm!=null : "An inline namespace URI should always have a lexical form.";
			appendable.append(TYPE_BEGIN);	//start a type declaration
			generateReference(appendable, inlineTypeURI, namespaceLabelManager, baseURI, determinePrefix);	//generate a reference to the inline type, determining a new prefix if needed
			appendable.append(SELECTOR_BEGIN);	//start the selector
			appendString(appendable, lexicalForm);	//write the string lexical form			
			appendable.append(SELECTOR_END);	//end the selector
		}
		else	//if this URI is not in an inline namespace
		{
			if(baseURI!=null)	//if there is a base URI
			{
				uri=baseURI.relativize(uri);	//relativize the URI to the base URI if possible
			}
			appendable.append(uri.toString());	//write the relative URI
		}
		appendable.append(REFERENCE_END);	//end the URI reference
	}

	/**Marks a reference as generated if appropriate.
	If a reference is to a resource with no properties, or the reference is an inline URI and there is only a single type of the inline type,
	there will be no need to generate the resource in full form, so it will be marked as generated.
	This method is useful for marking property URI references as generated if they do not need to stand on their own.
	@param urf The URF data model.
	@param resourceURI The URI of the generated reference.
	@exception NullPointerException if the given URI is <code>null</code>.
	*/
	protected void markReferenceGenerated(final URF urf, final URI resourceURI)
	{
		final URFResource resource=urf.getResource(resourceURI);	//see if there is a resource for this URI in the data model
		if(resource!=null)	//if there is such a resource
		{
			resource.readLock().lock();	//lock the resource for reading
			try
			{
				final long propertyCount=resource.getPropertyValueCount();	//find out how many properties the resource has
				if(propertyCount>0)	//if there is more than one property, make sure it's not just a type of an inline URI
				{
					if(propertyCount!=1 || !isInlineURI(resourceURI))	//if there's not just one property, or this is not an inline URI
					{
						return;	//we shouldn't mark this resource as generated
					}
					final URI inlineTypeURI=getInlineTypeURI(resourceURI);	//find out the inline type of the URI
					if(!resource.hasTypeURI(inlineTypeURI))	//if there is another type besides the inline type, don't mark the resource as generated; otherwise, the type is redundance
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

	/**Appends a label to an appendable with appropriate delimiters.
	@param appendable The appendable to which the information should be appended.
	@param label The label to append.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or label is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	*/
	public static Appendable appendLabel(final Appendable appendable, final String label) throws IOException
	{
		return appendable.append(LABEL_BEGIN).append(checkInstance(label, "Label cannot be null.")).append(LABEL_END);	//append the label
	}

	/**Returns a label short form with appropriate delimiters.
	@param label The label to store in the string.
	@return The string containing the label short form.
	@exception NullPointerException if the given label is <code>null</code>.
	*/
	public static String toLabelString(final Appendable appendable, final String label)
	{
		try
		{
			return appendLabel(new StringBuilder(), label).toString();
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}
	
	/**Appends a regular expression to an appendable surrounded by the regular expression short form delimiters.
	The usual reserved string characters, along with the regular expression delimiter, will be escaped.
	@param appendable The appendable to which the information should be appended.
	@param regularExpression The regular expression to append.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or regular expression is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	@see TURF#REGULAR_EXPRESSION_BEGIN
	@see TURF#REGULAR_EXPRESSION_END
	*/
	public static Appendable appendRegularExpression(final Appendable appendable, final String regularExpression) throws IOException
	{
		return appendString(appendable, regularExpression, REGULAR_EXPRESSION_BEGIN, REGULAR_EXPRESSION_END);	//append the regular expression using the regular expression short form delimiters
	}

	/**Returns a regular expression short form surrounded by the regular expression short form delimiters.
	The usual reserved string characters, along with the regular expression delimiter, will be escaped.
	@param regularExpression The regular expression to store in the string.
	@return The string containing the regular expression short form.
	@exception NullPointerException if the given regular expression is <code>null</code>.
	@see TURF#REGULAR_EXPRESSION_BEGIN
	@see TURF#REGULAR_EXPRESSION_END
	*/
	public static String toRegularExpressionString(final String regularExpression)
	{
		try
		{
			return appendRegularExpression(new StringBuilder(), regularExpression).toString();
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}

	/**Appends a string to an appendable surrounded by the string short form delimiters.
	The usual reserved string characters, along with the string delimiter, will be escaped.
	@param appendable The appendable to which the information should be appended.
	@param string The string to append.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or string is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	@see TURF#STRING_BEGIN
	@see TURF#STRING_END
	*/
	public static Appendable appendString(final Appendable appendable, final String string) throws IOException
	{
		return appendString(appendable, string, STRING_BEGIN, STRING_END);	//append the string using the string short form delimiters
	}

	/**Returns a string short form surrounded by the string short form delimiters.
	The usual reserved string characters, along with the string delimiter, will be escaped.
	@param string The string to store in the string.
	@return The string containing the string short form.
	@exception NullPointerException if the given string is <code>null</code>.
	@see TURF#STRING_BEGIN
	@see TURF#STRING_END
	*/
	public static String toStringString(final String string)
	{
		try
		{
			return appendString(new StringBuilder(), string).toString();
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}

	/**Appends a URI to an appendable surrounded by the URI short form delimiters.
	@param appendable The appendable to which the information should be appended.
	@param uri The URI to append.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or URI is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	@see TURF#URI_BEGIN
	@see TURF#URI_END
	*/
	public static Appendable appendURI(final Appendable appendable, final URI uri) throws IOException
	{
		return appendURI(appendable, uri.toString());	//append the string form of the URI
	}

	/**Returns a URI short form surrounded by the URI short form delimiters.
	@param uri The URI to store in the string.
	@return The string containing the URI short form.
	@exception NullPointerException if the given URI is <code>null</code>.
	@see TURF#URI_BEGIN
	@see TURF#URI_END
	*/
	public static String toURIString(final URI uri)
	{
		try
		{
			return appendURI(new StringBuilder(), uri).toString();
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}

	/**Appends a resource reference to an appendable surrounded by the reference short form delimiters.
	@param appendable The appendable to which the information should be appended.
	@param referenceURI The URI of the reference to append.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or reference URI is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	@see TURF#REFERENCE_BEGIN
	@see TURF#REFERENCE_END
	*/
	public static Appendable appendReference(final Appendable appendable, final URI referenceURI) throws IOException
	{
		return appendable.append(REFERENCE_BEGIN).append(referenceURI.toString()).append(REFERENCE_END);	//append a reference short form
	}

	/**Returns a resource reference short form surrounded by the reference short form delimiters.
	@param uri The URI of the reference to store in the string.
	@return The string containing the reference short form.
	@exception NullPointerException if the given reference URI is <code>null</code>.
	@see TURF#REFERENCE_BEGIN
	@see TURF#REFERENCE_END
	*/
	public static String toReferenceString(final URI uri)
	{
		try
		{
			return appendReference(new StringBuilder(), uri).toString();
		}
		catch(final IOException ioException)
		{
			throw new AssertionError(ioException);
		}
	}
	
	/**Appends a URI to an appendable surrounded by the URI short form delimiters.
	@param appendable The appendable to which the information should be appended.
	@param uri The URI to append.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or URI is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	@see TURF#URI_BEGIN
	@see TURF#URI_END
	*/
	protected static Appendable appendURI(final Appendable appendable, final String uri) throws IOException
	{
		return appendable.append(URI_BEGIN).append(uri).append(URI_END);	//append a URI short form
	}

	/**Appends a string to an appendable surrounded by specified string delimiters.
	The usual reserved string characters, along with the string end delimiter, will be escaped.
	@param appendable The appendable to which the information should be appended.
	@param string The string to append.
	@param stringBegin The beginning string delimiter.
	@param stringEnd The ending string delimiter.
	@return The appendable with the new information appended.
	@exception NullPointerException if the given appendable and/or string is <code>null</code>.
	@exception IOException if there was an error appending to the appendable.
	*/
	protected static Appendable appendString(final Appendable appendable, final String string, final char stringBegin, final char stringEnd) throws IOException
	{
		appendable.append(stringBegin);	//append the string beginning delimiter
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
				case CHARACTER_TABULATION_CHAR:	//tab
					escaped=true;	//show that we should escape the character
					c=ESCAPED_TAB;	//use \t for tab
					break;
				case START_OF_STRING_CHAR:	//start of string
					escaped=true;	//show that we should escape the character
					c=ESCAPED_START_OF_STRING;	//use left double quotation mark for start of string
					break;
				case STRING_TERMINATOR_CHAR:	//string terminator
					escaped=true;	//show that we should escape the character
					c=ESCAPED_STRING_TERMINATOR;	//use right double quotation mark for string terminator
					break;
				default:
					if(c==stringEnd)	//if this is one of the string ending delimiter
					{
						escaped=true;	//show that we should escape the character
					}
					break;
			}
			if(escaped)	//if we should escape this character
			{
				appendable.append(STRING_ESCAPE);	//append an escape character
				appendable.append(c);	//append the character normally
			}
			else if(Character.isISOControl(c))	//if this is a control character
			{
				appendable.append(STRING_ESCAPE).append(ESCAPED_UNICODE).append(Integers.toHexString(c, 4));	//append a Unicode escaped version of the character
			}
			else	//if this character is not escaped and not a control character
			{
				appendable.append(c);	//append the character normally
			}
		}
		appendable.append(stringEnd);	//append the string ending delimiter
		return appendable;
	}

	/**Writes a list separator.
	If the generator is formatted, this method generates a newline (optionally preceded by a
	{@link TURF#LIST_DELIMITER}, depending on the setting of {@link #isFormattedListDelimiter()})
	and and then indents at the current indent level.
	If the generator is not formatted, a {@link TURF#LIST_DELIMITER} is generated.
	@param appendable The appendable used for generating the information.
	@exception NullPointerException if the given appendable is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	@see #isFormatted()
	@see #isFormattedListDelimiter()
	@see #writeNewLine(Appendable) 
	*/
	public <A extends Appendable> void writeListSeparator(final A appendable) throws IOException
	{
		if(isFormatted())	//if we should format the output
		{
			if(isFormattedListDelimiter())	//if we should nonetheless write a list delimiter even when formatted
			{
				appendable.append(LIST_DELIMITER);	//write a list delimiter
			}
			writeNewLine(appendable);	//use a newline as a list delimiter
		}
		else	//if we shouldn't format the output
		{
			appendable.append(LIST_DELIMITER);	//write a list delimiter
		}
	}

	/**Writes the end of a line and then indents at the current indent level.
	If the generator is not formatted, no action occurs.
	@param appendable The appendable used for generating the information.
	@exception NullPointerException if the given appendable is <code>null</code>.
	@exception IOException if there was an error writing to the appendable.
	@see #isFormatted()
	@see #getIndentLevel()
	*/
	public <A extends Appendable> void writeNewLine(final A appendable) throws IOException
	{
		if(isFormatted())	//if we should format the output
		{
			appendable.append(LINE_FEED_CHAR);	//newline
			for(int i=getIndentLevel(); i>0; --i)	//for each indentation
			{
				appendable.append(CHARACTER_TABULATION_CHAR);	//write a tab
			}
		}
	}

}
