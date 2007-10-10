package com.garretwilson.net;

import java.net.URI;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

import static java.util.Collections.*;

import static com.garretwilson.net.URIUtilities.*;

import com.garretwilson.assess.qti.QTIConstants;
import com.garretwilson.net.http.webdav.ApacheWebDAVConstants;
import com.garretwilson.rdf.RDFConstants;
import com.garretwilson.rdf.dicto.DictoConstants;
import com.garretwilson.rdf.directory.vcard.VCard;
import com.garretwilson.rdf.dublincore.DCConstants;
import com.garretwilson.rdf.foaf.FOAF;
import com.garretwilson.rdf.maqro.MAQROConstants;
import com.garretwilson.rdf.rdfs.RDFSConstants;
import com.garretwilson.rdf.version.VersionConstants;
import com.garretwilson.rdf.xeb.XEBConstants;
import com.garretwilson.rdf.xpackage.XPackageConstants;
import com.garretwilson.text.xml.XMLConstants;
import com.garretwilson.text.xml.oeb.OEBConstants;
import com.garretwilson.text.xml.schema.XMLSchemaConstants;
import com.garretwilson.text.xml.xhtml.XHTMLConstants;
import com.garretwilson.text.xml.xlink.XLinkConstants;
import com.garretwilson.util.MapDecorator;

/**Map managing namespace URIs and labels for serialization.
Mapping labels to the <code>null</code> namespace or to the <code>null</code> label is allowed.
<p>This class is not thread safe.</p>
@author Garret Wilson
*/
public abstract class AbstractNamespaceLabelManager extends MapDecorator<URI, String>
{

	/**The default map of namespace-label mappaings.*/
	private final static Map<URI, String> DEFAULT_NAMESPACE_URI_LABEL_MAP;

	static	//add the default labels
	{
		final Map<URI, String> tempNamespaceURILabelMap=new HashMap<URI, String>();	//create a temporary map to fill
		tempNamespaceURILabelMap.put(ApacheWebDAVConstants.APACHE_WEBDAV_PROPERTY_NAMESPACE_URI, ApacheWebDAVConstants.APACHE_WEBDAV_PROPERTY_NAMESPACE_PREFIX); //Apache WebDAV properties
		tempNamespaceURILabelMap.put(DictoConstants.DICTO_NAMESPACE_URI, DictoConstants.DICTO_NAMESPACE_PREFIX); //Dicto
		tempNamespaceURILabelMap.put(DCConstants.DCMI11_ELEMENTS_NAMESPACE_URI, DCConstants.DCMI_ELEMENTS_NAMESPACE_PREFIX); //Dublin Core
		tempNamespaceURILabelMap.put(URI.create("http://example.com/example"), "example"); //example
		tempNamespaceURILabelMap.put(FOAF.FOAF_NAMESPACE_URI, FOAF.FOAF_NAMESPACE_PREFIX); //FOAF
		tempNamespaceURILabelMap.put(MAQROConstants.MAQRO_NAMESPACE_URI, MAQROConstants.MAQRO_NAMESPACE_PREFIX); //MAQRO
		tempNamespaceURILabelMap.put(OEBConstants.OEB1_DOCUMENT_NAMESPACE_URI, OEBConstants.OEB1_DOCUMENT_NAMESPACE_PREFIX); //OEB 1
		tempNamespaceURILabelMap.put(URI.create("http://globalmentor.com/namespaces/marmot#"), "marmot"); //Marmot TODO link to Marmot constants when Marmot is included in normal libraries
		tempNamespaceURILabelMap.put(URI.create("http://marmox.net/namespaces/content#"), "content"); //Marmox content
//TODO del		tempNamespaceURIPrefixMap.put(PLOOP.PLOOP_PROPERTY_NAMESPACE_URI, PLOOP.PLOOP_PROPERTY_NAMESPACE_PREFIX); //PLOOP property
		tempNamespaceURILabelMap.put(URI.create(QTIConstants.QTI_1_1_NAMESPACE_URI), QTIConstants.QTI_NAMESPACE_PREFIX); //QTI
		tempNamespaceURILabelMap.put(RDFConstants.RDF_NAMESPACE_URI, RDFConstants.RDF_NAMESPACE_PREFIX); //RDF
		tempNamespaceURILabelMap.put(RDFSConstants.RDFS_NAMESPACE_URI, RDFSConstants.RDFS_NAMESPACE_PREFIX); //RDFS
//G***add SOAP
		tempNamespaceURILabelMap.put(VCard.VCARD_NAMESPACE_URI, VCard.VCARD_NAMESPACE_PREFIX); //vCard
		tempNamespaceURILabelMap.put(VersionConstants.VERSION_NAMESPACE_URI, VersionConstants.VERSION_NAMESPACE_PREFIX); //version
		tempNamespaceURILabelMap.put(XMLSchemaConstants.XML_SCHEMA_NAMESPACE_URI, XMLSchemaConstants.XML_SCHEMA_NAMESPACE_PREFIX); //XML Schema
		tempNamespaceURILabelMap.put(XHTMLConstants.XHTML_NAMESPACE_URI, XHTMLConstants.XHTML_NAMESPACE_PREFIX); //XHTML
		tempNamespaceURILabelMap.put(XLinkConstants.XLINK_NAMESPACE_URI, XLinkConstants.XLINK_NAMESPACE_PREFIX); //XLink
		tempNamespaceURILabelMap.put(XMLConstants.XML_NAMESPACE_URI, XMLConstants.XML_NAMESPACE_PREFIX); //XML
		tempNamespaceURILabelMap.put(XMLConstants.XMLNS_NAMESPACE_URI, XMLConstants.XMLNS_NAMESPACE_PREFIX); //XML namespaces
		tempNamespaceURILabelMap.put(XEBConstants.XEB_NAMESPACE_URI, XEBConstants.XEB_NAMESPACE_PREFIX); //XEbook
		tempNamespaceURILabelMap.put(XPackageConstants.XPACKAGE_NAMESPACE_URI, XPackageConstants.XPACKAGE_NAMESPACE_PREFIX); //XPackage
		tempNamespaceURILabelMap.put(XPackageConstants.XML_ONTOLOGY_NAMESPACE_URI, XPackageConstants.XML_ONTOLOGY_NAMESPACE_PREFIX); //XPackage XML ontology
//TODO del		tempNamespaceURIPrefixMap.put(FileOntologyConstants.FILE_ONTOLOGY_NAMESPACE_URI, FileOntologyConstants.FILE_ONTOLOGY_NAMESPACE_PREFIX); //XPackage file ontology
//TODO add XPackage Unicode ontology
//TODO del		tempNamespaceURIPrefixMap.put(MIMEOntologyConstants.MIME_ONTOLOGY_NAMESPACE_URI, MIMEOntologyConstants.MIME_ONTOLOGY_NAMESPACE_PREFIX); //XPackage MIME ontology
		DEFAULT_NAMESPACE_URI_LABEL_MAP=unmodifiableMap(tempNamespaceURILabelMap);	//store a static read-only map
	}

	/**Default constructor using a hash map.*/
	public AbstractNamespaceLabelManager()
	{
		this(new HashMap<URI, String>());	//construct the class with a hash map
	}

	/**Map constructor.
	@param map The map this map should decorate.
	@exception NullPointerException if the provided map is <code>null</code>.
	*/
	public AbstractNamespaceLabelManager(final Map<URI, String> map)
	{
		super(map);	//construct the parent class
	}

	/**The atomic variable used to generate labels.*/
	private final AtomicLong generatedLabelCount=new AtomicLong(0);

		/**Generates a new label unique to this manager.
		@return A new label unique to this manager.
		*/
		private String generateLabel()
		{
			return "namespace"+generatedLabelCount.incrementAndGet();	//atomically get the next counter value and use it in generating a new labek
		}

	/**Determines whether the given namespace URI is that of a recognized namespace.
	A namespace is recognized if a label has been associated with the given namespace URI or the default namespace URI label map has a record of this namespace.
	@param namespaceURI A namespace URId
	@return <code>true</code> if a label has been associated with the given namespace URI or if a label is known that could be associated with this namespace URI.
	@see #DEFAULT_NAMESPACE_URI_LABEL_MAP
	*/
	public boolean isRecognized(final URI namespaceURI)
	{
		return containsKey(namespaceURI) || DEFAULT_NAMESPACE_URI_LABEL_MAP.containsKey(namespaceURI);	//return whether this map or the default namespace map recognizes the given namespace URI
	}

	/**Retrieves the label to use for the given namespace.
	If a namespace is unrecognized (i.e. no label, inluding the <code>null</code> label, has been registered with the given namespace),
	a new one will be created and stored in the map for future use.
	The last package segment of any hierarchical URIs will be used as the namespace label if possible.
	@param namespaceURI The namespace URI for which a label should be returned
	@return A label for use with the given namespace, or <code>null</code> if the <code>null</code> label is assigned to the given namespace.
	@see #isLabel(String)
	*/
	public String getNamespaceLabel(final URI namespaceURI)
	{
		String label=get(namespaceURI);  //get the label keyed by the namespace, if any
		if(label==null && !containsKey(namespaceURI))	//if we didn't find a label because the namespace wasn't registered, generate a label
		{
			label=DEFAULT_NAMESPACE_URI_LABEL_MAP.get(namespaceURI);	//look up the namespace in the map of default label mappings
			if(label==null)  //if there is still no label for this namespace, check for a Java package URI
			{
				final String name=getName(namespaceURI);	//get the name identified by the URI (the last URI path sequence)
				if(isLabel(name) && !containsValue(name))	//if the name is a valid label that we haven't yet used
				{
					label=name;	//use the name as the label
				}
				if(label==null)	//if we didn't find a Java package namespace label
				{
					label=generateLabel(); //generate a unique namespace label
				}
			}
			put(namespaceURI, label); //store the label in the map
		}
	  return label;  //return the label we found or created
	}

	/**Determines whether the given string is a valid label
	@param string The string to check for being a label.
	@return <code>true</code> if the given string represents a valid label.
	@exception NullPointerException if the given string is <code>null</code>.
	*/
	protected abstract boolean isLabel(final String string);

}
