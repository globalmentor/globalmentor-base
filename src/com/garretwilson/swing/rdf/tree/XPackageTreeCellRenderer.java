package com.garretwilson.swing.rdf.tree;

import javax.swing.*;
import com.garretwilson.io.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.xpackage.*;

/**A tree cell renderer that can render different icons and strings for user
	objects that are XPackage RDF resources.
	<p>If the a tree node is a <code>IconTreeNode</code> that has an icon, its icon
	is used. If not, specified icons are used for the specified types of
	RDF resource user objects, using first the XPackage content type and then, if
	there is no content type, the RDF resource type. If the user object is not an
	RDF resource, the specified class of user object is used to find an icon.
	If a tree node is not a <code>DefaultMutableTreeCell</code>, the default
	icons are used from the parent class.</p>
@author Garret Wilson
*/
public class XPackageTreeCellRenderer extends RDFResourceTreeCellRenderer
{

		/**Registers an open icon to be used with a particular media type.
		@param contentType The media type for the icon.
		@param icon The icon to register with the media type, or <code>null</code>
			if no icon should be associated with the media type.
		*/
		public void registerOpenIcon(final MediaType contentType, final Icon icon)
		{
			registerOpenIcon((Object)contentType, icon); //put the icon in the map, keyed to the media type
		}

		/**Registers a closed icon to be used with a particular media type.
		@param contentType The media type for the icon.
		@param icon The icon to register with the media type, or <code>null</code>
			if no icon should be associated with the media type.
		*/
		public void registerClosedIcon(final MediaType contentType, final Icon icon)
		{
			registerClosedIcon((Object)contentType, icon); //put the icon in the map, keyed to the media type
		}

		/**Registers a leaf icon to be used with a particular media type.
		@param contentType The media type for the icon.
		@param icon The icon to register with the media type, or <code>null</code>
			if no icon should be associated with the media type.
		*/
		public void registerLeafIcon(final MediaType contentType, final Icon icon)
		{
			registerLeafIcon((Object)contentType, icon); //put the icon in the map, keyed to the media type
		}

	/**Default constructor.*/
	public XPackageTreeCellRenderer()
	{
		super();  //construct the parent object
	}

	/**Retrieves the key used to lookup data, such as icons, specific for this
		user object.
		<p>If the user object is an RDF resource, its content type will be returned.
		If the resource has no content type, the RDF resource type will be returned.
		If the resource has multiple types it is undefined which type will be
		returned.</p>
		<p>If the user object is not an RDF resource, its class will be returned.</p>
	@param userObject The user object for which a key should be returned.
	@return The key for looking up data for the user object, or <code>null</code>
		if no key could be determined for the user object or if the user object
		is <code>null</code>.
	*/
	protected Object getUserObjectKey(final Object userObject)
	{
		if(userObject instanceof RDFResource) //if this is an RDF resource
		{
			final RDFResource rdfResource=(RDFResource)userObject;  //cast the object to an RDF resource
			final MediaType mediaType=MIMEOntologyUtilities.getMediaType(rdfResource);  //see if there is a media type for the resource
		  if(mediaType!=null) //if there is a media type
				return mediaType; //use the media type as the key
		}
		return super.getUserObjectKey(userObject);  //if we can't find a media type, use the default key
	}
}