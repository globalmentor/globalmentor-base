package com.garretwilson.text.xml;

/**A simple class that holds an external ID. This class does <em>not</em>
represent a node in the XML document tree; it is only used internally for
parsing. It can only accessed by classes in its own package.
@see XMLProcessor
@see XMLDocumentType
*/
class XMLExternalID
{

	/**Default constructor which creates an external ID with <code>null</code>
		public and system IDs.
	*/
	public XMLExternalID() {}

	/**Creates an external ID oject with the given public and private IDs.
	@param publicID The new public ID.
	@param systemID The new system ID.
	*/
	public XMLExternalID(final String publicID, final String systemID)
	{
		setPublicID(publicID);  //set the public ID
		setSystemID(systemID);  //set the system ID
	}

	/**The public identifier, or <code>null</code> if there is none.*/
	private String PublicID=null;

	/**@return The public identifier, or <code>null</code> if there is none.
	@see XMLExternalID#getSystemID
	@see org.w3c.dom.DOMImplementation
	@see org.w3c.dom.DocumentType
	@see org.w3c.dom.Notation
	@see org.w3c.dom.Entity
	*/
	public String getPublicID() {return PublicID;}

	/**Sets the public identifier.
	@param publicID The new public ID.
	@see XMLExternalID#setSystemID
	@see org.w3c.dom.DOMImplementation
	@see org.w3c.dom.DocumentType
	@see org.w3c.dom.Notation
	@see org.w3c.dom.Entity
	*/
	void setPublicID(final String publicID) {PublicID=publicID;}

	/**The system identifier, or <code>null</code> if there is none.*/
	private String SystemID=null;

	/**@return The public identifier, or <code>null</code> if there is none.
	@see XMLExternalID#getPublicID
	@see org.w3c.dom.DOMImplementation
	@see org.w3c.dom.DocumentType
	@see org.w3c.dom.Notation
	@see org.w3c.dom.Entity
	*/
	public String getSystemID() {return SystemID;}

	/**Sets the system identifier.
	@param systemID The new system ID.
	@see XMLExternalID#setPublicID
	@see org.w3c.dom.DOMImplementation
	@see org.w3c.dom.DocumentType
	@see org.w3c.dom.Notation
	@see org.w3c.dom.Entity
	*/
	void setSystemID(final String systemID) {SystemID=systemID;}

}

