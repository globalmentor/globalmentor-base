package com.garretwilson.text.xml.oeb;

/**Represents a guide in OEB 1.x.
@author Garret Wilson
*/
public class OEBGuide implements OEBGuideConstants
{

	/**The type of guide, usually one of the constants defined in
		<code>OEBGuideConstants</code>.
	*/
	private String type;

		/**@return The type of guide, usually one of the constants defined in
		  <code>OEBGuideConstants</code>.
		*/
		public String getType() {return type;}

		/**Sets the type of guide.
		@param newType The type of guide, usually one of the constants defined in
		  <code>OEBGuideConstants</code>.
		*/
		public void setType(final String newType) {type=newType;}

	/**The title of this guide.*/
	private String title;

		/**@return The title of this guide which can be used as a visual representation.*/
		public String getTitle() {return title;}

		/**Sets the title of this guide.
		@param newTitle The guide title.*/
		public void setTitle(final String newTitle) {title=newTitle;}

	/**The URI reference to the guide in the manifest.*/
	private String href;

		/**@return The URI reference to the guide in the manifest.*/
		public String getHRef() {return href;}

		/**Sets the URI reference to the guide in the manifest.
		@param newHRef The reference to the guide.*/
		public void setHRef(final String newHRef) {href=newHRef;}

	/**Constructs a guide.
		@param type The type of guide, usually one of the constants defined in
		  <code>OEBGuideConstants</code>.
		@param title The guide title.
		@param href The reference to the guide.
	*/
	public OEBGuide(final String type, final String title, final String href)
	{
		setType(type);  //set the guide type
		setTitle(title);  //set the guide title
		setHRef(href);  //set the href of the guide
	}
}