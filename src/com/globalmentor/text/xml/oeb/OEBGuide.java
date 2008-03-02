package com.globalmentor.text.xml.oeb;

/**Represents a guide in OEB 1.x.
@author Garret Wilson
*/
public class OEBGuide
{

	/**The type of guide, usually one of the constants defined in
		<code>OEBGuideConstants</code>.
	*/
	private final String type;

		/**@return The type of guide, usually one of the constants defined in <code>OEBGuideConstants</code>.
		*/
		public String getType() {return type;}

	/**The title of this guide.*/
	private final String title;

		/**@return The title of this guide which can be used as a visual representation.*/
		public String getTitle() {return title;}

	/**The URI reference to the guide in the manifest.*/
	private final String href;

		/**@return The URI reference to the guide in the manifest.*/
		public String getHRef() {return href;}

	/**Constructs a guide.
	@param type The type of guide, usually one of the constants defined in <code>OEBGuideConstants</code>.
	@param title The guide title.
	@param href The reference to the guide.
	*/
	public OEBGuide(final String type, final String title, final String href)
	{
		this.type=type;  //set the guide type
		this.title=title;  //set the guide title
		this.href=href;  //set the href of the guide
	}
}