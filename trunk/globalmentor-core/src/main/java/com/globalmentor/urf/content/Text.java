package com.globalmentor.urf.content;

import java.net.URI;


/**A content resource that holds text content in its description through the {@value Content#CONTENT_PROPERTY_URI} property.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@author Garret Wilson
*/
public class Text extends AbstractContentResource
{

	/**Default constructor with no URI.*/
	public Text()
	{
		this(null);	//create a resource without a URI
	}

	/**URI constructor.
	@param uri The URI for the resource, or <code>null</code> if the resource should have no URI.
	*/
	public Text(final URI uri)
	{
		super(uri, Content.CONTENT_NAMESPACE_URI);	//construct the class, specifying the Content namespace
	}

	/**Returns the actual content of the resource.
	This method delegates to {@link #getStringContent()}. 
	@return This resource's string content declaration, or <code>null</code> if the resource has no <code>content.content</code> property specified or the content is not a string.
	*/
	public String getContent()
	{
		return getStringContent();	//get any string content
	}

	/**Determines a string value to use for representation.
	This method may take into account the current properties of the resource in order to provide the best possible string representation.
	This implementation determines the label in the following sequence:
	<ol>
		<li>The determined label of any «{@value Content#CONTENT_PROPERTY_URI}» property.</li>
		<li>Whatever label is determined by the parent class.</li>
	</ol>
	@return A string label to use for representation of the resource.
	@see #getStringContent()
	*/
	public String determineLabel()
	{
		final String content=getStringContent();	//get the string content, if there is any
		return content!=null ? content : super.determineLabel();	//determine the string content, if there is any; otherwise determine the default label
	}
}
