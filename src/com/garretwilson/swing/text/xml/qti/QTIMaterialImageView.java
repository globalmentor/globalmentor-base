/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.garretwilson.swing.text.xml.qti;

import java.awt.Image;
import java.io.IOException;
import java.net.URISyntaxException;

import javax.swing.text.*;
import com.garretwilson.awt.ImageUtilities;
import com.garretwilson.swing.text.xml.XMLComponentImageView;
import com.garretwilson.swing.text.xml.XMLStyles;
import com.globalmentor.log.Log;
import static com.globalmentor.mentoract.qti.QTI.*;

/**A view that displays an image, intended to support the QTI
	<code>&lt;matimage&gt;</code> element.
@author Garret Wilson
@see javax.swing.text.html.ImageView
*/
public class QTIMaterialImageView extends XMLComponentImageView
{

	/**Creates a new view that represents a QTI material image element.
	@param element The element for which to create the view.
	*/
  public QTIMaterialImageView(final Element element)
	{
   	super(element);	//do the default constructing
   	initialize(element);	//do the necessary image value setting TODO perhaps promote this to XMLImageView or higher
	}

	/**Initializes the information needed to render the image.
	@param element The element which contains the image information.
	*/
	protected void initialize(final Element element)
	{
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
//TODO del		final String elementName=XMLStyleConstants.getXMLElementName(attributeSet); //get the name of this element
		final String uri=XMLStyles.getXMLAttributeValue(attributeSet, null, ATTRIBUTE_URI);  //get the URI of the image
			setHRef(uri);	//set the href to the value we found
		int height=-1; //assume for now that the image dimensions are not defined in the attributes
		int width=-1;  //
/*TODO fix
		try //try to get the width and the height from the attributes; if we can, we won't have to load the image, now
		{
			//get the height if it is defined TODO check about namespaces
			final String heightString=(String)XMLStyleConstants.getDefinedAttribute(attributeSet, ELEMENT_IMG_ATTRIBUTE_HEIGHT);
			if(heightString!=null)  //if there is a height defined
				height=Integer.parseInt(heightString);  //turn the height of the image into an integer
			//get the width if it is defined TODO check about namespaces
			final String widthString=(String)XMLStyleConstants.getDefinedAttribute(attributeSet, ELEMENT_IMG_ATTRIBUTE_WIDTH);
			if(widthString!=null)  //if there is a height defined
				width=Integer.parseInt(widthString);  //turn the width of the image into an integer
		}
		catch(NumberFormatException e) {} //ignore any number format exceptions; the width and/or the height will be left at -1 for us to check
*/
		if(height==-1 || width==-1) //if we were unable to find either the width or the height, load the image and get the dimensions directly
		{
			try
			{
Log.trace("we don't know the dimensions of the QTI image; we'll have to get it"); //TODO del
				final Image image=getImage(); //get the image, loading it if needed (in initialize() it will usually have to be loaded)
Log.trace("got the image;  loading it"); //TODO del
assert image!=null : "fImage is null";  //TODO fix
  			ImageUtilities.loadImage(image);  //load the image TODO optimize: perhaps there's a way to just load part of the image, to get the image dimensions
Log.trace("loaded the image"); //TODO del
				height=image.getHeight(this);	//get the image's height
				width=image.getWidth(this);	//get the image's width
Log.trace("height: ", height); //TODO del
Log.trace("width", width); //TODO del
				freeImage();  //free the image memory; this should speed up view flowing
			}
			catch(URISyntaxException e)	//TODO do something better here
			{
				Log.error(e);
			}
			catch(IOException e)	//TODO do something better here
			{
				Log.error(e);
			}
		}
		setHeight(height);	//set the height of the image view to whatever we found
		setWidth(width);	//set the width of the image view to whatever we found
	}

}
