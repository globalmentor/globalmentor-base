package com.garretwilson.swing.text.xml.qti;

import java.awt.Image;
import java.io.IOException;
import java.net.URISyntaxException;

import javax.swing.text.*;
import com.garretwilson.awt.ImageUtilities;
import com.garretwilson.swing.text.xml.XMLComponentImageView;
import com.garretwilson.swing.text.xml.XMLStyleConstants;
import com.garretwilson.assess.qti.QTIConstants;
import com.garretwilson.util.Debug;

/**A view that displays an image, intended to support the QTI
	<code>&lt;matimage&gt;</code> element.
@author Garret Wilson
@see javax.swing.text.html.ImageView
*/
public class QTIMaterialImageView extends XMLComponentImageView implements QTIConstants
{

	/**Creates a new view that represents a QTI material image element.
	@param element The element for which to create the view.
	*/
  public QTIMaterialImageView(final Element element)
	{
   	super(element);	//do the default constructing
   	initialize(element);	//do the necessary image value setting G***perhaps promote this to XMLImageView or higher
	}

	/**Initializes the information needed to render the image.
	@param element The element which contains the image information.
	*/
	protected void initialize(final Element element)
	{
		final AttributeSet attributeSet=element.getAttributes();  //get the element's attributes
//G***del		final String elementName=XMLStyleConstants.getXMLElementName(attributeSet); //get the name of this element
		final String uri=(String)XMLStyleConstants.getDefinedAttribute(attributeSet, ATTRIBUTE_URI);  //get the URI of the image
			setHRef(uri);	//set the href to the value we found
		int height=-1; //assume for now that the image dimensions are not defined in the attributes
		int width=-1;  //
/*G***fix
		try //try to get the width and the height from the attributes; if we can, we won't have to load the image, now
		{
			//get the height if it is defined G***check about namespaces
			final String heightString=(String)XMLStyleConstants.getDefinedAttribute(attributeSet, ELEMENT_IMG_ATTRIBUTE_HEIGHT);
			if(heightString!=null)  //if there is a height defined
				height=Integer.parseInt(heightString);  //turn the height of the image into an integer
			//get the width if it is defined G***check about namespaces
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
Debug.trace("we don't know the dimensions of the QTI image; we'll have to get it"); //G***del
				final Image image=getImage(); //get the image, loading it if needed (in initialize() it will usually have to be loaded)
Debug.trace("got the image;  loading it"); //G***del
	Debug.assert(image!=null, "fImage is null");  //G***fix
  			ImageUtilities.loadImage(image);  //load the image G***optimize: perhaps there's a way to just load part of the image, to get the image dimensions
Debug.trace("loaded the image"); //G***del
				height=image.getHeight(this);	//get the image's height
				width=image.getWidth(this);	//get the image's width
Debug.trace("height: ", height); //G***del
Debug.trace("width", width); //G***del
				freeImage();  //free the image memory; this should speed up view flowing
			}
			catch(URISyntaxException e)	//G***do something better here
			{
				Debug.error(e);
			}
			catch(IOException e)	//G***do something better here
			{
				Debug.error(e);
			}
		}
		setHeight(height);	//set the height of the image view to whatever we found
		setWidth(width);	//set the width of the image view to whatever we found
	}

}
