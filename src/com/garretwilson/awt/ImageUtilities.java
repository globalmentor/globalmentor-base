package com.garretwilson.awt;

import java.awt.*;
import com.garretwilson.util.Debug;

/**Convenience routines for manipulating images.
@see java.awt.Image
@author Garret Wilson
*/
public class ImageUtilities
{

	/**The component used by the media tracker for loading an image.*/
	protected final static Component mediaTrackerComponent=new Component() {};
	/**The media tracker used to load images.*/
	protected final static MediaTracker mediaTracker=new MediaTracker(mediaTrackerComponent);

	/**This class cannot be publicly instantiated.*/
	private ImageUtilities()
	{
	}

	/**Uses a <code>MediaTracker</code> to wait until an image is loaded. Returns
		the status of the loading.
	@param image The image to load.
	@return The status of the image loading, as defined by <code>MediaTracker</code>,
		bitwise <strong>OR</strong>ed together: <code>LOADING</code>,
		<code>ABORTED</code>, <code>ERRORED</code>, and/or <code>COMPLETE</code>, or
		zero if the image hasn't started loading (which should never happen).
	*/
	public static int loadImage(final Image image)
	{
		final int imageID=0;  //we'll use this as our ID when tracking the image loading
		synchronized(mediaTracker) //don't allow others to use the media track while we're using it
		{
	    mediaTracker.addImage(image, imageID); //add the image to the media tracker
			try
			{
				try
				{
					mediaTracker.waitForID(imageID);  //wait until the image is loaded
	//G***del or fix				mediaTracker.waitForID(0, 0);
				}
				catch (InterruptedException e)
				{
					throw new AssertionError(e);
				}
				return mediaTracker.statusID(imageID, false); //return the loading status
			}
			finally
			{
		    mediaTracker.removeImage(image, imageID);  //always remove the image from the tracker
			}
		}
	}

}