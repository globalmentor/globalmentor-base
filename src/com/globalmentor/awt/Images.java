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

package com.globalmentor.awt;

import java.awt.*;

/**Convenience routines for manipulating images.
@see Image
@author Garret Wilson
*/
public class Images
{

	/**The component used by the media tracker for loading an image.*/
	protected final static Component mediaTrackerComponent=new Component() {};
	/**The media tracker used to load images.*/
	protected final static MediaTracker mediaTracker=new MediaTracker(mediaTrackerComponent);

	/**This class cannot be publicly instantiated.*/
	private Images()
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
	//TODO del or fix				mediaTracker.waitForID(0, 0);
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