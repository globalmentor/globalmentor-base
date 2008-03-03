/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.applet;

import java.applet.*;

import javax.sound.sampled.*;

import static com.globalmentor.java.Objects.*;

/**A concrete implementation of an applet {@link AudioClip} using the Java sound interface {@link Clip}.
@author Garret Wilson
*/
public class ClipAudioClip implements AudioClip
{

	/**The clip to play.*/
	private final Clip clip;

	/**Constructs a new object using an existing clip.
	@param clip The clip to to play; this clip must be opened and ready to play.
	@throws NullPointerException if the given clip is <code>null</code>.
	*/
	public ClipAudioClip(final Clip clip)
	{
		this.clip=checkInstance(clip, "Clip cannot be null."); //store the clip
	}

	/**Starts playing this audio clip. Each time this method is called,
		the clip is restarted from the beginning.
	*/
	public void play()
	{
		stop(); //stop the clip
		clip.setFramePosition(0); //start at the beginning of the clip
		clip.start(); //start playing the clip
	}

	/**Starts playing this audio clip in a loop.*/
	public void loop()
	{
		stop(); //stop the clip
		clip.setFramePosition(0); //start at the beginning of the clip
		clip.loop(Clip.LOOP_CONTINUOUSLY);  // continuously loop the clip
	}

	/**Stops playing this audio clip.*/
	public void stop()
	{
		if(clip.isRunning())  //if the clip is already running
			clip.stop();  //stop playing the clip
	}

}