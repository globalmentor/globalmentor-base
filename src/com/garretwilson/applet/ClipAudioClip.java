package com.garretwilson.applet;

import java.applet.*;
import javax.sound.sampled.*;

/**A concrete implementation of an applet <code>AudioClip</code> using the
	new Java sound class <code>Clip</code>.
@author Garret Wilson
*/
public class ClipAudioClip implements AudioClip
{

	/**The clip to play.*/
	private final Clip clip;

	/**Constructs a new object using an existing clip.
	@param newClip The clip to to play; this clip must be opened and ready to play.
	*/
	public ClipAudioClip(final Clip newClip)
	{
		clip=newClip; //store the clip
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
		clip.loop(clip.LOOP_CONTINUOUSLY);  // continuously loop the clip
	}

	/**Stops playing this audio clip.*/
	public void stop()
	{
		if(clip.isRunning())  //if the clip is already running
			clip.stop();  //stop playing the clip
	}

}