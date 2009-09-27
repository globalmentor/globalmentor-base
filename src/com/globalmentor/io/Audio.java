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

package com.globalmentor.io;

import com.globalmentor.net.ContentType;

/**Utilities and constants for working with audio content.
@author Garret Wilson
*/
public class Audio
{

	/**The content type for an Ogg Vorbis file: <code>application/ogg</code>.
	@see <a href="http://www.rfc-editor.org/rfc/rfc3534.txt">RFC 3534</a>
	*/
	public static final ContentType OGG_CONTENT_TYPE=ContentType.getInstance(ContentType.APPLICATION_PRIMARY_TYPE, "ogg");

	/**The content type for MP3: <code>audio/mpeg</code>.*/ 
	public static final ContentType MPEG_CONTENT_TYPE=ContentType.getInstance(ContentType.AUDIO_PRIMARY_TYPE, "mpeg");

	/**The name extension for audio files.*/
	public final static String AU_NAME_EXTENSION="au";
	/**The extension for MPEG 2 layer 3 (MP3) files.*/
	public final static String MP3_NAME_EXTENSION="mp3";
	/**The extension for Ogg Vorbis files.*/
	public final static String OGG_NAME_EXTENSION="ogg";
	/**The name extension for Microsoft Windows Wave format.*/
	public final static String WAV_NAME_EXTENSION="wav";

	/**Determines if the given content type represents an audio file.
	<p>This method recognizes the following audio media types:</p>
	<ul>
		<li><code>audio/*</code></li>
		<li><code>application/ogg</code></li>
	</ul>
	@param mediaType The content type to examine.
	@return <code>true</code> if the content type represents audio.
	*/
	public static boolean isAudio(final ContentType mediaType)
	{
		final String topLevelType=mediaType.getPrimaryType();	//get the content type top-level type
		if(ContentType.AUDIO_PRIMARY_TYPE.equals(topLevelType))	//if this is a top-level audio content type
			return true;	//this is an audio content type
		if(ContentType.APPLICATION_PRIMARY_TYPE.equals(topLevelType))	//if this is an application type
		{
			if(OGG_CONTENT_TYPE.getSubType().equals(mediaType.getSubType()))	//if this is application/ogg
				return true;	//this is an Ogg Vorbis  audio file
		}
		return false;	//we didn't recognize the type as an audio type
	}

}
