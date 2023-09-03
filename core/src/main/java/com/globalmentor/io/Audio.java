/*
 * Copyright © 1996-2012 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.io;

import static com.globalmentor.net.MediaType.*;

import java.util.*;

import com.globalmentor.net.MediaType;

/**
 * Utilities and constants for working with audio content.
 * @author Garret Wilson
 */
public class Audio {

	//media types

	/** Single channel audio encoded using 8-bit ISDN mu-law [PCM] at a sample rate of 8000 Hz. */

	/**
	 * Basic audio content defined in <cite>RFC 2046</cite> but removed in later versions such as <cite>RFC 6838</cite> to indicate "an absolutely minimal lowest
	 * common denominator audio format" of "single channel audio encoded using 8bit ISDN mu-law [PCM] at a sample rate of 8000 Hz".
	 * @apiNote This constant is not defined in {@link MediaType} because this is a legacy type that is no longer part of the specification, and the media type
	 *          specification no longer includes any audio types.
	 * @see <a href="https://tools.ietf.org/html/rfc2046#section-4.3">RFC 2046: Multipurpose Internet Mail Extensions (MIME) Part Two: Media Types § 4.3. Audio
	 *      Media Type</a>
	 * @see <a href="https://www.iana.org/assignments/media-types/audio/basic">IANA media type assignments: audio/basic</a>
	 */
	public static final MediaType BASIC_MEDIA_TYPE = MediaType.of(AUDIO_PRIMARY_TYPE, "basic");

	/**
	 * The content type for an Ogg Vorbis file: <code>audio/ogg</code>.
	 * @see <a href="http://www.ietf.org/rfc/rfc5334.txt">RFC 5334: Ogg Media Types</a>
	 * @see <a href="http://wiki.xiph.org/MIME_Types_and_File_Extensions">MIME Types and File Extensions</a>
	 */
	public static final MediaType OGG_VORBIS_MEDIA_TYPE = MediaType.of(AUDIO_PRIMARY_TYPE, "ogg");

	/**
	 * The content type for MP3: <code>audio/mpeg</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc3003">RFC 3003: The audio/mpeg Media Type</a>
	 */
	public static final MediaType MPEG_MEDIA_TYPE = MediaType.of(AUDIO_PRIMARY_TYPE, "mpeg");

	//filename extensions

	/** The filename extension for audio files. */
	public static final String AU_FILENAME_EXTENSION = "au";

	/**
	 * The filename extension for MPEG 2 layer 3 (MP3) files.
	 * @see <a href="https://tools.ietf.org/html/rfc3003">RFC 3003: The audio/mpeg Media Type</a>
	 */
	public static final String MP3_FILENAME_EXTENSION = "mp3";

	/** The filename extension for Ogg Vorbis files. */
	public static final String OGG_FILENAME_EXTENSION = "ogg";

	/**
	 * The filename extension for Microsoft Windows Wave format.
	 * <p>
	 * Codec-aware content types use the form <code>audio/vnd.wave; codec-YYYY</code>. There is no clear official general Internet media type for this media type
	 * without a codec specified.
	 * </p>
	 * @see <a href="https://tools.ietf.org/html/rfc2361">RFC 2361: WAVE and AVI Codec Registries</a>
	 * @see <a href="http://wiki.whatwg.org/wiki/Video_type_parameters#WAVE">HTML 5 Video canPlay(type) parameters</a>
	 * @see <a href="http://tools.ietf.org/html/draft-ema-vpim-wav-00">Waveform Audio File Format MIME Sub-type Registration</a>
	 */
	public static final String WAV_FILENAME_EXTENSION = "wav";

	/**
	 * Internet media types for known audio filename extensions. Filename extensions are in canonical (lowercase) form.
	 * @apiNote This map may not include all the file extensions defined in this class.
	 */
	public static final Map<String, MediaType> MEDIA_TYPES_BY_FILENAME_EXTENSION = Map.of(
			//.au
			AU_FILENAME_EXTENSION, BASIC_MEDIA_TYPE,
			//.mp3
			MP3_FILENAME_EXTENSION, MPEG_MEDIA_TYPE,
			//.ogg
			OGG_FILENAME_EXTENSION, OGG_VORBIS_MEDIA_TYPE);

	//utilities

	/**
	 * Determines if the given content type represents an audio file.
	 * <p>
	 * This method recognizes the following audio media types:
	 * </p>
	 * <ul>
	 * <li><code>audio/*</code></li>
	 * <li><code>application/ogg</code></li>
	 * </ul>
	 * @param mediaType The content type to examine.
	 * @return <code>true</code> if the content type represents audio.
	 * @see <a href="http://www.ietf.org/rfc/rfc5334.txt">RFC 5334</a>
	 * @see <a href="http://www.ietf.org/rfc/rfc3534.txt">RFC 3534</a>
	 * @see <a href="http://wiki.xiph.org/MIME_Types_and_File_Extensions">MIME Types and File Extensions</a>
	 */
	public static boolean isAudio(final MediaType mediaType) {
		final String topLevelType = mediaType.getPrimaryType(); //get the content type top-level type
		if(MediaType.AUDIO_PRIMARY_TYPE.equals(topLevelType)) { //if this is a top-level audio content type
			return true; //this is an audio content type
		}
		if(MediaType.APPLICATION_PRIMARY_TYPE.equals(topLevelType)) { //if this is an application type
			if(OGG_VORBIS_MEDIA_TYPE.getSubType().equals(mediaType.getSubType())) { //if this is application/ogg
				return true; //this is an Ogg Vorbis  audio file
			}
		}
		return false; //we didn't recognize the type as an audio type
	}

}
