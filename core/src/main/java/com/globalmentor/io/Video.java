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

import static com.globalmentor.net.ContentType.*;
import static java.util.stream.Collectors.*;

import java.util.*;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.stream.Stream;

import com.globalmentor.net.ContentType;

/**
 * Utilities and constants for working with video content.
 * @author Garret Wilson
 */
public class Video {

	//media types

	/**
	 * MPEG-1 video content mentioned in <cite>RFC 2046</cite> but removed in later versions such as <cite>RFC 6838</cite> to indicate "video coded according to
	 * the MPEG standard".
	 * @apiNote This constant is not defined in {@link ContentType} because this is a legacy type that is no longer part of the specification, and the media type
	 *          specification no longer includes any video types.
	 * @see <a href="https://tools.ietf.org/html/rfc2046#section-4.4">RFC 2046: Multipurpose Internet Mail Extensions (MIME) Part Two: Media Types § 4.4. Video
	 *      Media Type</a>
	 */
	public static final ContentType MPEG_MEDIA_TYPE = ContentType.of(AUDIO_PRIMARY_TYPE, "basic");

	//filename extensions

	/** The filename extension for MPEG files. */
	public static final String MPG_FILENAME_EXTENSION = "mpg";

	/**
	 * Internet media types for known video filename extensions.
	 * @apiNote This map may not include all the file extensions defined in this class.
	 */
	public static final Map<String, ContentType> MEDIA_TYPES_BY_FILENAME_EXTENSION = Stream.of(
			//.mpg
			new SimpleImmutableEntry<>(MPG_FILENAME_EXTENSION, MPEG_MEDIA_TYPE),
			//.mpeg
			new SimpleImmutableEntry<>("mpeg", MPEG_MEDIA_TYPE))
			.collect(collectingAndThen(toMap(Map.Entry::getKey, Map.Entry::getValue), Collections::unmodifiableMap));

}
