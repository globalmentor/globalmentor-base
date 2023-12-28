/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <https://www.globalmentor.com/>
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
 * Utilities and constants for working with image content.
 * @author Garret Wilson
 */
public final class Images {

	private Images() {
	}

	//media types

	/**
	 * The media type for bitmap image files: <code>image/bmp</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc7903">RFC 7903: Windows Image Media Types</a>
	 */
	public static final MediaType BMP_MEDIA_TYPE = MediaType.of(IMAGE_PRIMARY_TYPE, "bmp");

	/**
	 * The media type for GIF images: <code>image/gif</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc2046">RFC 2046: Multipurpose Internet Mail Extensions (MIME) Part Two: Media Types</a>
	 */
	public static final MediaType GIF_MEDIA_TYPE = MediaType.of(IMAGE_PRIMARY_TYPE, "gif");

	/**
	 * The media type for JPEG images: <code>image/jpeg</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc2046">RFC 2046: Multipurpose Internet Mail Extensions (MIME) Part Two: Media Types</a>
	 */
	public static final MediaType JPEG_MEDIA_TYPE = MediaType.of(IMAGE_PRIMARY_TYPE, "jpeg");

	/**
	 * The media type for PNG images: <code>image/png</code>.
	 * @see <a href="https://www.iana.org/assignments/media-types/image/png">IANA media type assignments: image/png</a>
	 */
	public static final MediaType PNG_MEDIA_TYPE = MediaType.of(IMAGE_PRIMARY_TYPE, "png");

	/**
	 * The media type for TIFF images: <code>image/tiff</code>.
	 * @see <a href="https://tools.ietf.org/html/rfc2302">RFC 2302: Tag Image File Format (TIFF) - image/tiff MIME Sub-type Registration</a>
	 */
	public static final MediaType TIFF_MEDIA_TYPE = MediaType.of(IMAGE_PRIMARY_TYPE, "tiff");

	//filename extensions

	/** The filename extension for bitmap files. */
	public static final String BMP_FILENAME_EXTENSION = "bmp";

	/** The filename extension for GIF files. */
	public static final String GIF_FILENAME_EXTENSION = "gif";

	/** The filename extension for JPEG files. */
	public static final String JPG_FILENAME_EXTENSION = "jpg";

	/** The filename extension for PNG files. */
	public static final String PNG_FILENAME_EXTENSION = "png";

	/**
	 * The filename extension for TIFF files.
	 * @see <a href="https://tools.ietf.org/html/rfc2302">RFC 2302: Tag Image File Format (TIFF) - image/tiff MIME Sub-type Registration</a>
	 */
	public static final String TIF_FILENAME_EXTENSION = "tif";

	/**
	 * Internet media types for known image filename extensions. Filename extensions are in canonical (lowercase) form.
	 * @apiNote This map may not include all the file extensions defined in this class.
	 */
	public static final Map<String, MediaType> MEDIA_TYPES_BY_FILENAME_EXTENSION = Map.of(
			//.bmp
			BMP_FILENAME_EXTENSION, BMP_MEDIA_TYPE,
			//.gif
			GIF_FILENAME_EXTENSION, GIF_MEDIA_TYPE,
			//.jpg
			JPG_FILENAME_EXTENSION, JPEG_MEDIA_TYPE,
			//.jpeg
			"jpeg", JPEG_MEDIA_TYPE,
			//.png
			PNG_FILENAME_EXTENSION, PNG_MEDIA_TYPE,
			//.tif
			TIF_FILENAME_EXTENSION, TIFF_MEDIA_TYPE,
			//.tiff
			"tiff", TIFF_MEDIA_TYPE);

}
