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

import com.globalmentor.net.ContentType;

/**
 * Utilities and constants for working with archive content.
 * @author Garret Wilson
 */
public class Archive {

	//media types

	/**
	 * The media type for 7z files: <code>x-7z-compressed</code>.
	 * @see <a href="https://www.7-zip.org/7z.html">7z Format</a>
	 * @see <a href="https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types">MDN: Common MIME types</a>
	 */
	public static final ContentType SEVENZ_MEDIA_TYPE = ContentType.of(APPLICATION_PRIMARY_TYPE, "x-7z-compressed");

	/**
	 * The media type for RAR files: <code>application/vnd.rar</code>.
	 * @see <a href="https://www.rarlab.com/technote.htm">RAR 5.0 archive format</a>
	 * @see <a href="https://www.iana.org/assignments/media-types/application/vnd.rar">IANA media type assignments: application/vnd.rar</a>
	 */
	public static final ContentType RAR_MEDIA_TYPE = ContentType.of(APPLICATION_PRIMARY_TYPE, "vnd.rar");

	/**
	 * The media type for ZIP files: <code>application/zip</code>.
	 * @see <a href="https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT">.ZIP File Format Specification</a>
	 * @see <a href="https://www.iana.org/assignments/media-types/application/zip">IANA media type assignments: application/zip</a>
	 */
	public static final ContentType ZIP_MEDIA_TYPE = ContentType.of(APPLICATION_PRIMARY_TYPE, "zip");

	//filename extensions

	/**
	 * The filename extension for 7z compressed files.
	 * @see <a href="https://www.7-zip.org/7z.html">7z Format</a>
	 */
	public static final String SEVENZ_FILENAME_EXTENSION = "7z";

	/**
	 * The filename extension for RAR compressed files.
	 * @see <a href="https://www.rarlab.com/technote.htm">RAR 5.0 archive format</a>
	 */
	public static final String RAR_FILENAME_EXTENSION = "rar";

	/**
	 * The filename extension for ZIP files.
	 * @see <a href="https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT">.ZIP File Format Specification</a>
	 */
	public static final String ZIP_FILENAME_EXTENSION = "zip";

}
