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

import com.globalmentor.text.xml.XML;

/**Constant values related to MIME media types, as originally defined in
	<a href="http://www.rfc-editor.org/rfc/rfc2046.txt">RFC 2046</a>,
	"MIME Part 2: Media Types".
@author Garret Wilson
@see javax.mail.internet.ContentType
@see http://www.rfc-editor.org/rfc/rfc2046.txt
*/
public class ContentTypeConstants	//TODO transfer these constants to specific relevant classes and remove this class
{
		//subtype suffixes

	/**The suffix for RDF application types.*/
	public final static String RDF_XML_SUBTYPE_SUFFIX="rdf"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+XML.XML_SUBTYPE_SUFFIX;
	/**The suffix for TURF application types.*/
	public final static String TURF_SUBTYPE_SUFFIX="turf";
	/**The suffix for URF TURF application types.*/
	public final static String URF_TURF_SUBTYPE_SUFFIX="urf"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+TURF_SUBTYPE_SUFFIX;
	/**The suffix for zip application types.*/
	public final static String ZIP_SUBTYPE_SUFFIX="zip";

	//text media types
	public final static String CALENDAR_SUBTYPE="calendar";
	public final static String DIRECTORY_SUBTYPE="directory";
	public final static String JAVASCRIPT_SUBTYPE="javascript";
	/**A <code>text/uri-list</code> content type as defined in <a href="http://www.ietf.org/rfc/rfc2483.txt">RFC 2483</a>, "URI Resolution Services Necessary for URN Resolution".*/
	public final static String URI_LIST_SUBTYPE="uri-list";
	/**The wildcard subtype, matching any subtype.*/
	public final static String WILDCARD_SUBTYPE=String.valueOf(ContentTypes.TYPE_WILDCARD_CHAR);
	public final static String XML_SUBTYPE="xml";
	/**An XML external parsed entity, as defined in <a href="http://www.ietf.org/rfc/rfc3023.txt">RFC 3023</a>, "XML Media Types".*/
	public final static String XML_EXTERNAL_PARSED_ENTITY_SUBTYPE="xml-external-parsed-entity";

		//image media types
	/**A GIF image.*/
	public final static String GIF_SUBTYPE="gif";
	/**A JPEG image.*/
	public final static String JPEG_SUBTYPE="jpeg";
	/**A PNG image.*/
	public final static String PNG_SUBTYPE="png";
	/**A TIFF image.*/
	public final static String TIFF_SUBTYPE="tiff";
	/**A bitmap image.*/
	public final static String X_BITMAP_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"bitmap";

		//video media types
	/**An MPEG video, as wel as MPEG 2 layer 3 (MP3) audio with audio/mpeg; see <a href="http://www.rfc-editor.org/rfc/rfc3003.txt">RFC 3003</a>.*/
	public final static String MPEG_SUBTYPE="mpeg";

		//audio media types
	/**Single channel audio encoded using 8-bit ISDN mu-law [PCM] at a sample rate of 8000 Hz.*/
	public final static String BASIC_SUBTYPE="basic";
	/**Microsoft Windows Wave audio format.*/
	public final static String X_WAV_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"wav";

		//application media types
	/**A Dictionary Ontology (Dicto) dictionary.*/
	public final static String X_DICTO_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"dicto"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A Java application.*/
	public final static String JAVA_SUBTYPE="java";
	/**A Java JNLP file.*/
	public final static String X_JAVA_JNLP_FILE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"java-jnlp-file";
	/**A Java object.*/
	public final static String X_JAVA_OBJECT=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"java-object";
	/**A MAQRO activity.*/
	public final static String X_MAQRO_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"maqro"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A MAQRO question.*/
	public final static String X_QRO_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"qro"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**A Microsoft Word document; see <a href="http://www.iana.org/assignments/media-types/application/msword">http://www.iana.org/assignments/media-types/application/msword</a>.*/
	public final static String MSWORD_SUBTYPE="msword";
	/**An Ogg Vorbis file; see <a href="http://www.rfc-editor.org/rfc/rfc3534.txt">RFC 3534</a>.*/
	public final static String OGG_SUBTYPE="ogg";
	/**An Adobe PDF file.*/
	public final static String PDF_SUBTYPE="pdf";
	/**A Rar compressed file.*/	
	public final static String X_RAR_COMPRESSED_SUBTYPTE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"rar-compressed";
	/**A TURF data instance.*/
	public final static String TURF_SUBTYPE="turf";
	/**A Macromedia Flash object.*/
	public final static String X_SHOCKWAVE_FLASH_SUBTYPE="x-shockwave-flash";
	/**Submitted URL-encoded form data; see <a href="http://www.rfc-editor.org/rfc/rfc1867.txt">RFC 1867</a>.*/
	public final static String X_WWW_FORM_URLENCODED=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"www-form-urlencoded";
	/**An XML application.*/
//G***fix	public final static String APPLICATION_XML=APPLICATION+DIVIDER+XML;
	/**An XEB book file, <code>x-xebook+rdf+xml</code>.*/
	public final static String X_XEBOOK_RDF_XML_SUBTYPE=ContentTypes.SUBTYPE_EXTENSION_PREFIX+"xebook"+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+RDF_XML_SUBTYPE_SUFFIX;
	/**An XEB book zip file, <code>x-xebook+rdf+xml+zip</code>.*/
	public final static String X_XEBOOK_RDF_XML_ZIP_SUBTYPE=X_XEBOOK_RDF_XML_SUBTYPE+ContentTypes.SUBTYPE_SUFFIX_DELIMITER_CHAR+ZIP_SUBTYPE_SUFFIX;
	/**A Zip file.*/
	public final static String ZIP_SUBTYPE="zip";

		//multipart media types
	/**Submitted form data; see <a href="http://www.rfc-editor.org/rfc/rfc1867.txt">RFC 1867</a>.*/
	public final static String FORM_DATA_SUBTYPE="form-data";
}