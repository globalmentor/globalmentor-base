package com.garretwilson.io;

/**Constant values related to MIME media types.
@author Garret Wilson
*/
public interface MediaTypeConstants
{
	/**The divider character for media type strings.*/
	public final static char DIVIDER='/';

		//discrete top-level media types
	public final static String TEXT="text";
	public final static String IMAGE="image";
	public final static String AUDIO="audio";
	public final static String VIDEO="video";
	public final static String APPLICATION="application";
		//composite top-level media types
	public final static String MULTIPART="multipart";
	public final static String MESSAGE="message";

		//text media types
	/**An OEB 1.0 document.*/
	public final static String X_OEB1_CSS="x-oeb1-css";
	public final static String X_OEB1_DOCUMENT="x-oeb1-document";
	public final static String TEXT_X_OEB1_DOCUMENT=TEXT+DIVIDER+X_OEB1_DOCUMENT;
	public final static String X_OEB1_PACKAGE="x-oeb1-package";
	public final static String TEXT_X_OEB1_PACKAGE=TEXT+DIVIDER+X_OEB1_PACKAGE; //G***should this be application/..., or +xml or something?
	public final static String HTML="html";
	public final static String TEXT_HTML=TEXT+DIVIDER+HTML;
	public final static String PLAIN="plain";
	public final static String TEXT_PLAIN=TEXT+DIVIDER+PLAIN;
	public final static String XML="xml";
	public final static String TEXT_XML=TEXT+DIVIDER+XML;

		//image media types
	/**A GIF image.*/
	public final static String GIF="gif";
	public final static String IMAGE_GIF=IMAGE+DIVIDER+GIF;
	/**A JPEG image.*/
	public final static String JPEG="jpeg";
	public final static String IMAGE_JPEG=IMAGE+DIVIDER+JPEG;
	/**A PNG image.*/
	public final static String PNG="png";
	public final static String IMAGE_PNG=IMAGE+DIVIDER+PNG;
	/**A TIFF image.*/
	public final static String TIFF="tiff";
	public final static String IMAGE_TIFF=IMAGE+DIVIDER+TIFF;
	/**A bitmap image.*/
	public final static String X_BITMAP="x-bitmap";
	public final static String IMAGE_X_BITMAP=IMAGE+DIVIDER+X_BITMAP;

		//audio media types
	/**Single channel audio encoded using 8-bit ISDN mu-law [PCM] at a sample rate of 8000 Hz.*/
	public final static String BASIC="basic";
	public final static String AUDIO_BASIC=AUDIO+DIVIDER+BASIC;

		//video media types
	/**An MPEG video.*/
	public final static String MPEG="mpeg";
	public final static String VIDEO_MPEG=VIDEO+DIVIDER+MPEG;

		//application media types
	/**A stream of bytes.*/
	public final static String OCTET_STREAM="octet-stream";
	public final static String APPLICATION_OCTET_STREAM=APPLICATION+DIVIDER+OCTET_STREAM;
	/**A Java application.*/
	public final static String JAVA="java";
	public final static String APPLICATION_JAVA=APPLICATION+DIVIDER+JAVA;
	/**An XML application.*/
	public final static String APPLICATION_XML=APPLICATION+DIVIDER+XML;
	/**An XHTML application.*/
	public final static String XHTML_XML="xhtml+xml";
	public final static String APPLICATION_XHTML_XML=APPLICATION+DIVIDER+XHTML_XML;
	/**A Zip file.*/
	public final static String ZIP="zip";
	public final static String APPLICATION_ZIP=APPLICATION+DIVIDER+ZIP;
}