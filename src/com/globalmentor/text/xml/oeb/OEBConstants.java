package com.globalmentor.text.xml.oeb;

import java.net.URI;
import javax.mail.internet.ContentType;

import com.globalmentor.io.ContentTypeConstants;
import com.globalmentor.io.ContentTypes;

/**Several constants for OEB.
@author Garret Wilson
*/
public interface OEBConstants
{
	/**The package name of the OEB XML classes.*/
//TODO del	public final static String PACKAGE_NAME="com.garretwilson.text.xml.oeb";

	/**The public ID for the OEBPS 1.0 package.*/
	public final static String OEB10_PACKAGE_PUBLIC_ID="+//ISBN 0-9673008-1-9//DTD OEB 1.0 Package//EN";
	/**The default system ID for the OEBPS 1.0 package.*/
	public final static String OEB10_PACKAGE_SYSTEM_ID="http://openebook.org/dtds/oeb-1.0/oebpkg1.dtd";
	/**The default extension for the OEBPS 1.0 package.*/
	public final static String OEB1_PACKAGE_EXTENSION="opf";

	/**The public ID for the OEBPS 1.0.1 package.*/
	public final static String OEB101_PACKAGE_PUBLIC_ID="+//ISBN 0-9673008-1-9//DTD OEB 1.0.1 Package//EN";
	/**The default system ID for the OEBPS 1.0.1 package.*/
	public final static String OEB101_PACKAGE_SYSTEM_ID="http://openebook.org/dtds/oeb-1.0.1/oebpkg101.dtd";

	/**The recommended prefix to the OEB 1.0 package namespace.*/
	public static final String OEB1_PACKAGE_NAMESPACE_PREFIX="oebpackage";
	/**The URI to the OEB 1.0 package namespace.*/
	public static final URI OEB1_PACKAGE_NAMESPACE_URI=URI.create("http://openebook.org/namespaces/oeb-package/1.0/");

	//The OEB 1.0 XML package element names.*/
	public final static String PKG_ELEMENT_PACKAGE="package";
		public final static String PKG_ELEMENT_PACKAGE_ATTRIBUTE_UNIQUE_IDENTIFIER="unique-identifier";
	public final static String PKG_ELEMENT_METADATA="metadata";
		public final static String PKG_ELEMENT_METADATA_DC_METADATA="dc-metadata";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TITLE="dc:Title";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CREATOR="dc:Creator";
				public final static String PKG_METADATA_DC_METADATA_DC_CREATOR_ATTRIBUTE_ROLE="role";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SUBJECT="dc:Subject";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DESCRIPTION="dc:Description";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_PUBLISHER="dc:Publisher";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_CONTRIBUTOR="dc:Contributor";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_DATE="dc:Date";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_TYPE="dc:Type";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_FORMAT="dc:Format";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_IDENTIFIER="dc:Identifier";
				public final static String PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_ID="id";
				public final static String PKG_METADATA_DC_METADATA_DC_IDENTIFIER_ATTRIBUTE_SCHEME="scheme";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_SOURCE="dc:Source";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_LANGUAGE="dc:Language";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RELATION="dc:Relation";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_COVERAGE="dc:Coverage";
			public final static String PKG_ELEMENT_MANIFEST_DC_METADATA_DC_RIGHTS="dc:Rights";
	public final static String PKG_ELEMENT_MANIFEST="manifest";
		public final static String PKG_ELEMENT_MANIFEST_ITEM="item";
			public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_ID="id";
			public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_HREF="href";
			public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_MEDIA_TYPE="media-type";
			public final static String PKG_MANIFEST_ITEM_ATTRIBUTE_FALLBACK="fallback";
	public final static String PKG_ELEMENT_SPINE="spine";
		public final static String PKG_ELEMENT_SPINE_ITEMREF="itemref";
			public final static String PKG_SPINE_ITEMREF_ATTRIBUTE_IDREF="idref";
	public final static String PKG_ELEMENT_GUIDE="guide";
	public final static String PKG_ELEMENT_TOURS="tours";
		public final static String PKG_ELEMENT_GUIDE_REFERENCE="reference";
			public final static String PKG_GUIDE_REFERENCE_ATTRIBUTE_TYPE="type";
			public final static String PKG_GUIDE_REFERENCE_ATTRIBUTE_TITLE="title";
			public final static String PKG_GUIDE_REFERENCE_ATTRIBUTE_HREF="href";

	/**The media type of an OEB 1.0 package.*/
	public final static ContentType OEB10_PACKAGE_MEDIA_TYPE=new ContentType(ContentTypes.APPLICATION_PRIMARY_TYPE, ContentTypeConstants.X_OEB1_PACKAGE_XML_SUBTYPE, null);

		/**The media type of an OEB 1.0 document.*/
	public final static ContentType OEB10_DOCUMENT_MEDIA_TYPE=new ContentType(ContentTypes.TEXT_PRIMARY_TYPE, ContentTypeConstants.X_OEB1_DOCUMENT_SUBTYPE, null);

		/**The media type of an OEB 1.0 CSS document.*/
	public final static ContentType OEB10_CSS_MEDIA_TYPE=new ContentType(ContentTypes.TEXT_PRIMARY_TYPE, ContentTypeConstants.X_OEB1_CSS_SUBTYPE, null);

	/**The public ID for OEBPS 1.0.*/
	public final static String OEB10_DOCUMENT_PUBLIC_ID="+//ISBN 0-9673008-1-9//DTD OEB 1.0 Document//EN";
	/**The default system ID for OEBPS 1.0.*/
	public final static String OEB10_DOCUMENT_SYSTEM_ID="http://openebook.org/dtds/oeb-1.0/oebdoc1.dtd";

	/**The public ID for OEBPS 1.0.1.*/
	public final static String OEB101_DOCUMENT_PUBLIC_ID="+//ISBN 0-9673008-1-9//DTD OEB 1.0.1 Document//EN";
	/**The default system ID for OEBPS 1.0.1.*/
	public final static String OEB101_DOCUMENT_SYSTEM_ID="http://openebook.org/dtds/oeb-1.0.1/oebdoc101.dtd";

	/**The recommended prefix to the OEB 1.0 namespace.*/
	public static final String OEB1_DOCUMENT_NAMESPACE_PREFIX="oeb1";

	/**The URI to the OEB 1.0 namespace.*/
	public static final URI OEB1_DOCUMENT_NAMESPACE_URI=URI.create("http://openebook.org/namespaces/oeb-document/1.0/");


	//The media types recognized by OEB
//G***del when not needed	public final static String MEDIA_TYPE_TEXT_OEB1_DOCUMENT="text/x-oeb1-document";

	//The OEB 1.0 XML document element names.
	//G***maybe change these into DOC_ELEMENT_*
/*G***del
	public final static String ELEMENT_A="a";
	public final static String ELEMENT_AREA="area";
	public final static String ELEMENT_B="b";
	public final static String ELEMENT_BASE="base";
	public final static String ELEMENT_BIG="big";
	public final static String ELEMENT_BLOCKQUOTE="blockquote";
	public final static String ELEMENT_BODY="body";
	public final static String ELEMENT_BR="br";
	public final static String ELEMENT_CAPTION="caption";
	public final static String ELEMENT_CENTER="center";
	public final static String ELEMENT_CITE="cite";
	public final static String ELEMENT_CODE="code";
	public final static String ELEMENT_DD="dd";
	public final static String ELEMENT_DFN="dfn";
	public final static String ELEMENT_DIV="div";
	public final static String ELEMENT_DL="dl";
	public final static String ELEMENT_DT="dt";
	public final static String ELEMENT_EM="em";
	public final static String ELEMENT_FONT="font";
	public final static String ELEMENT_H1="h1";
	public final static String ELEMENT_H2="h2";
	public final static String ELEMENT_H3="h3";
	public final static String ELEMENT_H4="h4";
	public final static String ELEMENT_H5="h5";
	public final static String ELEMENT_H6="h6";
	public final static String ELEMENT_HEAD="head";
	public final static String ELEMENT_HR="hr";
	public final static String ELEMENT_HTML="html";
	public final static String ELEMENT_I="i";
	public final static String ELEMENT_IMG="img";
	public final static String ELEMENT_KBD="kbd";
	public final static String ELEMENT_LI="li";
	public final static String ELEMENT_LINK="link";
	public final static String ELEMENT_MAP="map";
	public final static String ELEMENT_META="meta";
	public final static String ELEMENT_OBJECT="object";
	public final static String ELEMENT_OL="ol";
	public final static String ELEMENT_P="p";
	public final static String ELEMENT_PARAM="param";
	public final static String ELEMENT_PRE="pre";
	public final static String ELEMENT_Q="q";
	public final static String ELEMENT_S="s";
	public final static String ELEMENT_SAMP="samp";
	public final static String ELEMENT_SCRIPT="script";
	public final static String ELEMENT_SMALL="small";
	public final static String ELEMENT_SPAN="span";
	public final static String ELEMENT_STRIKE="strike";
	public final static String ELEMENT_STRONG="strong";
	public final static String ELEMENT_STYLE="style";
	public final static String ELEMENT_SUB="sub";
	public final static String ELEMENT_SUP="sup";
	public final static String ELEMENT_TABLE="table";
	public final static String ELEMENT_TD="td";
	public final static String ELEMENT_TH="th";
	public final static String ELEMENT_TITLE="title";
	public final static String ELEMENT_TR="tr";
	public final static String ELEMENT_TT="tt";
	public final static String ELEMENT_U="u";
	public final static String ELEMENT_UL="ul";
	public final static String ELEMENT_VAR="var";
*/

/*G***del; moved to XHTMLConstants
		//attributes for <a>
	public final static String ELEMENT_A_ATTRIBUTE_HREF="href";

		//attributes for <applet>
	public final static String ELEMENT_APPLET_ATTRIBUTE_SRC="src";

		//attributes for <img>
	public final static String ELEMENT_IMG_ATTRIBUTE_HEIGHT="height";
	public final static String ELEMENT_IMG_ATTRIBUTE_WIDTH="width";
	public final static String ELEMENT_IMG_ATTRIBUTE_SRC="src";

		//attributes for <object>
	public final static String ELEMENT_OBJECT_ATTRIBUTE_CLASSID="classid";
	public final static String ELEMENT_OBJECT_ATTRIBUTE_CODETYPE="codetype";
	public final static String ELEMENT_OBJECT_ATTRIBUTE_DATA="data";
	public final static String ELEMENT_OBJECT_ATTRIBUTE_HEIGHT="height";
	public final static String ELEMENT_OBJECT_ATTRIBUTE_WIDTH="width";
	public final static String ELEMENT_OBJECT_ATTRIBUTE_TYPE="type";

		//attributes for <param>
	public final static String ELEMENT_PARAM_ATTRIBUTE_NAME="name";
	public final static String ELEMENT_PARAM_ATTRIBUTE_VALUE="value";
*/

	//Property names for OEB1
	public final static String OEB_CSS_PROP_OEB_COLUMN_NUMBER="oeb-column-number";

}
