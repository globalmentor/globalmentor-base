package com.garretwilson.jnlp;

/**Constant values for accessing the JNLP servlet.
@author Garret Wilson
*/
public class JNLPServletConstants
{

	/**The name of the JNLP servlet or JSP file.*/
	public final static String JNLP_SERVLET_NAME="jnlp";

	/**The title of the application to pass to the JNLP servlet.*/
	public final static String JNLP_TITLE_PARAM="title";
	/**The vendor of the application to pass to the JNLP servlet.*/
	public final static String JNLP_VENDOR_PARAM="vendor";
	/**A context-relative URL fragment that will generate an equivalent JNLP file, to pass to the JNLP servlet.*/
	public final static String JNLP_HREF_PARAM="href";
	/**An array of resource jar URL strings to pass to the JNLP servlet.*/
	public final static String JNLP_JARS_PARAM="jars";
	/**The name of the application main class to pass to the JNLP servlet.*/
	public final static String JNLP_APPLICATION_PARAM="application";
	/**An array of application arguments to pass to the JNLP servlet.*/
	public final static String JNLP_ARGUMENTS_PARAM="arguments";
	
}
