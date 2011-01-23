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

package com.globalmentor.jnlp;

import com.globalmentor.net.ContentType;

/**Constant values for accessing the JNLP servlet.
@author Garret Wilson
*/
public class JNLP
{

	/**The content type for JNLP: <code>application/x-java-jnlp-file</code>.*/ 
	public static final ContentType CONTENT_TYPE=ContentType.getInstance(ContentType.APPLICATION_PRIMARY_TYPE, "x-java-jnlp-file");

	/**The name of the JNLP servlet or JSP file.*/
	public final static String JNLP_SERVLET_NAME="jnlp";

	/**The title of the application to pass to the JNLP servlet.*/
	public final static String TITLE_PARAM="title";
	/**The vendor of the application to pass to the JNLP servlet.*/
	public final static String VENDOR_PARAM="vendor";
	/**A context-relative URL fragment that will generate an equivalent JNLP file, to pass to the JNLP servlet.*/
	public final static String HREF_PARAM="href";
	/**An array of resource jar URL strings to pass to the JNLP servlet.*/
	public final static String JARS_PARAM="jars";
	/**The name of the application main class to pass to the JNLP servlet.*/
	public final static String APPLICATION_PARAM="application";
	/**An array of application arguments to pass to the JNLP servlet.*/
	public final static String ARGUMENTS_PARAM="arguments";
	
}
