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

package com.globalmentor.rdf.dicto;

import java.net.URI;

/**
 * Constants used in Dictionary Ontology (Dicto) MAQRO processing.
 * @author Garret Wilson
 */
public class DictoMAQRO {

	/** The recommended prefix to the Dicto MAQRO ontology namespace. */
	public final static String DICTO_MAQRO_NAMESPACE_PREFIX = "dictomaqro";
	/** The URI to the Dicto MAQRO namespace. */
	public final static URI DICTO_MAQRO_NAMESPACE_URI = URI.create("http://garretwilson.com/namespaces/2004/dictomaqro#");

	//Dicto MAQRO property names
	/** The property to be used when forming MAQRO question queries. The local name of dictomaqro:queryProperty. */
	public final static String QUERY_PROPERTY_PROPERTY_NAME = "queryProperty";
	/** The property to be used when forming MAQRO question choices. The local name of dictomaqro:choiceProperty. */
	public final static String CHOICE_PROPERTY_PROPERTY_NAME = "choiceProperty";

}