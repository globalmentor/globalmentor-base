/*
 * Copyright © 2007-2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.dcmi;

import java.net.URI;

/**
 * Constants and methods used for Dublin Core.
 * @author Garret Wilson
 * @see <a href="http://dublincore.org/documents/dcmi-namespace/">DCMI Namespace Policy</a>
 * @see <a href="http://dublincore.org/documents/dces/">Dublin Core Metadata Element Set, Version 1.1</a>
 */
public class DCMI
{

	/** The recommended prefix of the Dublin Core Metadata Initiative elements namespace. */
	public final static String DCMI_ELEMENTS_NAMESPACE_PREFIX = "dc";
	/** The URI to the Dublin Core Metadata Initiative element set 1.0 namespace. */
	public final static URI DCMI10_ELEMENTS_NAMESPACE_URI = URI.create("http://purl.org/dc/elements/1.0/");
	/** The URI to the Dublin Core Metadata Initiative element set 1.1 namespace. */
	public final static URI DCMI11_ELEMENTS_NAMESPACE_URI = URI.create("http://purl.org/dc/elements/1.1/");

	//Dublin Core property names
	/** The title of a resource. */
	public final static String TITLE_PROPERTY_NAME = "title";
	/** The creator of a resource. */
	public final static String CREATOR_PROPERTY_NAME = "creator";
	/** The subject of a resource. */
	public final static String SUBJECT_PROPERTY_NAME = "subject";
	/** The description of a resource. */
	public final static String DESCRIPTION_PROPERTY_NAME = "description";
	/** The publisher of a resource. */
	public final static String PUBLISHER_PROPERTY_NAME = "publisher";
	/** The contributor of a resource. */
	public final static String CONTRIBUTOR_PROPERTY_NAME = "contributor";
	/** The date of a resource. */
	public final static String DATE_PROPERTY_NAME = "date";
	/** The Dublin Core type of a resource. */
	public final static String TYPE_PROPERTY_NAME = "type";
	/** The format of a resource. */
	public final static String FORMAT_PROPERTY_NAME = "format";
	/** The Dublin Core identifier of a resource. */
	public final static String IDENTIFIER_PROPERTY_NAME = "identifier";
	/** The source of a resource. */
	public final static String SOURCE_PROPERTY_NAME = "source";
	/** The language of a resource. */
	public final static String LANGUAGE_PROPERTY_NAME = "language";
	/** The relation of a resource. */
	public final static String RELATION_PROPERTY_NAME = "relation";
	/** The coverage of a resource. */
	public final static String COVERAGE_PROPERTY_NAME = "coverage";
	/** The rights of a resource. */
	public final static String RIGHTS_PROPERTY_NAME = "rights";

	//Dublin Core property URIs
	/** The title of a resource. */
	public final static URI TITLE_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(TITLE_PROPERTY_NAME);
	/** The creator of a resource. */
	public final static URI CREATOR_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(CREATOR_PROPERTY_NAME);
	/** The subject of a resource. */
	public final static URI SUBJECT_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(SUBJECT_PROPERTY_NAME);
	/** The description of a resource. */
	public final static URI DESCRIPTION_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(DESCRIPTION_PROPERTY_NAME);
	/** The publisher of a resource. */
	public final static URI PUBLISHER_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(PUBLISHER_PROPERTY_NAME);
	/** The contributor of a resource. */
	public final static URI CONTRIBUTOR_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(CONTRIBUTOR_PROPERTY_NAME);
	/** The date of a resource. */
	public final static URI DATE_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(DATE_PROPERTY_NAME);
	/** The Dublin Core type of a resource. */
	public final static URI TYPE_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(TYPE_PROPERTY_NAME);
	/** The format of a resource. */
	public final static URI FORMAT_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(FORMAT_PROPERTY_NAME);
	/** The Dublin Core identifier of a resource. */
	public final static URI IDENTIFIER_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(IDENTIFIER_PROPERTY_NAME);
	/** The source of a resource. */
	public final static URI SOURCE_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(SOURCE_PROPERTY_NAME);
	/** The language of a resource. */
	public final static URI LANGUAGE_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(LANGUAGE_PROPERTY_NAME);
	/** The relation of a resource. */
	public final static URI RELATION_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(RELATION_PROPERTY_NAME);
	/** The coverage of a resource. */
	public final static URI COVERAGE_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(COVERAGE_PROPERTY_NAME);
	/** The rights of a resource. */
	public final static URI RIGHTS_PROPERTY_URI = DCMI11_ELEMENTS_NAMESPACE_URI.resolve(RIGHTS_PROPERTY_NAME);

}