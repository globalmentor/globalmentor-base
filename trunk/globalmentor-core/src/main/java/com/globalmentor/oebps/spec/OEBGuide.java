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

package com.globalmentor.oebps.spec;

/**
 * Represents a guide in OEB 1.x.
 * @author Garret Wilson
 */
public class OEBGuide {	//TODO move to separate, future OEB implementation library

	/**
	 * The type of guide, usually one of the constants defined in <code>OEBGuideConstants</code>.
	 */
	private final String type;

	/**
	 * @return The type of guide, usually one of the constants defined in <code>OEBGuideConstants</code>.
	 */
	public String getType() {
		return type;
	}

	/** The title of this guide. */
	private final String title;

	/** @return The title of this guide which can be used as a visual representation. */
	public String getTitle() {
		return title;
	}

	/** The URI reference to the guide in the manifest. */
	private final String href;

	/** @return The URI reference to the guide in the manifest. */
	public String getHRef() {
		return href;
	}

	/**
	 * Constructs a guide.
	 * @param type The type of guide, usually one of the constants defined in <code>OEBGuideConstants</code>.
	 * @param title The guide title.
	 * @param href The reference to the guide.
	 */
	public OEBGuide(final String type, final String title, final String href) {
		this.type = type; //set the guide type
		this.title = title; //set the guide title
		this.href = href; //set the href of the guide
	}
}