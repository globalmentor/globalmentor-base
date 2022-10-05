/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.net;

import java.io.IOException;
import java.net.URI;
import com.globalmentor.io.URIAccessible;
import com.globalmentor.util.prefs.Preferencesable;

/**
 * An interface to an object that allows resources to be selected for input or output.
 * @param <R> The type of resource being selected.
 * @author Garret Wilson
 * @deprecated for removal; it's not clear what this class was originally used for.
 */
@Deprecated
public interface ResourceSelector<R extends Resource> extends URIAccessible, Preferencesable {

	/**
	 * Retrieves a description of the resource with the given reference URI.
	 * @param referenceURI The reference URI of the resource in question.
	 * @return A description of the identified resource.
	 * @throws IOException Thrown if there is an error retrieving the resource description.
	 */
	public R getResource(final URI referenceURI) throws IOException;

	/**
	 * Selects a resource for input.
	 * @param oldResource The currently selected resource, if applicable, or <code>null</code> if there is no selected resource.
	 * @return The selected resource, or <code>null</code> if selection was canceled.
	 * @throws SecurityException Thrown if selecting an input resource is not allowed.
	 * @throws IOException Thrown if there is an error locating a resource.
	 */
	public R selectInputResource(final R oldResource) throws SecurityException, IOException;

	/**
	 * Selects a resource for output.
	 * @param oldResource The currently selected resource, if applicable, or <code>null</code> if there is no selected resource.
	 * @return The selected resource, or <code>null</code> if selection was canceled.
	 * @throws SecurityException Thrown if selecting an output resource is not allowed.
	 * @throws IOException Thrown if there is an error locating a resource.
	 */
	public R selectOutputResource(final R oldResource) throws SecurityException, IOException;

}
