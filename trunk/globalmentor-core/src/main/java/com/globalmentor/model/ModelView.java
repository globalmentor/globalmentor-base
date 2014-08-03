/*
 * Copyright © 1996-2009 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.model;

/**
 * Represents a view of a model.
 * @author Garret Wilson
 */
@Deprecated
public interface ModelView<M> extends Verifiable {

	/**
	 * Returns the view data model.
	 * <p>
	 * A calling program should first call {@link #verify()} to ensure the data is valid and that the model reflects the currently entered data.
	 * </p>
	 * @return The data model for which this component provides a view.
	 * @see #verify()
	 */
	public M getModel();

	/**
	 * Sets the data model.
	 * @param newModel The data model for which this object provides a view.
	 */
	public void setModel(final M newModel);

}
