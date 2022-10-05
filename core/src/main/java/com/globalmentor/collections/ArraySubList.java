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

package com.globalmentor.collections;

import java.util.ArrayList;

/**
 * Represents a list that is a subset of some larger list, based upon an array.
 * @author Garret Wilson
 */
public class ArraySubList<E> extends ArrayList<E> implements SubList<E> {

	private static final long serialVersionUID = -4622676698966860986L;

	/** The size of the super-list of which this list is a sublist. */
	private int superListSize = 0;

	/** @return The size of the super-list of which this list is a sublist. */
	public int getSuperListSize() {
		return superListSize;
	}

	/**
	 * Sets the size of the super-list of which this list is a sublist.
	 * @param newSuperListSize The size of the super-list.
	 */
	public void setSuperListSize(final int newSuperListSize) {
		superListSize = newSuperListSize;
	}

	/** The index of the super-list at which this list starts. */
	private int startIndex = 0;

	/** @return The index of the super-list at which this list starts. */
	public int getStartIndex() {
		return startIndex;
	}

	/**
	 * Sets The index of the super-list at which this list starts.
	 * @param newStartIndex The new starting index.
	 */
	public void setStartIndex(final int newStartIndex) {
		startIndex = newStartIndex;
	}

}
