/*
 * Copyright © 2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static com.globalmentor.java.Classes.*;

import java.beans.PropertyVetoException;

/**
 * A task performed in a sequence.
 * @author Garret Wilson
 */
public interface SequenceTask extends Task {

	/** The bound, constrained property of the sequence index, of type {@link Integer}. */
	public static final String SEQUENCE_INDEX_PROPERTY = getPropertyName(SequenceTask.class, "sequenceIndex");

	/**
	 * Returns the index in the sequence. This is a constrained property of type {@link Integer}.
	 * @return The index in the sequence, or -1 if the sequence has not started.
	 */
	public int getSequenceIndex();

	/** Starts the sequence by going to the first step in the sequence. */
	public void goStart();

	/**
	 * Goes to the first step in the sequence.
	 * @throws PropertyVetoException if the sequence index change has been vetoed.
	 */
	public void goFirst() throws PropertyVetoException;

	/**
	 * Determines if there is a previous step in the sequence.
	 * @return <code>true</code> if there is a previous step before the current one.
	 */
	public boolean hasPrevious();

	/**
	 * Goes to the previous step in the sequence. If there is no previous component, no action occurs.
	 * @throws PropertyVetoException if the sequence index change has been vetoed.
	 * @see #hasPrevious()
	 */
	public void goPrevious() throws PropertyVetoException;

	/**
	 * Determines if there is a next step in the sequence.
	 * @return <code>true</code> if there is a next step after the current one.
	 */
	public boolean hasNext();

	/**
	 * Goes to the next step in the sequence. If there is no next step, no action occurs.
	 * @see #hasNext()
	 * @throws PropertyVetoException if the sequence index change has been vetoed.
	 */
	public void goNext() throws PropertyVetoException;

	/** Verifies the contents and finishes the sequence. */
	public void goFinish();

}
