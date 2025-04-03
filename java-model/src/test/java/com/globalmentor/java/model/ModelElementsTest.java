/*
 * Copyright © 2024 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java.model;

import static java.util.Objects.*;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.extension.ExtendWith;

import com.globalmentor.java.AbstractAnnotationsTest;
import com.globalmentor.java.Annotations;
import com.karuslabs.elementary.junit.Labels;
import com.karuslabs.elementary.junit.ToolsExtension;
import com.karuslabs.elementary.junit.annotations.*;

/**
 * Tests of {@link ModelElements} and its implementation of {@link Annotations}.
 * @author Garret Wilson
 */
@ExtendWith(ToolsExtension.class)
@Introspect
public class ModelElementsTest extends AbstractAnnotationsTest {

	private final Labels labels;

	ModelElementsTest(final Labels labels) {
		this.labels = requireNonNull(labels);
	}

	/**
	 * {@inheritDoc}
	 * @implSpec This implementation returns an {@link Annotations} implementation by delegating to
	 *           {@link ModelElements#annotationsOf(javax.lang.model.element.Element)}
	 */
	@Override
	protected Annotations getMethodTestAnnotations(final String methodName) {
		return ModelElements.annotationsOf(labels.get(methodName));
	}

	/**
	 * Interface for testing annotations.
	 * @apiNote The annotations here have no semantic significance; they are only for testing annotation access.
	 */
	interface TestAnnotatedInterface {

		@BeforeEach
		@Label("methodWithAnnotationHavingNoValue")
		public void methodWithAnnotationHavingNoValue();

		@Disabled("value for testing")
		@Label("methodWithAnnotationHavingValue")
		public void methodWithAnnotationHavingValue();

	}

}
