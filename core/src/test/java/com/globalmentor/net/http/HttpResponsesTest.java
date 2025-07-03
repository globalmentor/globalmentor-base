/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.net.http;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.net.http.HttpResponse.*;
import java.util.concurrent.CompletionException;

import org.junit.jupiter.api.*;

import com.globalmentor.net.HTTP;

/**
 * Tests of {@link HttpResponses}.
 * @author Garret Wilson
 */
public class HttpResponsesTest {

	/** @see HttpResponses#checkingResponseStatus(BodyHandler, java.util.function.Function) */
	@Test
	void testCheckingResponseStatusDelegatesOnSucces() {
		final BodySubscriber<InputStream> successBodySubscriber = BodySubscribers.ofInputStream();
		final BodyHandler<InputStream> testBodyHandler = HttpResponses.checkingResponseStatus(responseInfo -> successBodySubscriber,
				responseInfo -> new IOException("detected error"));

		final ResponseInfo okMockResponseInfo = mock(ResponseInfo.class);
		when(okMockResponseInfo.statusCode()).thenReturn(HTTP.SC_OK);
		assertThat(testBodyHandler.apply(okMockResponseInfo), is(sameInstance(successBodySubscriber)));

		final ResponseInfo noContentMockResponseInfo = mock(ResponseInfo.class);
		when(noContentMockResponseInfo.statusCode()).thenReturn(HTTP.SC_NO_CONTENT);
		assertThat(testBodyHandler.apply(noContentMockResponseInfo), is(sameInstance(successBodySubscriber)));

		final ResponseInfo notFoundMockResponseInfo = mock(ResponseInfo.class);
		when(notFoundMockResponseInfo.statusCode()).thenReturn(HTTP.SC_NOT_FOUND);
		final BodySubscriber<InputStream> errorBodySubscriber = testBodyHandler.apply(notFoundMockResponseInfo);
		assertThat(errorBodySubscriber, is(not(sameInstance(successBodySubscriber))));
		assertThat(errorBodySubscriber.getBody().toCompletableFuture().isCompletedExceptionally(), is(true));
		final CompletionException completionException = assertThrows(CompletionException.class, () -> errorBodySubscriber.getBody().toCompletableFuture().join());
		assertThat(completionException.getCause(), is(instanceOf(IOException.class)));
		assertThat(completionException.getCause().getMessage(), is("detected error"));
	}

}
