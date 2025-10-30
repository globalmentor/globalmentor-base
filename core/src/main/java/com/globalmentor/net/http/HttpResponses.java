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

import static com.globalmentor.net.HTTP.ResponseClass.*;

import java.io.IOException;
import java.net.http.HttpResponse.*;
import java.util.function.Function;

import org.jspecify.annotations.*;

/**
 * Utilities for working with {@link java.net.http.HttpResponse} and related classes, such as {@link BodyHandler} and {@link BodySubscriber}
 * @author Garret Wilson
 */
public class HttpResponses {

	private HttpResponses() {
	}

	/**
	 * Wraps an existing body handler with one that adds automatic checking of the response status code. If the status code is not successful (i.e. in the
	 * <code>2XX</code> category), a body subscriber is returned that discards the body content and reports an error throwable.
	 * @implSpec This implementation delegates to {@link #checkingResponseStatus(BodyHandler, Function)}, creating an {@link IOException} with a general message
	 *           if there was an error.
	 * @param <T> The response body type.
	 * @param bodyHandler The original body handler, which presumably does not check the status code.
	 * @return A new body handler that checks the status, reports an error if appropriate, but otherwise delegates to the given body handler to function normally.
	 */
	public static <T> BodyHandler<T> checkingResponseStatus(@NonNull final BodyHandler<T> bodyHandler) {
		return checkingResponseStatus(bodyHandler,
				responseInfo -> new IOException("HTTP request failed with response status code %d.".formatted(responseInfo.statusCode())));
	}

	/**
	 * Wraps an existing body handler with one that adds automatic checking of the response status code. If the status code is not successful (i.e. in the
	 * <code>2XX</code> category), a body subscriber is returned that discards the body content and reports an error throwable, produced by the given error
	 * strategy.
	 * @apiNote Typically an error strategy would use the response of {@link ResponseInfo#statusCode()} when producing an error object.
	 * @param <T> The response body type.
	 * @param bodyHandler The original body handler, which presumably does not check the status code.
	 * @param errorStrategy The strategy for producing an error object to report if the status code is not successful.
	 * @return A new body handler that checks the status, reports an error if appropriate, but otherwise delegates to the given body handler to function normally.
	 */
	public static <T> BodyHandler<T> checkingResponseStatus(@NonNull final BodyHandler<T> bodyHandler,
			@NonNull final Function<ResponseInfo, ? extends Throwable> errorStrategy) {
		return responseInfo -> switch(forStatusCode(responseInfo.statusCode())) {
			case SUCCESSFUL -> bodyHandler.apply(responseInfo);
			default -> {
				final BodySubscriber<T> discardingErrorSubscriber = BodySubscribers.replacing(null);
				discardingErrorSubscriber.onError(errorStrategy.apply(responseInfo));
				yield discardingErrorSubscriber;
			}
		};
	}

}
