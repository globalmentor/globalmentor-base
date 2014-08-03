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

package com.globalmentor.cache;

import java.util.EventListener;

/**
 * Indicates the implementing class can listen for a value being fetched in a cache.
 * @param <Q> The type of query used to request data from the cache.
 * @param <V> The type of value stored in the cache.
 * @see Cache
 * @see CacheFetchEvent
 * @author Garret Wilson
 */
public interface CacheFetchListener<Q, V> extends EventListener {

	/**
	 * Called when a value is fetched in the cache.
	 * @param cacheFetchEvent The event identifying the value fetched.
	 */
	public void fetched(final CacheFetchEvent<Q, V> cacheFetchEvent);

}