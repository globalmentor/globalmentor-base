/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

/**
 * A resource that represents some value object instance. The value object is normally immutable.
 * 
 * @param <V> The type of value represented by the resource.
 * 
 * @author Garret Wilson
 */
public interface ValueResource<V> extends Resource {

	/** @return The class representing the type of value represented by the resource. */
	public Class<V> getValueClass();

	/** @return The non-<code>null</code> value represented by the resource. */
	public V getValue();

}