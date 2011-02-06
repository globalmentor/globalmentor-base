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

package com.globalmentor.java;

/**
 * Indicates that an object supports explicit uninitialization at the end of its life.
 * 
 * <p>When a <dfn>disposble</dfn> object will no longer be used, its {@link #dispose()} method should be called, and the
 * object should not be used further. It is not defined here what component calls the {@link #dispose()} method.
 * Furthermore, the object's behavior if used further is implementation and context-specific.</p>
 * 
 * @author Garret Wilson
 * 
 */
public interface Disposable
{

	/**
	 * Unitializes the object. After calling this method, the object should not be used further.
	 */
	public void dispose();
}
