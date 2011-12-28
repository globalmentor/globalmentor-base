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
 * <p>
 * When a <dfn>disposable</dfn> object will no longer be used, its {@link #dispose()} method should be called, and the object should not be used further. The
 * {@link #dispose()} can be called multiple times without danger. It is not defined here what component calls the {@link #dispose()} method. Furthermore, the
 * object's behavior if used further is implementation and context-specific.
 * </p>
 * 
 * <p>
 * The {@link #dispose()} method is considered to be a last-chance opportunity to release resources; it therefore should allow no exceptions to escape.
 * Disposing an object is considered to be an operation more final than <code>close()</code> (although a <code>close()</code> method may call {@link #dispose()}
 * ), but less final than <code>finalize()</code> (although <code>finalize()</code> may and should call {@link #dispose()}). One of its uses is in the following
 * pattern:
 * </p>
 * <blockquote>
 * 
 * <pre>
 * <code>
 * Disposable myDisposable=createDisposable();
 * try
 * {
 *   ...
 * }
 * finally
 * {
 *   myDisposable.dispose();
 * }
 * </code>
 * </pre>
 * 
 * </blockquote>
 * 
 * <p>
 * Such a pattern prevents the <code>try{} finally{ object.close() }</code> problem in which closing the object attempts to retry an operation which caused an
 * exception in the first place.
 * </p>
 * 
 * @author Garret Wilson
 * 
 */
public interface Disposable
{

	/**
	 * Uninitializes the object. After calling this method, the object should not be used further. This method can be called multiple times without danger. All
	 * exceptions should be caught and dealt with in this method. Child classes must call the parent class version.
	 */
	public void dispose();
}
