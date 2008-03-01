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

package com.globalmentor.util;

import java.util.Collection;

import java.util.concurrent.locks.ReadWriteLock;

/**A thread-safe collection that uses a pair of read and write locks to access its data.
@param <E> The type of elements in the collection.
@author Garret Wilson
*/
public interface ReadWriteLockCollection<E> extends Collection<E>, ReadWriteLock
{
}
