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

import java.io.*;
import java.util.Date;

/**An abstract cache that stores cached values in files.
@param <K> The type of key used to look up data in the cache.
@param <Q> The type of query used to request data from the cache.
@author Garret Wilson
*/
public abstract class AbstractFileCache<K, Q extends AbstractCache.Query<K>> extends AbstractCache<K, Q, File, AbstractFileCache.FileData>
{
	
	/**Constructor.
	@param fetchSynchronous Whether fetches for new values should occur synchronously.
	@param expiration The length of time, in milliseconds, to keep cached information.
	*/
	public AbstractFileCache(final boolean fetchSynchronous, final long expiration)
	{
		super(fetchSynchronous, expiration);
	}

	/**Class for storing cached file information.
	If no modified time is known, this will not influence the staleness determination of cached information.
	@author Garret Wilson
	*/
	public static class FileData extends Cache.Data<File>
	{

		/**The last known modified time of the resource represented, or <code>null</code> if the last modified time is not known.*/
		private final Date modifiedTime;

			/**@return The last known modified time of the resource represented, or <code>null</code> if the last modified time is not known.*/
			public Date getModifiedTime() {return modifiedTime;}

		/**File constructor.
		@param file The file to store.
		@param modifiedTime The last known modified time of the resource represented, or <code>null</code> if the last modified time is not known.
		*/
		public FileData(final File value, final Date modifiedTime)
		{
			super(value);	//construct the parent class
			this.modifiedTime=modifiedTime;
		}
	}

}
