/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.io;

import java.io.*;

/**
 * A file filter that accepts all files.
 * 
 * <p>
 * This class is provided to allow creation of a filter that also implements {@link FilenameFilter}. Normally {@link Files#WILDCARD_FILE_FILTER} should be used
 * in preference to creating a new instance of this class.
 * </p>
 * 
 * @author Garret Wilson
 */
public class WildcardFileFilter extends AbstractFileFilter {

	/** Constructor. */
	public WildcardFileFilter() {
	}

	@Override
	public boolean accept(final File pathname) {
		return true;
	}
}
