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

package com.globalmentor.model;

import static com.globalmentor.java.Classes.*;

/**Indicates the object not only can be modified, it can keep track of whether it has been modified.
@author Garret Wilson
*/
public interface Modifiable
{

	/**The name of the modified property, if it is bound in any modifiable object.*/
	public final static String MODIFIED_PROPERTY=getPropertyName(Modifiable.class, "modified");

	/**@return Whether the object has been modified.*/
	public boolean isModified();

	/**Sets whether the object has been modified.
	@param newModified The new modification status.
	*/
	public void setModified(final boolean newModified);

}