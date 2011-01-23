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

package com.globalmentor.model;

import static com.globalmentor.java.Classes.*;

/**Indicates that an implementing class is a data model.
@author Garret Wilson
*/
@Deprecated
public interface Model //TODO del if not needed extends Modifiable
{
	/**The property for the data model for which a component provides a view.*/
	public final String MODEL_PROPERTY=getFullName(Model.class, "model");
	
}
