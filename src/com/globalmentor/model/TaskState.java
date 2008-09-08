/*
 * Copyright © 2005-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

/**Represents the progress of a task.
@author Garret Wilson
*/
public enum TaskState
{

	/**The task has not yet begun.*/
	UNSTARTED,

	/**The task is preparing to begin.*/
	INITIALIZE,

	/**The task has been started but is incomplete.*/
	INCOMPLETE,

	/**The task has been started but there is an error.*/
	ERROR,

	/**The task is temporarily paused.*/
	PAUSED,
	
	/**The task is stopped.*/
	STOPPED,
	
	/**The task has been abandoned.*/
	CANCELED,
	
	/**The task has been completed.*/
	COMPLETE;

	/**The resource key template for each label.*/
//TODO del	private final static StringTemplate LABEL_RESOURCE_KEY_TEMPLATE=new StringTemplate("task.state.", StringTemplate.STRING_PARAMETER, ".label");
	/**The resource key template for each glyph.*/
//TODO del	private final static StringTemplate GLYPH_RESOURCE_KEY_TEMPLATE=new StringTemplate("task.state.", StringTemplate.STRING_PARAMETER, ".glyph");

	/**@return A resource reference representing a label for no task state.*/
/*TODO del
	public static String getNoLabel()
	{
		return createStringResourceReference(LABEL_RESOURCE_KEY_TEMPLATE.apply(""));	//get the label representing no value
	}
*/

	/**@return The resource reference for the label.*/
/*TODO del
	public String getLabel()
	{
		return createStringResourceReference(LABEL_RESOURCE_KEY_TEMPLATE.apply(getResourceKeyName(this)));	//create a resource reference using the resource key name of this enum value
	}
*/

	/**@return A resource reference representing a glyph for no task state.*/
/*TODO del
	public static URI getNoGlyph()
	{
		return createURIResourceReference(GLYPH_RESOURCE_KEY_TEMPLATE.apply(""));	//get the glyph representing no value
	}
*/

	/**@return The resource reference for the glyph.*/
/*TODO del
	public URI getGlyph()
	{
		return createURIResourceReference(GLYPH_RESOURCE_KEY_TEMPLATE.apply(getResourceKeyName(this)));	//create a resource reference using the resource key name of this enum value
	}
*/

	/**Returns a string representation of the task state.
	This implementation delegates to {@link #getLabel()}.
	@return A string representation of the object.
	*/
/*TODO del
	public String toString()
	{
		return getLabel();
	}
*/
}
