package com.guiseframework.model;

import java.net.URI;

import com.garretwilson.util.StringTemplate;

import static com.guiseframework.Resources.*;

/**Represents the progress of a task.
@author Garret Wilson
*/
public enum TaskState
{

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

	/**The resource key template for each task state label.*/
	private final static StringTemplate LABEL_RESOURCE_KEY_TEMPLATE=new StringTemplate("theme.task.state.", StringTemplate.STRING_PARAMETER, ".label");
	/**The resource key template for each task state glyph.*/
	private final static StringTemplate GLYPH_RESOURCE_KEY_TEMPLATE=new StringTemplate("theme.task.state.", StringTemplate.STRING_PARAMETER, ".glyph");

	/**@return A resource reference representing a label for no task state.*/
	public static String getNoLabel()
	{
		return createStringResourceReference(LABEL_RESOURCE_KEY_TEMPLATE.apply(""));	//get the label representing no value
	}

	/**@return The resource reference for the task state label.*/
	public String getLabel()
	{
		return createStringResourceReference(LABEL_RESOURCE_KEY_TEMPLATE.apply(getResourceKeyName(this)));	//create a resource reference using the resource key name of this enum value
	}

	/**@return A resource reference representing a glyph for no task state.*/
	public static URI getNoGlyph()
	{
		return createURIResourceReference(GLYPH_RESOURCE_KEY_TEMPLATE.apply(""));	//get the glyph representing no value
	}

	/**@return The resource reference for the task state glyph.*/
	public URI getGlyph()
	{
		return createURIResourceReference(GLYPH_RESOURCE_KEY_TEMPLATE.apply(getResourceKeyName(this)));	//create a resource reference using the resource key name of this enum value
	}

	/**Returns a string representation of the task state.
	This implementation delegates to {@link #getLabel()}.
	@return A string representation of the object.
	*/
	public String toString()
	{
		return getLabel();
	}
}
