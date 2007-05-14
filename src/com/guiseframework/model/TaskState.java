package com.guiseframework.model;

import java.net.URI;
import java.text.MessageFormat;

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

	/**The resource key format pattern for each task state label.*/
	private final static String LABEL_RESOURCE_KEY_FORMAT_PATTERN="theme.task.state.{0}.label";
	/**The resource key format pattern for each task state glyph.*/
	private final static String GLYPH_RESOURCE_KEY_FORMAT_PATTERN="theme.task.state.{0}.glyph";

	/**@return A resource reference representing a label for no task state.*/
	public static String getNoLabel()
	{
		return createStringResourceReference(MessageFormat.format(LABEL_RESOURCE_KEY_FORMAT_PATTERN, ""));	//get the label representing no task state
	}

	/**@return The resource reference for the task state label.*/
	public String getLabel()
	{
		return createStringResourceReference(MessageFormat.format(LABEL_RESOURCE_KEY_FORMAT_PATTERN, getResourceKeyName(this)));	//create a resource reference using the resource key name of this enum value
	}

	/**@return A resource reference representing a glyph for no task state.*/
	public static URI getNoGlyph()
	{
		return createURIResourceReference(MessageFormat.format(GLYPH_RESOURCE_KEY_FORMAT_PATTERN, ""));	//get the glyph representing no task state
	}

	/**@return The resource reference for the task state glyph.*/
	public URI getGlyph()
	{
		return createURIResourceReference(MessageFormat.format(GLYPH_RESOURCE_KEY_FORMAT_PATTERN, getResourceKeyName(this)));	//create a resource reference using the resource key name of this enum value
	}
}
