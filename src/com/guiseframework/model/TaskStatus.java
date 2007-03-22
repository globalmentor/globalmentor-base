package com.guiseframework.model;

/**Represents the progress of a task.
@author Garret Wilson
*/
public enum TaskStatus
{

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

	/**The resource key format pattern for each task status glyph.*/
	public final static String GLYPH_RESOURCE_KEY_FORMAT_PATTERN="theme.task.status.{0}.glyph";
}
