package com.guiseframework.model;

/**Represents the progress of a task.
@author Garret Wilson
*/
public enum TaskStatus
{

	/**The task has been started.*/
	STARTED,
	
	/**The task is temporarily paused.*/
	PAUSED,
	
	/**The task is stopped.*/
	STOPPED,
	
	/**The task has been abandoned.*/
	CANCELED,
	
	/**The task has been completed.*/
	COMPLETE;
}
