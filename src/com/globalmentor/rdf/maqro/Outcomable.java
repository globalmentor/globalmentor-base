package com.globalmentor.rdf.maqro;

/**Indicates a class can provide a MAQRO outcome representing a user's response.*/
public interface Outcomable
{

	/**Retrieves user response information for the associated interaction and returns the outcome information.
	@return An object representing the current state of user responses and outcome for the current interaction,
		or <code>null</code> if no results are available for the associated interaction.
	*/
	public Outcome getOutcome();

}
