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

package com.globalmentor.urf.maqro;

import java.net.URI;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.urf.URF.*;
import static com.globalmentor.urf.maqro.MAQRO.*;

/**The result of a score evalution.
@author Garret Wilson
*/
public class Score extends AbstractResult
{

	/**Default constructor.*/
	public Score()
	{
		this(null);	//construct the class with no URI
	}

	/**Interaction constructor.
	@param interaction The interaction this result represents.
	*/
/*TODO fix evaluation constructor
	public Score(final Interaction interaction)
	{
		this();	//construct the class
		setInteraction(interaction);	//set the interaction
	}
*/
	
	/**URI constructor.
	@param uri The URI for the new resource.
	*/
	public Score(final URI uri)
	{
		super(uri);  //construct the parent class
	}

	/**Sets the actual value of the score.
	@param actual The actual score value.
	*/
	public void setActual(final Number actual)
	{
		setPropertyValue(ACTUAL_PROPERTY_URI, actual);	//set the actual value of this score resource
	}

	/**Retrieves the actual value of the score.
	@return The actual score value, or <code>null</code> if there is no actual value or the actual is not a number.
	*/
	public Number getActual()
	{
		return asNumber(getPropertyValue(ACTUAL_PROPERTY_URI));	//get the actual value of this score resource
	}

	/**Sets the possible value of the score.
	@param possible The score possible value.
	*/
	public void setPossible(final Number possible)
	{
		setPropertyValue(POSSIBLE_PROPERTY_URI, possible);	//set the possible value of this score resource
	}

	/**Retrieves the number possible value of the score.
	@return The possible score value, or <code>null</code> if there is no possible value or the possible value is not a number.
	*/
	public Number getPossible()
	{
		return asNumber(getPropertyValue(POSSIBLE_PROPERTY_URI));	//get the possible value of this score resource
	}
	
}
