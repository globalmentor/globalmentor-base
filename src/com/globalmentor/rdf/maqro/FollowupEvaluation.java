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

package com.globalmentor.rdf.maqro;

import java.net.URI;

import static com.globalmentor.java.Objects.*;
import static com.globalmentor.rdf.maqro.MAQRO.*;

/**Class representing an evaluation to determine a followup.
@author Garret Wilson
*/
public class FollowupEvaluation extends Evaluation
{

	/**@return The local name of the default type of this resource.*/
	public String getDefaultTypeName() {return FOLLOWUP_EVALUATION_CLASS_NAME;}

	/**Default constructor.*/
	public FollowupEvaluation()
	{
	}

	/**Constructs an activity with a reference URI.
	@param referenceURI The reference URI for the new resource.
	*/
	public FollowupEvaluation(final URI referenceURI)
	{
		super(referenceURI);  //construct the parent class
	}

	/**@return The followup interaction indicated by this evaluation, or <code>null</code> if no followup interaction is indicated.*/
	public Interaction getFollowup()
	{
		return asInstance(getPropertyValue(MAQRO_NAMESPACE_URI, FOLLOWUP_PROPERTY_NAME), Interaction.class);	//retrieve the followup interaction, if one is present
	}

	/**Sets the followup for this evaluation.
	@param followup The followup interaction for this evaluation.
	 */
	public void setFollowup(final Interaction followup)
	{
		setProperty(MAQRO_NAMESPACE_URI, FOLLOWUP_PROPERTY_NAME, followup);	//set the followup
	}

}
