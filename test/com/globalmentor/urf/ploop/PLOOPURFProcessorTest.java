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

package com.globalmentor.urf.ploop;

import java.io.*;
import java.net.*;

//TODO fix import com.guiseframework.AbstractGuiseApplication;

import org.junit.Test;

/**Tests for URF processing of TURF.
@author Garret Wilson
*/
public class PLOOPURFProcessorTest
{

	/**@return The I/O implementation that reads a Guise application description from TURF.*/
/*TODO fix
	protected PLOOPTURFIO<AbstractGuiseApplication> getApplicationIO() {
		return new PLOOPTURFIO<AbstractGuiseApplication>(AbstractGuiseApplication.class);	//create the Guise application I/O
	}
*/

	/**Tests that the URF parser can parse a legacy TURF file containing old, deprecated features.*/
/*TODO fix
	@Test
	public void testMarmoxApplication() throws IOException, URISyntaxException
	{
		final PLOOPTURFIO<AbstractGuiseApplication> io=getApplicationIO();
		final URL turfURL=getClass().getResource("marmox-application.turf");
		final InputStream inputStream=turfURL.openStream();
		try
		{
			getApplicationIO().read(inputStream, turfURL.toURI());	//read the application description from the PLOOP TURF, using the URI of the application description as the base URI
		}
		finally
		{
			inputStream.close();
		}
	}
*/
}
