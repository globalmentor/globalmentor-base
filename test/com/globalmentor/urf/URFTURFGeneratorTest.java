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

package com.globalmentor.urf;

import java.io.*;
import java.net.*;

import com.globalmentor.io.Charsets;
import com.globalmentor.log.Log;

import static org.junit.Assert.*;
import org.junit.Test;

/**Tests for URF generation of TURF.
@author Garret Wilson
*/
public class URFTURFGeneratorTest
{

	/**Retrieves URF for testing from a resource.
	@param resourceName The name of the TURF resource to retrieve.
	@return URF from a TURF resource.
	@throws IOException If there was an error parsing the TURF.
	*/
	protected URF getTestURF(final String resourceName) throws IOException, URISyntaxException
	{
		final URL turfURL=getClass().getResource(resourceName);
		final Reader reader=new LineNumberReader(new InputStreamReader(turfURL.openStream(), Charsets.UTF_8_CHARSET));
		try
		{
			final URFTURFProcessor urfProcessor=new URFTURFProcessor();
			urfProcessor.process(reader, turfURL.toURI());
			return urfProcessor.getURF();
		}
		catch(final URISyntaxException uriSyntaxException)
		{
			throw new AssertionError(uriSyntaxException);
		}
		finally
		{
			reader.close();
		}
	}

	/**Uses the given generator to generate the source URF and compare the output to the original.
	This method asserts that the generated URF matches that from the source.
	@param generator The configured TURF generator.
	@param sourceURF The URF to generate.
	@throws IOException if there was an error generating or otherwise processing the URF. 
	*/
	protected void testGeneration(final URFTURFGenerator generator, final URF sourceURF) throws IOException
	{
		final URFResource sourceResource=sourceURF.getResourceByTypeURI(URI.create("http://guiseframework.com/namespaces/theme#Theme"));
		assertNotNull("Could not find resource from source.", sourceResource);
		final StringWriter writer=new StringWriter();
		generator.generateResources(writer, sourceResource);
		Log.info(writer.toString());
		final URFTURFProcessor urfProcessor=new URFTURFProcessor();
		final StringReader reader=new StringReader(writer.toString());
		urfProcessor.process(new LineNumberReader(reader), null);
		final URFResource destinationResource=urfProcessor.getURF().getResourceByTypeURI(URI.create("http://guiseframework.com/namespaces/theme#Theme"));
		assertNotNull("Could not find resource from source.", sourceResource);
		assertTrue("The generated TURF is not equavalent to what was parsed", sourceResource.equals(destinationResource));
	}

	/**Tests that the TURF generator correctly generates a complex, formatted document.*/
	@Test
	public void testComplexFormattedTURFGeneration() throws IOException, URISyntaxException
	{
		final URF sourceURF=getTestURF("complex-comma-list-separated.turf");
		final URFTURFGenerator generator=new URFTURFGenerator();
		generator.setFormatted(true);	//test formatted output
		testGeneration(generator, sourceURF);
	}

	/**Tests that the TURF generator correctly generates a complex, formatted document using list delimiters.*/
	@Test
	public void testComplexFormattedListDelimiterTURFGeneration() throws IOException, URISyntaxException
	{
		final URF sourceURF=getTestURF("complex-comma-list-separated.turf");
		final URFTURFGenerator generator=new URFTURFGenerator();
		generator.setFormatted(true);	//test formatted output
		generator.setFormattedListDelimiter(true);	//test formatted output with redundant list delimiters
		testGeneration(generator, sourceURF);
	}

	/**Tests that the TURF generator correctly generates a complex, unformatted document.*/
	@Test
	public void testComplexUnformattedTURFGeneration() throws IOException, URISyntaxException
	{
		final URF sourceURF=getTestURF("complex-comma-list-separated.turf");
		final URFTURFGenerator generator=new URFTURFGenerator();
		generator.setFormatted(false);	//test unformatted output
		testGeneration(generator, sourceURF);
	}
}
