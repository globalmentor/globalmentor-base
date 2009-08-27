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

/**Tests for URF processing of TURF.
@author Garret Wilson
*/
public class URFTURFProcessorTest
{

	/**Tests that the URF parser can parse a complex, comma-list-separated file.*/
	@Test
	public void testComplexCommaListSeparatedTURF() throws IOException, URISyntaxException
	{
		final URFTURFProcessor urfProcessor=new URFTURFProcessor();
		final URL turfURL=getClass().getResource("complex-comma-list-separated.turf");
		final Reader reader=new LineNumberReader(new InputStreamReader(turfURL.openStream(), Charsets.UTF_8_CHARSET));
		try
		{
			urfProcessor.process(reader, turfURL.toURI());
		}
		finally
		{
			reader.close();
		}
		Log.info(URF.toString(urfProcessor.getURF()));
	}

	/**Tests that the URF parser can parse a complex, comma-list-separated file.*/
	@Test
	public void testComplexNewlineListSeparatedTURF() throws IOException, URISyntaxException
	{
		final URFTURFProcessor urfProcessor=new URFTURFProcessor();
		final URL turfURL=getClass().getResource("complex-newline-list-separated.turf");
		final Reader reader=new LineNumberReader(new InputStreamReader(turfURL.openStream(), Charsets.UTF_8_CHARSET));
		try
		{
			urfProcessor.process(reader, turfURL.toURI());
		}
		finally
		{
			reader.close();
		}
		Log.debug(URF.toString(urfProcessor.getURF()));
	}

	/**Tests that the URF parser can parse two complex, comma-list-separated and comma-newline-separated file and consider them equal.*/
	@Test
	public void testComplexEquality() throws IOException, URISyntaxException
	{
		final URFResource resource1, resource2;
		final URL turf1URL=getClass().getResource("complex-comma-list-separated.turf");
		final Reader reader1=new LineNumberReader(new InputStreamReader(turf1URL.openStream(), Charsets.UTF_8_CHARSET));
		try
		{
			final URFTURFProcessor urfProcessor=new URFTURFProcessor();
			urfProcessor.process(reader1, turf1URL.toURI());
			resource1=urfProcessor.getURF().getResourceByTypeURI(URI.create("http://guiseframework.com/namespaces/theme#Theme"));
			assertNotNull("Could not find parsed resource from second file.", resource1);
		}
		finally
		{
			reader1.close();
		}
		final URL turf2URL=getClass().getResource("complex-newline-list-separated.turf");
		final Reader reader2=new LineNumberReader(new InputStreamReader(turf2URL.openStream(), Charsets.UTF_8_CHARSET));
		try
		{
			final URFTURFProcessor urfProcessor=new URFTURFProcessor();
			urfProcessor.process(reader2, turf2URL.toURI());
			resource2=urfProcessor.getURF().getResourceByTypeURI(URI.create("http://guiseframework.com/namespaces/theme#Theme"));
			assertNotNull("Could not find parsed resource from second file.", resource2);
		}
		finally
		{
			reader2.close();
		}
		assertTrue("The two parsed TURF files are not equivalent.", resource1.equals(resource2));
	}

	/**Tests that the URF parser can parse a simple TURF file containing an anonymous resource.*/
	@Test
	public void testSimpleAnonymousResource() throws IOException, URISyntaxException
	{
		final URFTURFProcessor urfProcessor=new URFTURFProcessor();
		final URL turfURL=getClass().getResource("simple-anonymous-resource.turf");
		final Reader reader=new LineNumberReader(new InputStreamReader(turfURL.openStream(), Charsets.UTF_8_CHARSET));
		try
		{
			urfProcessor.process(reader, turfURL.toURI());
		}
		finally
		{
			reader.close();
		}
		Log.info(URF.toString(urfProcessor.getURF()));
	}
}
