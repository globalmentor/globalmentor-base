/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory.vcard;

import java.io.IOException;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.junit.Test;

import com.globalmentor.io.IO;
import com.globalmentor.java.Classes;

/**
 * Tests for correctly reading and writing VCard data.
 * 
 * @author Garret Wilson
 */
public class VCardTest
{

	@Test
	public void testReadNokiaC301JaneDoe() throws IOException
	{
		final IO<VCard> vcardIO = new VCardIO();
		final VCard vcard = Classes.readResource(getClass(), "nokia-c3-01-janedoe.vcf", vcardIO);
		assertThat(vcard.getName().getGivenName(), is("Jane"));
		assertThat(vcard.getName().getFamilyName(), is("Doe"));
	}

}
