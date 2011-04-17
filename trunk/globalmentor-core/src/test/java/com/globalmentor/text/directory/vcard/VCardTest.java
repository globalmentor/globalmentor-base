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

import org.hamcrest.CoreMatchers;
import org.junit.Test;

import com.globalmentor.io.IO;
import com.globalmentor.itu.TelephoneNumber;
import com.globalmentor.java.Classes;
import com.globalmentor.urf.AbstractURFDateTime;
import com.globalmentor.urf.URFDateTime;

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
		assertThat(vcard.getAddress().getExtendedAddress(), is("Oak and Pine"));
		assertThat(vcard.getAddress().getStreetAddress(), is("123 Oak Street"));
		assertThat(vcard.getAddress().getLocality(), is("San Francisco"));
		assertThat(vcard.getAddress().getPostalCode(), is("94120"));
		assertThat(vcard.getAddress().getCountryName(), is("USA"));
		assertThat(vcard.getBirthday(), CoreMatchers.<AbstractURFDateTime> is(URFDateTime.valueOf("1970-01-02T00:00:00")));
		assertThat(vcard.getFormattedName().toString(), is("Ms. Jane Lívia Doe"));
		final Telephone homeTelephone = vcard.getTelephone(new TelephoneNumber("+14155551212"));
		assertNotNull(homeTelephone);
		//TODO fix for Nokia VCard files; switch to using enums	assertThat(homeTelephone.getTelephoneType() & Telephone.HOME_TELEPHONE_TYPE, is(Telephone.HOME_TELEPHONE_TYPE));

		/*
		TEL;PREF;HOME;VOICE;ENCODING=8BIT:+14155551212
		TEL;CELL;VOICE;ENCODING=8BIT:+19185551212
		TEL;VOICE;ENCODING=8BIT:+5105551212
		TEL;WORK;VOICE;ENCODING=8BIT:+552138232003
		EMAIL;CHARSET=UTF-8;ENCODING=8BIT:jane@example.com
		URL;CHARSET=UTF-8;ENCODING=8BIT:http://www.example.com/
		NOTE;ENCODING=BASE64:VGhpcyBpcyBqdXN0IGEgdGVzdC4KSXNzbyDDqSBzw7MgdW0gZXhlbX
		 Bsby4=
		 */
	}

}
