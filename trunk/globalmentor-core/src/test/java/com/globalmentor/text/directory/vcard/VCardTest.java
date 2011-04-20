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

import java.io.*;
import java.io.IOException;
import java.net.URI;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import org.hamcrest.CoreMatchers;
import org.junit.Test;

import com.globalmentor.io.IO;
import com.globalmentor.itu.TelephoneNumber;
import com.globalmentor.java.Classes;
import com.globalmentor.model.LocaledText;
import com.globalmentor.text.directory.DirectorySerializer;
import com.globalmentor.urf.AbstractURFDateTime;
import com.globalmentor.urf.URFDateTime;

/**
 * Tests for correctly reading and writing VCard data.
 * 
 * @author Garret Wilson
 */
public class VCardTest
{

	/**
	 * Performs tests on a VCard to ensure that it has the appropriate properties of Jane Doe.
	 * @param vcard The VCard to test.
	 */
	public void testJaneDoe(final VCard vcard)
	{
		assertThat(vcard.getName().getGivenName(), is("Jane"));
		assertThat(vcard.getName().getFamilyName(), is("Doe"));
		assertThat(vcard.getAddress().getExtendedAddress(), is("Oak and Pine"));
		assertThat(vcard.getAddress().getStreetAddress(), is("123 Oak Street\nDowntown"));
		assertThat(vcard.getAddress().getLocality(), is("San Francisco"));
		assertThat(vcard.getAddress().getPostalCode(), is("94120"));
		assertThat(vcard.getAddress().getCountryName(), is("USA"));
		assertThat(vcard.getBirthday(), CoreMatchers.<AbstractURFDateTime> is(URFDateTime.valueOf("1970-01-02T00:00:00")));
		assertThat(vcard.getFormattedName().toString(), is("Ms. Jane Lívia Doe"));
		final Telephone homeTelephone = vcard.getTelephone(new TelephoneNumber("+14155551212"));
		assertNotNull(homeTelephone);
		assertThat(homeTelephone.getTypes().size(), is(3));
		assertTrue(homeTelephone.getTypes().contains(Telephone.Type.PREF));
		assertTrue(homeTelephone.getTypes().contains(Telephone.Type.HOME));
		assertTrue(homeTelephone.getTypes().contains(Telephone.Type.VOICE));
		final Telephone cellTelephone = vcard.getTelephone(new TelephoneNumber("+19185551212"));
		assertNotNull(cellTelephone);
		assertThat(cellTelephone.getTypes().size(), is(2));
		assertTrue(cellTelephone.getTypes().contains(Telephone.Type.CELL));
		assertTrue(cellTelephone.getTypes().contains(Telephone.Type.VOICE));
		final Telephone telephone = vcard.getTelephone(new TelephoneNumber("+15105551212"));
		assertNotNull(telephone);
		assertThat(telephone.getTypes().size(), is(1));
		assertTrue(telephone.getTypes().contains(Telephone.Type.VOICE));
		final Telephone workTelephone = vcard.getTelephone(new TelephoneNumber("+552138232003"));
		assertNotNull(workTelephone);
		assertThat(workTelephone.getTypes().size(), is(2));
		assertTrue(workTelephone.getTypes().contains(Telephone.Type.WORK));
		assertTrue(workTelephone.getTypes().contains(Telephone.Type.VOICE));
		assertThat(vcard.getEmail().getAddress(), is(new Email("jane@example.com").getAddress()));
		assertThat(vcard.getURL(), is(URI.create("http://www.example.com/")));
		assertThat(vcard.getNotes().size(), is(2));
		assertTrue(vcard.getNotes().contains(new LocaledText("This is just a test.\nIsso é só um exemplo.")));
		assertTrue(vcard.getNotes().contains(new LocaledText("This is another note.")));
	}

	@Test
	public void testReadNokiaC301JaneDoe() throws IOException
	{
		final IO<VCard> vcardIO = new VCardIO();
		final VCard vcard = Classes.readResource(getClass(), "nokia-c3-01-janedoe.vcf", vcardIO);
		testJaneDoe(vcard);
	}

	@Test
	public void testWriteJaneDoe() throws IOException
	{
		final IO<VCard> vcardIO = new VCardIO();
		//read the card from resources
		final VCard inputVCard = Classes.readResource(getClass(), "nokia-c3-01-janedoe.vcf", vcardIO);
		final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		//write the card to a byte array
		vcardIO.write(outputStream, null, inputVCard);
		//System.out.print(new String(outputStream.toByteArray(), Charsets.UTF_8_CHARSET));		
		final ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
		//read the card back in
		final VCard outputVCard = vcardIO.read(inputStream, null);
		//test to make sure it's what we started with
		testJaneDoe(outputVCard);
	}

	/** Tests the ability to combine NOTEs when serializing, for consumers such as GMail that don't allow multiple notes. */
	@Test
	public void testWriteJaneDoeCombineNotes() throws IOException
	{
		final VCardIO vcardIO = new VCardIO();
		vcardIO.setSerializationSingleValueNames(VCard.NOTE_TYPE); //combine notes
		//read the card from resources
		final VCard inputVCard = Classes.readResource(getClass(), "nokia-c3-01-janedoe.vcf", vcardIO);
		final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		//write the card to a byte array
		vcardIO.write(outputStream, null, inputVCard);
		//System.out.print(new String(outputStream.toByteArray(), Charsets.UTF_8_CHARSET));
		final ByteArrayInputStream inputStream = new ByteArrayInputStream(outputStream.toByteArray());
		//read the card back in
		final VCard outputVCard = vcardIO.read(inputStream, null);
		assertTrue(outputVCard.getNotes().contains(
				new LocaledText("This is just a test.\nIsso é só um exemplo." + DirectorySerializer.CONTENT_LINE_TEXT_COMBINE_STRING + "This is another note.")));
	}

}
