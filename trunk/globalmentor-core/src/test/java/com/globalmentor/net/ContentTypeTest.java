/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net;

import com.globalmentor.test.AbstractTest;
import com.globalmentor.text.ArgumentSyntaxException;

import static com.globalmentor.collections.Sets.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 * Tests of {@link ContentType}.
 * @author Garret Wilson
 */
public class ContentTypeTest extends AbstractTest {

	/** Tests of {@link ContentType#create(CharSequence)}. */
	@Test
	public void testParseContentTypes() {
		//simple
		assertThat(ContentType.create("text/plain").getPrimaryType(), is("text"));
		assertThat(ContentType.create("text/plain").getSubType(), is("plain"));
		//one parameter
		assertThat(ContentType.create("text/plain; charset=us-ascii").getPrimaryType(), is("text"));
		assertThat(ContentType.create("text/plain; charset=us-ascii").getSubType(), is("plain"));
		assertEquals(immutableSetOf(new ContentType.Parameter("charset", "us-ascii")), ContentType.create("text/plain; charset=us-ascii").getParameters());
		//two parameters
		assertThat(ContentType.create("text/plain; charset=us-ascii; foo=bar").getPrimaryType(), is("text"));
		assertThat(ContentType.create("text/plain; charset=us-ascii; foo=bar").getSubType(), is("plain"));
		assertEquals(immutableSetOf(new ContentType.Parameter("charset", "us-ascii"), new ContentType.Parameter("foo", "bar")),
				ContentType.create("text/plain; charset=us-ascii; foo=bar").getParameters());
	}

	/** Tests equality, including parameter order and case insensitivity of content type names. */
	@Test
	public void testEquality() {
		//parsing versus construction
		assertEquals(ContentType.create("text", "plain"), ContentType.create("text/plain"));
		assertEquals(ContentType.create("text", "plain", new ContentType.Parameter("charset", "us-ascii"), new ContentType.Parameter("foo", "bar")),
				ContentType.create("text/plain; charset=us-ascii; foo=bar"));
		//parameter order
		assertThat(ContentType.create("text/plain; charset=us-ascii; foo=bar"), equalTo(ContentType.create("text/plain; foo=bar; charset=us-ascii")));
		//case insensitivity
		assertThat(ContentType.create("text/plain; charset=us-ascii"), equalTo(ContentType.create("TEXT/PLAIN; CHARSET=us-ascii")));
		assertThat(ContentType.create("text/plain; charset=us-ascii"), not(equalTo(ContentType.create("TEXT/PLAIN; CHARSET=US-ASCII"))));
		//optional parameter value quotes
		assertThat(ContentType.create("text/plain; charset=us-ascii"), equalTo(ContentType.create("text/plain; charset=\"us-ascii\"")));
	}

	@Test(expected = ArgumentSyntaxException.class)
	public void testMissingDelimiter() {
		ContentType.create("text");
	}

	@Test(expected = ArgumentSyntaxException.class)
	public void testMissingSubType() {
		ContentType.create("text/");
	}

	@Test(expected = ArgumentSyntaxException.class)
	public void testMissingParameters() {
		ContentType.create("text/plain;");
	}

	@Test(expected = ArgumentSyntaxException.class)
	public void testMissingParameterDelimiter() {
		ContentType.create("text/plain; charset");
	}

	@Test(expected = ArgumentSyntaxException.class)
	public void testMissingParameterValue() {
		ContentType.create("text/plain; charset=");
	}

	@Test(expected = ArgumentSyntaxException.class)
	public void testMissingSecondParameter() {
		ContentType.create("text/plain; charset=us-ascii;");
	}
}
