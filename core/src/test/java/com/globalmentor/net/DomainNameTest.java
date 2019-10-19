/*
 * Copyright © 2019 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.junit.jupiter.api.*;

/**
 * Tests of {@link DomainName}.
 * @author Garret Wilson
 */
public final class DomainNameTest {

	/** @see DomainName#of(String) */
	@Test
	public void testStaticFactorymethod() {
		assertThat(DomainName.of("").toString(), is(""));
		assertThat(DomainName.of(".").toString(), is("."));
		assertThat(DomainName.of("com").toString(), is("com"));
		assertThat(DomainName.of("com.").toString(), is("com."));
		assertThat(DomainName.of("example.com").toString(), is("example.com"));
		assertThat(DomainName.of("example.com.").toString(), is("example.com."));
		assertThat(DomainName.of("www.example.com").toString(), is("www.example.com"));
		assertThat(DomainName.of("www.example.com.").toString(), is("www.example.com."));
		assertThat(DomainName.of("www").toString(), is("www"));
		assertThat(DomainName.of("www.").toString(), is("www."));
		assertThat(DomainName.of("foo.bar").toString(), is("foo.bar"));
		assertThat(DomainName.of("foo.bar.").toString(), is("foo.bar."));
		assertThrows(IllegalArgumentException.class, () -> DomainName.of(".."));
		assertThrows(IllegalArgumentException.class, () -> DomainName.of(".foo"));
		assertThrows(IllegalArgumentException.class, () -> DomainName.of(".foo."));
		assertThrows(IllegalArgumentException.class, () -> DomainName.of("foo..bar"));
	}

	/** @see DomainName#isAbsolute() */
	@Test
	public void testIsAbsolute() {
		assertThat(DomainName.of("").isAbsolute(), is(false));
		assertThat(DomainName.of(".").isAbsolute(), is(true));
		assertThat(DomainName.of("com").isAbsolute(), is(false));
		assertThat(DomainName.of("com.").isAbsolute(), is(true));
		assertThat(DomainName.of("example.com").isAbsolute(), is(false));
		assertThat(DomainName.of("example.com.").isAbsolute(), is(true));
		assertThat(DomainName.of("www.example.com").isAbsolute(), is(false));
		assertThat(DomainName.of("www.example.com.").isAbsolute(), is(true));
		assertThat(DomainName.of("www").isAbsolute(), is(false));
		assertThat(DomainName.of("www.").isAbsolute(), is(true));
		assertThat(DomainName.of("foo.bar").isAbsolute(), is(false));
		assertThat(DomainName.of("foo.bar.").isAbsolute(), is(true));
	}

	/** @see DomainName#relativize(DomainName) */
	@Test
	public void testRelativize() {
		assertThat(DomainName.of("").relativize(DomainName.of("")), is(DomainName.of("")));
		assertThat(DomainName.of("").relativize(DomainName.of(".")), is(DomainName.of(".")));
		assertThat(DomainName.of(".").relativize(DomainName.of(".")), is(DomainName.of("")));
		assertThat(DomainName.of(".").relativize(DomainName.of("")), is(DomainName.of("")));
		assertThat(DomainName.of("").relativize(DomainName.of("com")), is(DomainName.of("com")));
		assertThat(DomainName.of("").relativize(DomainName.of("com.")), is(DomainName.of("com.")));
		assertThat(DomainName.of("").relativize(DomainName.of("example.com")), is(DomainName.of("example.com")));
		assertThat(DomainName.of("").relativize(DomainName.of("example.com.")), is(DomainName.of("example.com.")));
		assertThat(DomainName.of(".").relativize(DomainName.of("example.com")), is(DomainName.of("example.com")));
		assertThat(DomainName.of(".").relativize(DomainName.of("example.com.")), is(DomainName.of("example.com")));
		assertThat(DomainName.of(".").relativize(DomainName.of("www.example.com")), is(DomainName.of("www.example.com")));
		assertThat(DomainName.of(".").relativize(DomainName.of("www.example.com.")), is(DomainName.of("www.example.com")));
		assertThat(DomainName.of("example.com").relativize(DomainName.of("")), is(DomainName.of("")));
		assertThat(DomainName.of("example.com.").relativize(DomainName.of("")), is(DomainName.of("")));
		assertThat(DomainName.of("example.com").relativize(DomainName.of("www.example.com")), is(DomainName.of("www")));
		assertThat(DomainName.of("example.com.").relativize(DomainName.of("www.example.com.")), is(DomainName.of("www")));
		assertThat(DomainName.of("www.example.com").relativize(DomainName.of("www.example.com")), is(DomainName.of("")));
		assertThat(DomainName.of("www.example.com.").relativize(DomainName.of("www.example.com.")), is(DomainName.of("")));
		assertThat(DomainName.of("www.example.com").relativize(DomainName.of("example.org")), is(DomainName.of("example.org")));
		assertThat(DomainName.of("www.example.com.").relativize(DomainName.of("example.org.")), is(DomainName.of("example.org.")));
		assertThat(DomainName.of("www.example.com").relativize(DomainName.of("www.example.org")), is(DomainName.of("www.example.org")));
		assertThat(DomainName.of("www.example.com.").relativize(DomainName.of("www.example.org.")), is(DomainName.of("www.example.org.")));
	}

	/** @see DomainName#resolve(DomainName) */
	@Test
	public void testResolve() {
		assertThat(DomainName.of("example.com").resolve(DomainName.of("")), is(DomainName.of("example.com")));
		assertThat(DomainName.of("example.com").resolve(DomainName.of(".")), is(DomainName.ROOT));
		assertThat(DomainName.of("example.com").resolve(DomainName.of("www")), is(DomainName.of("www.example.com")));
		assertThat(DomainName.of("").resolve(DomainName.of("www")), is(DomainName.of("www")));
		assertThat(DomainName.of("").resolve(DomainName.of("")), is(DomainName.EMPTY));
		assertThat(DomainName.of(".").resolve(DomainName.of("")), is(DomainName.ROOT));
		assertThat(DomainName.of("").resolve(DomainName.of(".")), is(DomainName.ROOT));
		assertThat(DomainName.of(".").resolve(DomainName.of(".")), is(DomainName.ROOT));
		assertThat(DomainName.of(".").resolve(DomainName.of("www.example.org")), is(DomainName.of("www.example.org.")));
		assertThat(DomainName.of(".").resolve(DomainName.of("www.example.org.")), is(DomainName.of("www.example.org.")));
		assertThat(DomainName.of("example.com").resolve(DomainName.of("www.example.org")), is(DomainName.of("www.example.org.example.com")));
		assertThat(DomainName.of("example.com").resolve(DomainName.of("www.example.org.")), is(DomainName.of("www.example.org.")));
		assertThat(DomainName.of("example.com.").resolve(DomainName.of("")), is(DomainName.of("example.com.")));
		assertThat(DomainName.of("example.com.").resolve(DomainName.of(".")), is(DomainName.ROOT));
		assertThat(DomainName.of("example.com.").resolve(DomainName.of("www")), is(DomainName.of("www.example.com.")));
		assertThat(DomainName.of("example.com.").resolve(DomainName.of("www.example.org")), is(DomainName.of("www.example.org.example.com.")));
		assertThat(DomainName.of("example.com.").resolve(DomainName.of("www.example.org.")), is(DomainName.of("www.example.org.")));
	}

	/**
	 * Tests that round-trip conversions between {@link DomainName#relativize(DomainName)} and {@link DomainName#resolve(DomainName)} results in the original
	 * domain for those domains that can be relativized. That is, domain name B relativized against domain name A and then resolved back against domain name A
	 * results in domain B, but only if domain B can be relativized against domain A.
	 */
	@Test
	public void testRelativeRoundTrip() {
		final List<String> relativeDomainNames = Arrays.asList("", "com", "example.com", "example.org", "www.example.com", "mail.example.com",
				"foo.bar.example.com", "globalmentor.com", "www.globalmentor.com");
		final List<DomainName> domainNames = new ArrayList<>(relativeDomainNames.size() * 2);
		for(final String relativeDomainNameString : relativeDomainNames) {
			domainNames.add(DomainName.of(relativeDomainNameString));
			domainNames.add(DomainName.of(relativeDomainNameString + DomainName.ROOT.toString())); //use both a relative and absolute form
		}
		for(final DomainName domainName1 : domainNames) {
			for(final DomainName domainName2 : domainNames) {
				final DomainName relativeDomainName = domainName1.relativize(domainName2);
				if(!relativeDomainName.equals(domainName2)) { //if the name was relativized
					assertThat(String.format("`%s` relativize+resolve `%s` round trip", domainName1, domainName2), domainName1.resolve(relativeDomainName),
							is(domainName2));
				}
			}
		}
	}

}
