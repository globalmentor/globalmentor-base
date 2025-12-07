/*
 * Copyright © 2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.globalmentor.java;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.Arrays;

import org.junit.jupiter.api.*;

/**
 * Tests for {@link ByteSequence} and {@link AbstractByteArrayByteSequence}.
 * @author Garret Wilson
 */
public class ByteSequenceTest {

	//## Static factory methods

	/** Tests for {@link ByteSequence#empty()}. */
	@Test
	void testEmpty() {
		final ByteSequence empty = ByteSequence.empty();
		assertThat("empty length", empty.length(), is(0));
		assertThat("empty isEmpty", empty.isEmpty(), is(true));
		assertThat("empty toByteArray", empty.toByteArray(), is(new byte[0]));
		assertThat("empty returns same instance", ByteSequence.empty(), is(sameInstance(empty)));
	}

	/** Tests for {@link ByteSequence#copyOf(byte[])}. */
	@Test
	void testCopyOf() {
		final byte[] original = {0x01, 0x02, 0x03};
		final ByteSequence bs = ByteSequence.copyOf(original);
		assertThat("copyOf length", bs.length(), is(3));
		assertThat("copyOf toByteArray equals original", bs.toByteArray(), is(original));
		original[0] = 0x00; // modify original to prove defensive copy
		assertThat("copyOf is independent of original", bs.byteAt(0), is((byte)0x01));
	}

	/** Tests that {@link ByteSequence#copyOf(byte[])} returns the empty singleton for empty arrays. */
	@Test
	void testCopyOfEmptyReturnsEmptySingleton() {
		assertThat("copyOf empty returns empty singleton", ByteSequence.copyOf(new byte[0]), is(sameInstance(ByteSequence.empty())));
	}

	/** Tests for {@link Bytes#asByteSequence(byte[])}. */
	@Test
	void testAsByteSequence() {
		final byte[] original = {0x01, 0x02, 0x03};
		final ByteSequence bs = Bytes.asByteSequence(original);
		assertThat("asByteSequence length", bs.length(), is(3));
		assertThat("asByteSequence toByteArray equals original", bs.toByteArray(), is(original));
	}

	/** Tests that {@link Bytes#asByteSequence(byte[])} returns the empty singleton for empty arrays. */
	@Test
	void testAsByteSequenceEmptyReturnsEmptySingleton() {
		assertThat("asByteSequence empty returns empty singleton", Bytes.asByteSequence(new byte[0]), is(sameInstance(ByteSequence.empty())));
	}

	//## Core methods

	/** Tests for {@link ByteSequence#length()}. */
	@Test
	void testLength() {
		assertThat("length of empty", ByteSequence.empty().length(), is(0));
		assertThat("length of single byte", ByteSequence.copyOf(new byte[] {0x42}).length(), is(1));
		assertThat("length of multiple bytes", ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04, 0x05}).length(), is(5));
	}

	/** Tests for {@link ByteSequence#byteAt(int)}. */
	@Test
	void testByteAt() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x10, 0x20, 0x30});
		assertThat("byteAt(0)", bs.byteAt(0), is((byte)0x10));
		assertThat("byteAt(1)", bs.byteAt(1), is((byte)0x20));
		assertThat("byteAt(2)", bs.byteAt(2), is((byte)0x30));
	}

	/** Tests that {@link ByteSequence#byteAt(int)} throws for invalid indices. */
	@Test
	void testByteAtThrowsForInvalidIndex() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02});
		assertThrows(IndexOutOfBoundsException.class, () -> bs.byteAt(-1), "negative index");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.byteAt(2), "index equals length");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.byteAt(3), "index exceeds length");
		assertThrows(IndexOutOfBoundsException.class, () -> ByteSequence.empty().byteAt(0), "index on empty");
	}

	/** Tests for {@link ByteSequence#subSequence(int, int)}. */
	@Test
	void testSubSequence() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04, 0x05});
		assertThat("subSequence full range returns same instance", bs.subSequence(0, 5), is(sameInstance(bs)));
		assertThat("subSequence empty range returns empty singleton", bs.subSequence(2, 2), is(sameInstance(ByteSequence.empty())));
		final ByteSequence sub = bs.subSequence(1, 4);
		assertThat("subSequence length", sub.length(), is(3));
		assertThat("subSequence content", sub.toByteArray(), is(new byte[] {0x02, 0x03, 0x04}));
	}

	/** Tests that {@link ByteSequence#subSequence(int, int)} throws for invalid ranges. */
	@Test
	void testSubSequenceThrowsForInvalidRange() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		assertThrows(IndexOutOfBoundsException.class, () -> bs.subSequence(-1, 2), "negative start");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.subSequence(2, 1), "start > end");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.subSequence(0, 4), "end > length");
	}

	/** Tests for {@link ByteSequence#toByteArray()}. */
	@Test
	void testToByteArray() {
		final byte[] original = {0x0A, 0x0B, 0x0C};
		final ByteSequence bs = ByteSequence.copyOf(original);
		final byte[] result = bs.toByteArray();
		assertThat("toByteArray equals original", result, is(original));
		result[0] = 0x00; // modify result to prove it's a defensive copy
		assertThat("toByteArray returns defensive copy", bs.byteAt(0), is((byte)0x0A));
	}

	//## Default methods

	/** Tests for {@link ByteSequence#isEmpty()}. */
	@Test
	void testIsEmpty() {
		assertThat("empty is empty", ByteSequence.empty().isEmpty(), is(true));
		assertThat("non-empty is not empty", ByteSequence.copyOf(new byte[] {0x01}).isEmpty(), is(false));
	}

	/** Tests for {@link ByteSequence#bytes()}. */
	@Test
	void testBytes() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {(byte)0x00, (byte)0x7F, (byte)0x80, (byte)0xFF});
		final int[] expected = {0x00, 0x7F, 0x80, 0xFF}; // unsigned values
		assertThat("bytes stream as unsigned", bs.bytes().toArray(), is(expected));
	}

	/** Tests for {@link ByteSequence#getBytes(int, int, byte[], int)}. */
	@Test
	void testGetBytes() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04, 0x05});
		final byte[] dst = new byte[5];
		bs.getBytes(1, 4, dst, 1);
		assertThat("getBytes copies correctly", dst, is(new byte[] {0x00, 0x02, 0x03, 0x04, 0x00}));
	}

	/** Tests that {@link ByteSequence#getBytes(int, int, byte[], int)} throws for invalid parameters. */
	@Test
	void testGetBytesThrows() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		final byte[] dst = new byte[5];
		assertThrows(IndexOutOfBoundsException.class, () -> bs.getBytes(-1, 2, dst, 0), "negative srcBegin");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.getBytes(2, 1, dst, 0), "srcBegin > srcEnd");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.getBytes(0, 4, dst, 0), "srcEnd > length");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.getBytes(0, 3, dst, -1), "negative dstBegin");
		assertThrows(IndexOutOfBoundsException.class, () -> bs.getBytes(0, 3, dst, 4), "dst overflow");
	}

	//## Comparison methods

	/** Tests for {@link ByteSequence#startsWith(ByteSequence)}. */
	@Test
	void testStartsWithByteSequence() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("starts with empty", bs.startsWith(ByteSequence.empty()), is(true));
		assertThat("starts with prefix", bs.startsWith(ByteSequence.copyOf(new byte[] {0x01, 0x02})), is(true));
		assertThat("starts with self", bs.startsWith(bs), is(true));
		assertThat("does not start with different", bs.startsWith(ByteSequence.copyOf(new byte[] {0x02, 0x03})), is(false));
		assertThat("does not start with longer", bs.startsWith(ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04, 0x05})), is(false));
	}

	/** Tests for {@link ByteSequence#startsWith(byte[])}. */
	@Test
	void testStartsWithByteArray() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("starts with empty array", bs.startsWith(new byte[0]), is(true));
		assertThat("starts with prefix array", bs.startsWith(new byte[] {0x01, 0x02}), is(true));
		assertThat("does not start with different array", bs.startsWith(new byte[] {0x02, 0x03}), is(false));
	}

	/** Tests for {@link ByteSequence#endsWith(ByteSequence)}. */
	@Test
	void testEndsWithByteSequence() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("ends with empty", bs.endsWith(ByteSequence.empty()), is(true));
		assertThat("ends with suffix", bs.endsWith(ByteSequence.copyOf(new byte[] {0x03, 0x04})), is(true));
		assertThat("ends with self", bs.endsWith(bs), is(true));
		assertThat("does not end with different", bs.endsWith(ByteSequence.copyOf(new byte[] {0x02, 0x03})), is(false));
		assertThat("does not end with longer", bs.endsWith(ByteSequence.copyOf(new byte[] {0x00, 0x01, 0x02, 0x03, 0x04})), is(false));
	}

	/** Tests for {@link ByteSequence#endsWith(byte[])}. */
	@Test
	void testEndsWithByteArray() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("ends with empty array", bs.endsWith(new byte[0]), is(true));
		assertThat("ends with suffix array", bs.endsWith(new byte[] {0x03, 0x04}), is(true));
		assertThat("does not end with different array", bs.endsWith(new byte[] {0x02, 0x03}), is(false));
	}

	/** Tests for {@link ByteSequence#isPrefixOf(ByteSequence)}. */
	@Test
	void testIsPrefixOfByteSequence() {
		final ByteSequence prefix = ByteSequence.copyOf(new byte[] {0x01, 0x02});
		final ByteSequence full = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("prefix is prefix of full", prefix.isPrefixOf(full), is(true));
		assertThat("full is not prefix of prefix", full.isPrefixOf(prefix), is(false));
		assertThat("empty is prefix of anything", ByteSequence.empty().isPrefixOf(full), is(true));
		assertThat("sequence is prefix of itself", prefix.isPrefixOf(prefix), is(true));
	}

	/** Tests for {@link ByteSequence#isPrefixOf(byte[])}. */
	@Test
	void testIsPrefixOfByteArray() {
		final ByteSequence prefix = ByteSequence.copyOf(new byte[] {0x01, 0x02});
		assertThat("prefix is prefix of array", prefix.isPrefixOf(new byte[] {0x01, 0x02, 0x03}), is(true));
		assertThat("prefix is not prefix of shorter array", prefix.isPrefixOf(new byte[] {0x01}), is(false));
		assertThat("prefix is not prefix of different array", prefix.isPrefixOf(new byte[] {0x02, 0x03}), is(false));
	}

	/** Tests for {@link ByteSequence#isSuffixOf(ByteSequence)}. */
	@Test
	void testIsSuffixOfByteSequence() {
		final ByteSequence suffix = ByteSequence.copyOf(new byte[] {0x03, 0x04});
		final ByteSequence full = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03, 0x04});
		assertThat("suffix is suffix of full", suffix.isSuffixOf(full), is(true));
		assertThat("full is not suffix of suffix", full.isSuffixOf(suffix), is(false));
		assertThat("empty is suffix of anything", ByteSequence.empty().isSuffixOf(full), is(true));
		assertThat("sequence is suffix of itself", suffix.isSuffixOf(suffix), is(true));
	}

	/** Tests for {@link ByteSequence#isSuffixOf(byte[])}. */
	@Test
	void testIsSuffixOfByteArray() {
		final ByteSequence suffix = ByteSequence.copyOf(new byte[] {0x03, 0x04});
		assertThat("suffix is suffix of array", suffix.isSuffixOf(new byte[] {0x01, 0x02, 0x03, 0x04}), is(true));
		assertThat("suffix is not suffix of shorter array", suffix.isSuffixOf(new byte[] {0x04}), is(false));
		assertThat("suffix is not suffix of different array", suffix.isSuffixOf(new byte[] {0x01, 0x02}), is(false));
	}

	/** Tests for {@link ByteSequence#equalsBytes(byte[])}. */
	@Test
	void testEqualsBytes() {
		final ByteSequence bs = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		assertThat("equals same bytes", bs.equalsBytes(new byte[] {0x01, 0x02, 0x03}), is(true));
		assertThat("not equals different bytes", bs.equalsBytes(new byte[] {0x01, 0x02, 0x04}), is(false));
		assertThat("not equals different length", bs.equalsBytes(new byte[] {0x01, 0x02}), is(false));
		assertThat("empty equals empty array", ByteSequence.empty().equalsBytes(new byte[0]), is(true));
	}

	//## Static comparison

	/** Tests for {@link ByteSequence#compare(ByteSequence, ByteSequence)}. */
	@Test
	void testCompare() {
		final ByteSequence a = ByteSequence.copyOf(new byte[] {0x01, 0x02});
		final ByteSequence b = ByteSequence.copyOf(new byte[] {0x01, 0x03});
		final ByteSequence c = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		assertThat("equal sequences", ByteSequence.compare(a, a), is(0));
		assertThat("a < b", ByteSequence.compare(a, b), is(lessThan(0)));
		assertThat("b > a", ByteSequence.compare(b, a), is(greaterThan(0)));
		assertThat("a < c (shorter)", ByteSequence.compare(a, c), is(lessThan(0)));
		assertThat("c > a (longer)", ByteSequence.compare(c, a), is(greaterThan(0)));
	}

	/** Tests that {@link ByteSequence#compare(ByteSequence, ByteSequence)} treats bytes as unsigned. */
	@Test
	void testCompareUnsigned() {
		// 0xFF as unsigned is 255, which is greater than 0x01
		final ByteSequence low = ByteSequence.copyOf(new byte[] {0x01});
		final ByteSequence high = ByteSequence.copyOf(new byte[] {(byte)0xFF});
		assertThat("0xFF > 0x01 when unsigned", ByteSequence.compare(high, low), is(greaterThan(0)));
	}

	//## Equality and hash code

	/** Tests for {@link ByteSequence#equals(Object)}. */
	@SuppressWarnings("unlikely-arg-type")
	@Test
	void testEquals() {
		final ByteSequence bs1 = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		final ByteSequence bs2 = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x03});
		final ByteSequence bs3 = Bytes.asByteSequence(new byte[] {0x01, 0x02, 0x03});
		final ByteSequence different = ByteSequence.copyOf(new byte[] {0x01, 0x02, 0x04});
		assertThat("equals self", bs1.equals(bs1), is(true));
		assertThat("equals equivalent", bs1.equals(bs2), is(true));
		assertThat("equals different implementation", bs1.equals(bs3), is(true));
		assertThat("not equals different content", bs1.equals(different), is(false));
		assertThat("not equals null", bs1.equals(null), is(false));
		assertThat("not equals non-ByteSequence", bs1.equals("not a ByteSequence"), is(false));
		assertThat("empty equals empty", ByteSequence.empty().equals(ByteSequence.copyOf(new byte[0])), is(true));
	}

	/** Tests for {@link ByteSequence#hashCode()}. */
	@Test
	void testHashCode() {
		final byte[] data = {0x01, 0x02, 0x03};
		final ByteSequence bs1 = ByteSequence.copyOf(data);
		final ByteSequence bs2 = Bytes.asByteSequence(data.clone());
		assertThat("hashCode equals Arrays.hashCode", bs1.hashCode(), is(Arrays.hashCode(data)));
		assertThat("equal sequences have equal hashCode", bs1.hashCode(), is(bs2.hashCode()));
		assertThat("empty hashCode equals Arrays.hashCode of empty", ByteSequence.empty().hashCode(), is(Arrays.hashCode(new byte[0])));
	}

	/** Tests that equal byte sequences have equal hash codes (contract verification). */
	@Test
	void testEqualsHashCodeContract() {
		final ByteSequence bs1 = ByteSequence.copyOf(new byte[] {0x10, 0x20, 0x30});
		final ByteSequence bs2 = ByteSequence.copyOf(new byte[] {0x10, 0x20, 0x30});
		assertThat("equals implies same hashCode", bs1.equals(bs2), is(true));
		assertThat("hashCode consistency", bs1.hashCode(), is(bs2.hashCode()));
	}

}
