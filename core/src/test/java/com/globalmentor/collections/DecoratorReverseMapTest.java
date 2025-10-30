/*
 * Copyright © 1996-2025 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.collections;

import static org.hamcrest.MatcherAssert.*;
import static org.hamcrest.Matchers.*;

import java.util.*;

import org.junit.jupiter.api.*;

/**
 * Tests for {@link DecoratorReverseMap}.
 * @author Garret Wilson
 */
public class DecoratorReverseMapTest {

	/**
	 * Tests for {@link DecoratorReverseMap#put(Object, Object)}.
	 */
	@Test
	public void testPut() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		//Basic put operation
		assertThat("put returns null for new key", reverseMap.put("key1", "value1"), is(nullValue()));
		assertThat("forward mapping is stored", reverseMap.get("key1"), is("value1"));
		assertThat("reverse mapping is stored", reverseMap.getKey("value1"), is("key1"));

		//Updating existing key with different value
		assertThat("put returns old value when updating key", reverseMap.put("key1", "value2"), is("value1"));
		assertThat("forward mapping is updated", reverseMap.get("key1"), is("value2"));
		assertThat("new reverse mapping is stored", reverseMap.getKey("value2"), is("key1"));
		assertThat("old reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Duplicate value with different key (the critical bug fix)
		reverseMap.put("key1", "test");
		reverseMap.put("key2", "test"); //this should remove key1->test mapping
		assertThat("second key is mapped to value", reverseMap.get("key2"), is("test"));
		assertThat("reverse mapping points to second key", reverseMap.getKey("test"), is("key2"));
		assertThat("first key no longer has mapping", reverseMap.get("key1"), is(nullValue()));
		assertThat("map size is correct", reverseMap.size(), is(1));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#remove(Object)}.
	 */
	@Test
	public void testRemove() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");

		//Remove existing key
		assertThat("remove returns old value", reverseMap.remove("key1"), is("value1"));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Remove non-existent key
		assertThat("remove returns null for non-existent key", reverseMap.remove("key3"), is(nullValue()));

		//Verify other mapping is unaffected
		assertThat("other forward mapping remains", reverseMap.get("key2"), is("value2"));
		assertThat("other reverse mapping remains", reverseMap.getKey("value2"), is("key2"));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#removeValue(Object)}.
	 */
	@Test
	public void testRemoveValue() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");

		//Remove existing value
		assertThat("removeValue returns old key", reverseMap.removeValue("value1"), is("key1"));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Remove non-existent value
		assertThat("removeValue returns null for non-existent value", reverseMap.removeValue("value3"), is(nullValue()));

		//Verify other mapping is unaffected
		assertThat("other forward mapping remains", reverseMap.get("key2"), is("value2"));
		assertThat("other reverse mapping remains", reverseMap.getKey("value2"), is("key2"));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#clear()}.
	 */
	@Test
	public void testClear() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");

		reverseMap.clear();

		assertThat("map is empty", reverseMap.isEmpty(), is(true));
		assertThat("forward mapping is cleared", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is cleared", reverseMap.getKey("value1"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#putIfAbsent(Object, Object)}.
	 */
	@Test
	public void testPutIfAbsent() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		//Put when key is absent
		assertThat("putIfAbsent returns null for new key", reverseMap.putIfAbsent("key1", "value1"), is(nullValue()));
		assertThat("forward mapping is stored", reverseMap.get("key1"), is("value1"));
		assertThat("reverse mapping is stored", reverseMap.getKey("value1"), is("key1"));

		//Put when key is present
		assertThat("putIfAbsent returns existing value", reverseMap.putIfAbsent("key1", "value2"), is("value1"));
		assertThat("forward mapping is unchanged", reverseMap.get("key1"), is("value1"));
		assertThat("reverse mapping is unchanged", reverseMap.getKey("value1"), is("key1"));
		assertThat("new value has no reverse mapping", reverseMap.getKey("value2"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#remove(Object, Object)}.
	 */
	@Test
	public void testRemoveKeyValue() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");

		//Remove with matching key-value pair
		assertThat("remove returns true for matching pair", reverseMap.remove("key1", "value1"), is(true));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Remove with non-matching value
		assertThat("remove returns false for non-matching value", reverseMap.remove("key2", "wrongValue"), is(false));
		assertThat("forward mapping remains", reverseMap.get("key2"), is("value2"));
		assertThat("reverse mapping remains", reverseMap.getKey("value2"), is("key2"));

		//Remove with non-existent key
		assertThat("remove returns false for non-existent key", reverseMap.remove("key3", "value3"), is(false));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#replace(Object, Object, Object)}.
	 */
	@Test
	public void testReplaceWithOldValue() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");

		//Replace with matching old value
		assertThat("replace returns true for matching old value", reverseMap.replace("key1", "value1", "newValue1"), is(true));
		assertThat("forward mapping is updated", reverseMap.get("key1"), is("newValue1"));
		assertThat("new reverse mapping is stored", reverseMap.getKey("newValue1"), is("key1"));
		assertThat("old reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Replace with non-matching old value
		assertThat("replace returns false for non-matching old value", reverseMap.replace("key2", "wrongValue", "newValue2"), is(false));
		assertThat("forward mapping is unchanged", reverseMap.get("key2"), is("value2"));
		assertThat("reverse mapping is unchanged", reverseMap.getKey("value2"), is("key2"));

		//Replace with non-existent key
		assertThat("replace returns false for non-existent key", reverseMap.replace("key3", "value3", "newValue3"), is(false));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#replace(Object, Object)}.
	 */
	@Test
	public void testReplace() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Replace existing key
		assertThat("replace returns old value", reverseMap.replace("key1", "newValue1"), is("value1"));
		assertThat("forward mapping is updated", reverseMap.get("key1"), is("newValue1"));
		assertThat("new reverse mapping is stored", reverseMap.getKey("newValue1"), is("key1"));
		assertThat("old reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Replace non-existent key
		assertThat("replace returns null for non-existent key", reverseMap.replace("key2", "value2"), is(nullValue()));
		assertThat("no forward mapping is created", reverseMap.get("key2"), is(nullValue()));
		assertThat("no reverse mapping is created", reverseMap.getKey("value2"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#computeIfAbsent(Object, java.util.function.Function)}.
	 */
	@Test
	public void testComputeIfAbsent() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Compute when key is absent
		final String computedValue = reverseMap.computeIfAbsent("key2", key -> "computed_" + key);
		assertThat("computeIfAbsent returns computed value", computedValue, is("computed_key2"));
		assertThat("forward mapping is stored", reverseMap.get("key2"), is("computed_key2"));
		assertThat("reverse mapping is stored", reverseMap.getKey("computed_key2"), is("key2"));

		//Compute when key is present
		final String existingValue = reverseMap.computeIfAbsent("key1", key -> "ignored");
		assertThat("computeIfAbsent returns existing value", existingValue, is("value1"));
		assertThat("forward mapping is unchanged", reverseMap.get("key1"), is("value1"));

		//Compute returning null
		final String nullValue = reverseMap.computeIfAbsent("key3", key -> null);
		assertThat("computeIfAbsent returns null when function returns null", nullValue, is(nullValue()));
		assertThat("no forward mapping is created", reverseMap.get("key3"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#computeIfPresent(Object, java.util.function.BiFunction)}.
	 */
	@Test
	public void testComputeIfPresent() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Compute when key is present
		final String computedValue = reverseMap.computeIfPresent("key1", (key, value) -> value + "_updated");
		assertThat("computeIfPresent returns computed value", computedValue, is("value1_updated"));
		assertThat("forward mapping is updated", reverseMap.get("key1"), is("value1_updated"));
		assertThat("new reverse mapping is stored", reverseMap.getKey("value1_updated"), is("key1"));
		assertThat("old reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Compute when key is absent
		final String absentValue = reverseMap.computeIfPresent("key2", (key, value) -> "ignored");
		assertThat("computeIfPresent returns null for absent key", absentValue, is(nullValue()));
		assertThat("no forward mapping is created", reverseMap.get("key2"), is(nullValue()));

		//Compute returning null removes mapping
		final String nullValue = reverseMap.computeIfPresent("key1", (key, value) -> null);
		assertThat("computeIfPresent returns null when function returns null", nullValue, is(nullValue()));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1_updated"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#compute(Object, java.util.function.BiFunction)}.
	 */
	@Test
	public void testCompute() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Compute with existing key
		final String computedValue = reverseMap.compute("key1", (key, value) -> value + "_computed");
		assertThat("compute returns computed value", computedValue, is("value1_computed"));
		assertThat("forward mapping is updated", reverseMap.get("key1"), is("value1_computed"));
		assertThat("new reverse mapping is stored", reverseMap.getKey("value1_computed"), is("key1"));
		assertThat("old reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Compute with non-existent key
		final String newValue = reverseMap.compute("key2", (key, value) -> "new_value");
		assertThat("compute returns computed value for new key", newValue, is("new_value"));
		assertThat("forward mapping is created", reverseMap.get("key2"), is("new_value"));
		assertThat("reverse mapping is created", reverseMap.getKey("new_value"), is("key2"));

		//Compute returning null removes mapping
		final String nullValue = reverseMap.compute("key1", (key, value) -> null);
		assertThat("compute returns null when function returns null", nullValue, is(nullValue()));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1_computed"), is(nullValue()));

		//Compute returning null for non-existent key does nothing
		final String nullValue2 = reverseMap.compute("key3", (key, value) -> null);
		assertThat("compute returns null for non-existent key", nullValue2, is(nullValue()));
		assertThat("no forward mapping is created", reverseMap.get("key3"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#merge(Object, Object, java.util.function.BiFunction)}.
	 */
	@Test
	public void testMerge() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Merge with existing key
		final String mergedValue = reverseMap.merge("key1", "suffix", (oldValue, value) -> oldValue + "_" + value);
		assertThat("merge returns merged value", mergedValue, is("value1_suffix"));
		assertThat("forward mapping is updated", reverseMap.get("key1"), is("value1_suffix"));
		assertThat("new reverse mapping is stored", reverseMap.getKey("value1_suffix"), is("key1"));
		assertThat("old reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));

		//Merge with non-existent key
		final String newValue = reverseMap.merge("key2", "new_value", (oldValue, value) -> "ignored");
		assertThat("merge returns new value for absent key", newValue, is("new_value"));
		assertThat("forward mapping is created", reverseMap.get("key2"), is("new_value"));
		assertThat("reverse mapping is created", reverseMap.getKey("new_value"), is("key2"));

		//Merge returning null removes mapping
		final String nullValue = reverseMap.merge("key1", "suffix", (oldValue, value) -> null);
		assertThat("merge returns null when function returns null", nullValue, is(nullValue()));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1_suffix"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#replaceAll(java.util.function.BiFunction)}.
	 */
	@Test
	public void testReplaceAll() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");
		reverseMap.put("key3", "value3");

		//Replace all values
		reverseMap.replaceAll((key, value) -> value.toUpperCase());

		assertThat("forward mapping 1 is updated", reverseMap.get("key1"), is("VALUE1"));
		assertThat("forward mapping 2 is updated", reverseMap.get("key2"), is("VALUE2"));
		assertThat("forward mapping 3 is updated", reverseMap.get("key3"), is("VALUE3"));
		assertThat("reverse mapping 1 is updated", reverseMap.getKey("VALUE1"), is("key1"));
		assertThat("reverse mapping 2 is updated", reverseMap.getKey("VALUE2"), is("key2"));
		assertThat("reverse mapping 3 is updated", reverseMap.getKey("VALUE3"), is("key3"));
		assertThat("old reverse mapping 1 is removed", reverseMap.getKey("value1"), is(nullValue()));
		assertThat("old reverse mapping 2 is removed", reverseMap.getKey("value2"), is(nullValue()));
		assertThat("old reverse mapping 3 is removed", reverseMap.getKey("value3"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#replaceAll(java.util.function.BiFunction)} when the function produces duplicate values.
	 */
	@Test
	public void testReplaceAllWithDuplicateValues() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");
		reverseMap.put("key3", "value3");

		//Replace all values with the same value - last one should win
		reverseMap.replaceAll((key, value) -> "SAME");

		//Only the last key processed should remain (LinkedHashMap iteration order)
		assertThat("at least one key maps to SAME", reverseMap.containsValue("SAME"), is(true));
		assertThat("reverse mapping points to one key", reverseMap.getKey("SAME"), is(notNullValue()));
		assertThat("map size is 1 after consolidation", reverseMap.size(), is(1));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#replaceAll(java.util.function.BiFunction)} when the function returns null.
	 */
	@Test
	public void testReplaceAllWithNullReturn() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");
		reverseMap.put("key2", "value2");

		//Remove all mappings by returning null
		reverseMap.replaceAll((key, value) -> null);

		assertThat("map is empty", reverseMap.isEmpty(), is(true));
		assertThat("forward mapping 1 is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("forward mapping 2 is removed", reverseMap.get("key2"), is(nullValue()));
		assertThat("reverse mapping 1 is removed", reverseMap.getKey("value1"), is(nullValue()));
		assertThat("reverse mapping 2 is removed", reverseMap.getKey("value2"), is(nullValue()));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#compute(Object, java.util.function.BiFunction)} when the function returns null.
	 */
	@Test
	public void testComputeReturningNull() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Remove by computing null
		final String result = reverseMap.compute("key1", (key, value) -> null);

		assertThat("compute returns null", result, is(nullValue()));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));
		assertThat("map is empty", reverseMap.isEmpty(), is(true));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#computeIfPresent(Object, java.util.function.BiFunction)} when the function returns null.
	 */
	@Test
	public void testComputeIfPresentReturningNull() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Remove by computing null
		final String result = reverseMap.computeIfPresent("key1", (key, value) -> null);

		assertThat("computeIfPresent returns null", result, is(nullValue()));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));
		assertThat("map is empty", reverseMap.isEmpty(), is(true));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#merge(Object, Object, java.util.function.BiFunction)} when the function returns null.
	 */
	@Test
	public void testMergeReturningNull() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		//Remove by merging to null
		final String result = reverseMap.merge("key1", "suffix", (oldValue, value) -> null);

		assertThat("merge returns null", result, is(nullValue()));
		assertThat("forward mapping is removed", reverseMap.get("key1"), is(nullValue()));
		assertThat("reverse mapping is removed", reverseMap.getKey("value1"), is(nullValue()));
		assertThat("map is empty", reverseMap.isEmpty(), is(true));
	}

	/**
	 * Tests for {@link DecoratorReverseMap#containsValue(Object)}.
	 */
	@Test
	public void testContainsValue() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		reverseMap.put("key1", "value1");

		assertThat("contains existing value", reverseMap.containsValue("value1"), is(true));
		assertThat("does not contain non-existent value", reverseMap.containsValue("value2"), is(false));
	}

	/**
	 * Tests the one-to-one relationship enforcement with complex scenarios.
	 */
	@Test
	public void testOneToOneRelationshipEnforcement() {
		final ReverseMap<String, String> reverseMap = new DecoratorReverseMap<>(new HashMap<>(), new HashMap<>());

		//Scenario: Multiple keys trying to map to the same value
		reverseMap.put("foo", "test");
		assertThat("foo maps to test", reverseMap.get("foo"), is("test"));
		assertThat("test reverse maps to foo", reverseMap.getKey("test"), is("foo"));

		reverseMap.put("bar", "test"); //This should remove foo->test
		assertThat("bar maps to test", reverseMap.get("bar"), is("test"));
		assertThat("test now reverse maps to bar", reverseMap.getKey("test"), is("bar"));
		assertThat("foo no longer has a mapping", reverseMap.get("foo"), is(nullValue()));
		assertThat("map size is 1", reverseMap.size(), is(1));

		reverseMap.put("baz", "test"); //This should remove bar->test
		assertThat("baz maps to test", reverseMap.get("baz"), is("test"));
		assertThat("test now reverse maps to baz", reverseMap.getKey("test"), is("baz"));
		assertThat("bar no longer has a mapping", reverseMap.get("bar"), is(nullValue()));
		assertThat("map size is still 1", reverseMap.size(), is(1));
	}

}
