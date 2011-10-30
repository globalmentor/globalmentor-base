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

package com.globalmentor.temp;

import static junit.framework.Assert.*;

import java.util.*;

import org.junit.Test;

/**
 * This is a test of various approaches to a union of lists, based upon discussions between Garret Wilson and Bruno from Google.
 * 
 * <p>
 * The discussion was based upon a scenario where various people (e.g. in a social network) each have various friends, and it is desired to determine which
 * friends all the people have in common.
 * </p>
 * 
 * <p>
 * Garret brainstormed by suggesting counting all the occurrences of all friends and using only those friends that have an ending count equal to the number of
 * friends. Bruno pointed out that this would have large memory implications.
 * </p>
 * 
 * <p>
 * The ending solution favored by Bruno was one in which a new set of the current union was carried forward each time.
 * </p>
 * 
 * <p>
 * This class presents a third alternative, a hybrid of the two approaches, which maintains low memory overhead yet improves the execution time by over 30%.
 * </p>
 * 
 * @author Garret Wilson
 * 
 */
public class UnionTest
{

	/**
	 * Tests three approaches to getting the union of multiple lists.
	 * <p>
	 * The test performs several iterations, generating new test data each time. The test data generation is rather inefficient: the friends generated are
	 * "oversampled" (that is, random friends are generated more times than there are people) in order to make it more likely that there exist friends in common
	 * with all people (as the random number generator will create duplicates). The numbers are therefore not arbitrary to create what is believed to be a
	 * reasonable/typical number of friends in common. A better test data generator would "force" the number of common friends to some predetermined number, but
	 * this would require a more complicated test generation algorithm.
	 * </p>
	 */
	@Test
	public void testUnion()
	{
		final int iterationCount = 10; //the number of tests to perform
		final int personCount = 1000; //the number of people
		//the number of friends each person has
		//we'll use a large number here just to increase likelihood of common friends, as we use a random number generator which may wind up choosing the same friend multiple times
		//try changing this to a higher number to increase the number of friends in common
		//at 10000, for example, the average number of friends in common is 96%, and the hybrid approach shows an improvement of around 50% over the carry-forward approach
		final int maxFriendCount = 6000; //the number of friends each person has; we'll use a large number here just to increase likelihood of common friends
		final Random random = new Random();
		long byCountTime = 0, byCarryTime = 0, byHybridTime = 0;
		for(int iteration = 0; iteration < iterationCount; ++iteration)
		{
			final List<List<Integer>> lists = new ArrayList<List<Integer>>();
			for(int i = 0; i < personCount; ++i)
			{
				final Set<Integer> friends = new HashSet<Integer>();
				for(int j = 0; j < maxFriendCount; ++j) //add random friends
				{
					friends.add(random.nextInt(personCount)); //use a set to prevent duplicates
				}
				lists.add(new ArrayList<Integer>(friends)); //add a list of friends
			}

			long time = System.currentTimeMillis();
			final Set<Integer> commonFriendsByCount = unionByCount(lists);
			byCountTime += System.currentTimeMillis() - time;
			time = System.currentTimeMillis();
			final Set<Integer> commonFriendsByCarry = unionByCarry(lists);
			byCarryTime += System.currentTimeMillis() - time;
			time = System.currentTimeMillis();
			final Set<Integer> commonFriendsByHybrid = unionByHybrid(lists);
			byHybridTime += System.currentTimeMillis() - time;
			assertEquals(commonFriendsByCount, commonFriendsByCarry); //make sure we get the same values by each method
			assertEquals(commonFriendsByCount, commonFriendsByHybrid);
			System.out.println("Iteration " + iteration + " common friends: " + commonFriendsByCount.size() + " ("
					+ Math.round(commonFriendsByCount.size() / (double)personCount * 100) + "%)");
		}
		System.out.println("By count time: " + byCountTime);
		System.out.println("By carry time: " + byCarryTime);
		System.out.println("By hybrid time: " + byHybridTime);
	}

	/**
	 * Creates a union of the lists by counting the occurrence of each item and returning only those items for which the count is equal to the number of lists.
	 * <p>
	 * If fewer than one list is given, an empty set will be returned.
	 * </p>
	 * @param lists The lists for which a set union should be created.
	 * @return A set containing the union of the lists.
	 */
	public <T> Set<T> unionByCount(final List<List<T>> lists)
	{
		final int listCount = lists.size();
		if(listCount < 2)
		{
			return Collections.emptySet();
		}
		final Map<T, Count> unionMap = new HashMap<T, Count>(); //create a single map of the counts of all items
		for(final List<T> list : lists) //go through all lists
		{
			for(final T item : list) //for each item in each list
			{
				final Count count = unionMap.get(item);
				if(count != null) //if we've seen the item before, increment its count
				{
					count.increment();
				}
				else
				//if the item doesn't yet exist in the map, put it there with an initial count of 1
				{
					unionMap.put(item, new Count(1));

				}
			}
		}
		//the union is all entries with a count equal to the number of lists
		final Set<T> union = new HashSet<T>();
		for(final Map.Entry<T, Count> unionMapEntry : unionMap.entrySet())
		{
			if(unionMapEntry.getValue().getValue() == listCount)
			{
				union.add(unionMapEntry.getKey());
			}
		}
		return union;
	}

	/**
	 * Creates a union of the lists by keeping track of the current union, and for each union creating a new union between the current list and the last union,
	 * and carrying this union forward.
	 * <p>
	 * If fewer than one list is given, an empty set will be returned.
	 * </p>
	 * @param lists The lists for which a set union should be created.
	 * @return A set containing the union of the lists.
	 */
	public <T> Set<T> unionByCarry(final List<List<T>> lists)
	{
		final int listCount = lists.size();
		if(listCount < 2)
		{
			return Collections.emptySet();
		}
		Set<T> union = new HashSet<T>(lists.get(0)); //hash the first list
		for(int i = 1; i < listCount; ++i) //for all the other lists
		{
			final Set<T> newUnion = new HashSet<T>();
			for(final T item : lists.get(i)) //find the union between the current union and the other items
			{
				if(union.contains(item))
				{
					newUnion.add(item);
				}
			}
			union = newUnion; //carry our new union forward
		}
		return union; //return the union we end up with
	}

	/**
	 * Creates a union of the lists in a hybrid of the previous methods by:
	 * <ol>
	 * <li>Creating a union of the first two sets (greatly reducing the memory usage.</li>
	 * <li>Incrementing the count of each item in the next lists.</li>
	 * <li>Automatically removing items as found if they are obviously not in the current union.</li>
	 * </ol>
	 * @param lists The lists for which a set union should be created.
	 * @return A set containing the union of the lists.
	 */
	public <T> Set<T> unionByHybrid(final List<List<T>> lists)
	{
		final int listCount = lists.size();
		if(listCount < 2)
		{
			return Collections.emptySet();
		}
		final Map<T, Count> unionMap = new HashMap<T, Count>();
		//perform a union with the first two lists
		{ //create a temporary block; we only need the initial set for the first union---then we can throw it away
			final Set<T> initialSet = new HashSet<T>(lists.get(0));
			for(final T item : lists.get(1))
			{
				if(initialSet.contains(item))
				{
					unionMap.put(item, new Count(2));
				}
			}
		}
		if(listCount == 2) //shortcut: if there are just two lists, we already know the union
		{
			return new HashSet<T>(unionMap.keySet()); //no need to count---if it's in the map at this point, it has to be in exactly both lists
		}
		//now check for items in common with the other lists
		for(int i = 2; i < listCount; ++i) //look at the other lists
		{
			for(final T item : lists.get(i)) //for each item in each other list
			{
				final Count count = unionMap.get(item); //see if we've seen the item before
				if(count != null) //if we've already seen the item before
				{
					final int currentCountValue = count.getValue();
					if(currentCountValue < i) //if the item isn't in all the other lists
					{
						unionMap.remove(item); //we can remove it and not consider it further
					}
					else
					//if the item is in all the other lists
					{
						count.increment(); //increment its count; we'll see if it winds up being in all lists
					}
				}
				//if we haven't seen the item before, it definitely isn't in the union, so don't even consider it
			}
			//we could improve the algorithm here, for the uncommon cases in which, say, the first two lists have a great number of
			//items in common but the others don't, by keeping a count of who many items had their count incremented; if this
			//number is significantly smaller than the current size of the map (e.g. 50% or less), then we could take time to
			//compact the map (i.e. throw out non-union items) on a one-time basis without severely impacting overall performance
		}
		//the union is all entries with a count equal to the number of lists,
		//but in this hybrid approach, the list will be much smaller as we started out with a smaller union
		//and we threw out items as we discovered in a short-circuit manner that they definitely
		//would not be in the final union
		final Set<T> union = new HashSet<T>();
		for(final Map.Entry<T, Count> unionMapEntry : unionMap.entrySet())
		{
			if(unionMapEntry.getValue().getValue() == listCount)
			{
				union.add(unionMapEntry.getKey());
			}
		}
		return union;
	}

	/**
	 * A class for efficiently maintaining and incrementing a count.
	 * 
	 * @author Garret Wilson
	 */
	private static class Count
	{
		private int value;

		/**
		 * Value constructor.
		 * @param value The initializing value.
		 */
		public Count(final int value)
		{
			this.value = value;
		}

		/** @return The current value. */
		public int getValue()
		{
			return value;
		}

		/**
		 * Increments the current value.
		 * @return The new current value.
		 */
		public int increment()
		{
			return ++value;
		}
	}

}
