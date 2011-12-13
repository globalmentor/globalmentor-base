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

package com.globalmentor.collections;

import static com.globalmentor.java.Characters.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.PrintStream;
import java.util.*;

import org.junit.Test;

import com.globalmentor.collections.CharSequenceSuffixTree.Edge;
//import com.globalmentor.log.Log;
import com.globalmentor.model.Counter;
import com.globalmentor.test.AbstractTest;

/**
 * Tests for constructing a suffix tree based upon a character sequence.
 * 
 * @author Garret Wilson
 * 
 * @see <a href="http://marknelson.us/1996/08/01/suffix-trees/">Mark Nelson: Fast String Searching With Suffix Trees</a>
 */
public class CharSequenceSuffixTreeTest extends AbstractTest
{

	/** Interesting test string to use for creating suffix trees. */
	private final static Set<String> TEST_STRINGS = Sets.immutableSetOf("xabxa", "bananas", "bookkeeper", "mississippi", "dooodah");

	/**
	 * Tests the creation of suffix trees based upon supplied strings.
	 * @see #TEST_STRINGS
	 */
	@Test
	public void testCreate()
	{
		for(final String testString : TEST_STRINGS)
		{
			final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create(testString + NULL_CHAR); //TODO fix to automatically create explicit suffix trees
			//dumpEdges(suffixTree, System.out);
			//suffixTree.printTree(System.out);
			validate(suffixTree); //validate the tree
		}
	}

	/**
	 * Dumps the edges of a suffix tree to a given print stream.
	 * @param suffixTree The suffix tree the edges of which to print.
	 * @param printStream The destination to which the tree edges should be printed.
	 * @see <a href="http://marknelson.us/1996/08/01/suffix-trees/">Mark Nelson: Fast String Searching With Suffix Trees</a>
	 * @throws NullPointerException if the given suffix tree and/or print stream is <code>null</code>.
	 */
	public static void dumpEdges(final CharSequenceSuffixTree suffixTree, final PrintStream printStream)
	{
		printStream.println("  From     To Suffix  First   Last String");
		for(final Edge edge : suffixTree.getEdges())
		{
			printStream.format("%6d %6d %6d %6d %6d %s\n", edge.getParentNodeIndex(), edge.getChildNodeIndex(), suffixTree.getNode(edge.getChildNodeIndex())
					.getSuffixNodeIndex(), edge.getStart(), edge.getEnd(), edge.getSubSequence());
		}
	}

	/**
	 * Validates a suffix tree, throwing an appropriate assertion error if it is found to be invalid.
	 * @param suffixTree The suffix tree to validate.
	 * @throws NullPointerException if the given suffix tree is <code>null</code>.
	 * @throws AssertionError if the suffix tree is not valid.
	 */
	public static void validate(final CharSequenceSuffixTree suffixTree)
	{
		final StringBuilder stringBuilder = new StringBuilder(); //keep track of the suffix down each path
		final Map<Integer, Counter> nodeChildEdgeCountMap = new HashMap<Integer, Counter>(); //keep track of the count of each edge for each node
		final Map<Integer, Counter> suffixCountMap = new HashMap<Integer, Counter>(); //keep track of how many times we've seen each suffix
		suffixCountMap.put(0, new Counter(1)); //there is inherently a suffix of length zero in the suffix tree
		validate(suffixTree, 0, 0, stringBuilder, nodeChildEdgeCountMap, suffixCountMap);
		final CharSequence charSequence = suffixTree.getCharSequence();
		final int charSequenceLength = charSequence.length();

		//TODO fix for implicit trees
		for(int suffixLength = 0; suffixLength < charSequenceLength; ++suffixLength) //make sure we have one and only one suffix of each length
		{
			final long suffixCount = Counter.getCount(suffixCountMap, suffixLength); //get the number of suffixes of this length
			assertThat("Unexpeted suffix count for suffix " + charSequence.subSequence(0, suffixLength) + ".", suffixCount, equalTo(1L));
		}

		final int nodeCount = suffixTree.getNodeCount();
		int leafNodeCount = 0;
		int branchCount = 0;
		for(int nodeIndex = 0; nodeIndex < nodeCount; nodeIndex++) //check that we counted each node, and update the leaf/branch counts
		{
			assertTrue("Node " + nodeIndex + " never counted.", nodeChildEdgeCountMap.containsKey(nodeIndex));
			final long childBranchCount = Counter.getCount(nodeChildEdgeCountMap, nodeIndex);
			if(Counter.getCount(nodeChildEdgeCountMap, nodeIndex) == 0L) //update the branch/leaf count
			{
				leafNodeCount++;
			}
			else
			{
				branchCount += childBranchCount;
			}
		}

		//TODO fix for implicit trees
		assertThat("Leaf count not equal to number of characters in the sequence.", leafNodeCount, equalTo(charSequenceLength));

		assertThat("Branch count not equal to number of nodes minus one.", branchCount, equalTo(nodeCount - 1));
	}

	/**
	 * Validates a node and its descendants suffix tree, throwing an appropriate assertion error if it is found to be invalid.
	 * <p>
	 * The node child edge count map will always be updated with a counter, even for a count of zero.
	 * </p>
	 * @param suffixTree The suffix tree to validate.
	 * @param nodeIndex The index of the node to validate
	 * @param length The length of characters already validated.
	 * @param stringBuilder The string builder containing the characters that have up till now been validated; this will be at least length characters, and maybe
	 *          more.
	 * @param nodeChildEdgeCountMap The map containing the counts of child edges for each node; this is to always be updated, even for a count of zero (indicating
	 *          a leaf node).
	 * @param suffixCountMap The map of counts of suffixes of each length.
	 * @return <code>true</code> if it was determined that this node was a leaf node.
	 * @throws NullPointerException if the given suffix tree, string builder, node child edge count map, and/or suffix count map is <code>null</code>.
	 * @throws AssertionError if the suffix tree is not valid.
	 */
	protected static boolean validate(final CharSequenceSuffixTree suffixTree, final int nodeIndex, final int length, final StringBuilder stringBuilder,
			final Map<Integer, Counter> nodeChildEdgeCountMap, final Map<Integer, Counter> suffixCountMap)
	{
		int edgeCount = 0; //keep track of the edges from this node
		for(final Edge childEdge : suffixTree.getChildEdges(nodeIndex)) //look at all the child edges for this node
		{
			assertFalse("Node " + nodeIndex + " already counted.", nodeChildEdgeCountMap.containsKey(nodeIndex));
			edgeCount++; //update our local record of edges for the parent node
			stringBuilder.replace(length, stringBuilder.length(), childEdge.getSubSequence().toString()); //add the substring from this edge to our growing suffix (replacing any leftover text from previous edges)
			if(validate(suffixTree, childEdge.getChildNodeIndex(), stringBuilder.length(), stringBuilder, nodeChildEdgeCountMap, suffixCountMap)) //validate this edge; if the edge's child node is a leaf node
			{
				assertTrue("Leaf node " + childEdge.getChildNodeIndex() + " should have no children.",
						Counter.getCount(nodeChildEdgeCountMap, childEdge.getChildNodeIndex()) == 0);
			}
		}
		nodeChildEdgeCountMap.put(nodeIndex, new Counter(edgeCount)); //set the count of edges for this parent node
		if(edgeCount == 0) //if this is a leaf node (it has no child edges)
		{
			final CharSequence suffix = stringBuilder.subSequence(0, length); //get our collected suffix up to the parent node
			final CharSequence expectedSuffix = suffixTree.getCharSequence().subSequence(suffixTree.getCharSequence().length() - length,
					suffixTree.getCharSequence().length()); //get what we would have expected if we were looking directly at the underlying character sequence
			//Log.debug("Suffix:", suffix);
			assertThat("Bad suffix at position " + length + " for leaf node " + nodeIndex + ".", suffix, equalTo(expectedSuffix));
			Counter.incrementCounterMapCount(suffixCountMap, length); //show that we found a suffix of this length (at the end we should only have one of each)
			return true; //indicate that this is a leaf node
		}
		else
		{
			return false; //indicate that this is not a leaf node
		}
	}
}
