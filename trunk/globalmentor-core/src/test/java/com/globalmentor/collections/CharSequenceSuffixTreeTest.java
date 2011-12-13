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

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.*;

import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.globalmentor.collections.CharSequenceSuffixTree.Edge;
import com.globalmentor.java.StringBuilders;
import com.globalmentor.java.Strings;
import com.globalmentor.log.Log;
import com.globalmentor.model.Counter;
import com.globalmentor.test.AbstractTest;

/**
 * Tests for constructing a suffix tree based upon a character sequence.
 * 
 * @author Garret Wilson
 */
public class CharSequenceSuffixTreeTest extends AbstractTest
{

	@Test
	public void testStrings()
	{

		//		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create("banana$");
		//		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create("aba$");
		//		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create("bana$");
		//		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create("banana");
		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create("xabxa$");
		//		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create("bookkeeper");
		dumpEdges(suffixTree, System.out);
		suffixTree.printTree(System.out);
		validate(suffixTree);
	}

	protected static void dumpEdges(final CharSequenceSuffixTree suffixTree, final PrintStream printStream)
	{
		printStream.println("  From     To Suffix  First   Last String");
		for(final Edge edge : suffixTree.getEdges())
		{
			printStream.format("%6d %6d %6d %6d %6d %s\n", edge.getParentNodeIndex(), edge.getChildNodeIndex(), suffixTree.getNode(edge.getChildNodeIndex())
					.getSuffixNodeIndex(), edge.getStart(), edge.getEnd(), edge.getSubSequence());
		}
	}

	public void validate(final CharSequenceSuffixTree suffixTree)
	{
		final StringBuilder stringBuilder = new StringBuilder(); //keep track of the suffix down each path
		final Map<Integer, Counter> nodeChildEdgeCountMap = new HashMap<Integer, Counter>(); //keep track of the count of each edge for each node
		final Map<Integer, Counter> suffixCountMap = new HashMap<Integer, Counter>(); //keep track of how many times we've seen each suffix
		suffixCountMap.put(0, new Counter(1)); //there is inherently a suffix of length zero in the suffix tree
		walk_tree(suffixTree, 0, 0, stringBuilder, nodeChildEdgeCountMap, suffixCountMap);
		final CharSequence charSequence = suffixTree.getCharSequence();
		final int charSequenceLength = charSequence.length();
		for(int suffixLength = 0; suffixLength < charSequenceLength; ++suffixLength) //make sure we have one and only one suffix of each length
		{
			final long suffixCount = Counter.getCount(suffixCountMap, suffixLength);
			assertThat("Unexpeted suffix count for suffix " + charSequence.subSequence(0, suffixLength) + ".", suffixCount, equalTo(1L));
		}
		final int nodeCount = suffixTree.getNodeCount();
		int leafNodeCount = 0;
		int branchCount = 0;
		for(int nodeIndex = 0; nodeIndex < nodeCount; nodeIndex++) //check each node
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
		assertThat("Leaf count not equal to number of characters in the sequence.", leafNodeCount, equalTo(charSequenceLength));
		assertThat("Branch count not equal to number of nodes minus one.", branchCount, equalTo(nodeCount - 1));
	}

	/**
	 * 
	 * <p>
	 * The node child edge count map will always be updated with a counter, even for a count of zero.
	 * </p>
	 * 
	 * @param suffixTree
	 * @param nodeIndex
	 * @param length
	 * @param stringBuilder
	 * @param nodeChildEdgeCountMap
	 * @param suffixCountMap
	 * @return
	 */
	protected boolean walk_tree(final CharSequenceSuffixTree suffixTree, final int nodeIndex, final int length, final StringBuilder stringBuilder,
			final Map<Integer, Counter> nodeChildEdgeCountMap, final Map<Integer, Counter> suffixCountMap)
	{
		int edgeCount = 0; //keep track of the edges from this node
		for(final Edge childEdge : suffixTree.getChildEdges(nodeIndex)) //look at all the child edges for this node
		{
			//TODO del if works  	 assertTrue("Already determined node "+childEdge.getParentNodeIndex()+" to be a leaf node.", Counter.getCount(edgeCountMap, childEdge.getParentNodeIndex()) ==0);	//TODO comment
			assertFalse("Node " + nodeIndex + " already counted.", nodeChildEdgeCountMap.containsKey(nodeIndex));
			/*TODO fix
			           if ( BranchCount[ edge.start_node ] < 0 )
			               cerr << "Logic error on node "
			                    << edge.start_node
			                    << '\n';
			*/
			//TODO del if works  	 Counter.incrementCounterMapCount(edgeCountMap, childEdge.getParentNodeIndex());	//increment our count of edges for this parent node
			edgeCount++; //update our local record of edges for the parent node
			stringBuilder.replace(length, stringBuilder.length(), childEdge.getSubSequence().toString()); //TODO test
			//  	 stringBuilder.append(childEdge.getSubSequence());
			if(walk_tree(suffixTree, childEdge.getChildNodeIndex(), stringBuilder.length(), stringBuilder, nodeChildEdgeCountMap, suffixCountMap)) //validate this edge; if the edge's child node is a leaf node
			{
				assertTrue("Leave node " + childEdge.getChildNodeIndex() + " should have no children.",
						Counter.getCount(nodeChildEdgeCountMap, childEdge.getChildNodeIndex()) == 0);
				//TODO why?            	 Counter.decrementCounterMapCount(edgeCountMap, childEdge.getChildNodeIndex());	//decrement our count of edges for this child node
			}
		}

		nodeChildEdgeCountMap.put(nodeIndex, new Counter(edgeCount)); //set the count of edges for this parent node

		if(edgeCount == 0) //if this is a leaf node (it has no child edges)
		{
			final CharSequence suffix = stringBuilder.subSequence(0, length);
			final CharSequence expectedSuffix = suffixTree.getCharSequence().subSequence(suffixTree.getCharSequence().length() - length,
					suffixTree.getCharSequence().length());
			Log.debug("Suffix:", suffix);
			assertThat("Bad suffix at position " + length + " for leaf node " + nodeIndex + ".", suffix, equalTo(expectedSuffix));
			Counter.incrementCounterMapCount(suffixCountMap, length); //show that we found another suffix
			return true; //indicate that this is a leaf node
		}
		else
		{
			return false; //indicate that this is not a leaf node
		}
	}
}
