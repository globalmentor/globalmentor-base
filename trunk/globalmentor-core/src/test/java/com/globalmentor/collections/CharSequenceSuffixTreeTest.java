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

import org.junit.Test;

import com.globalmentor.java.StringBuilders;
import com.globalmentor.java.Strings;
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
	}

	protected static void dumpEdges(final CharSequenceSuffixTree suffixTree, final PrintStream printStream)
	{
		printStream.println("  From     To Suffix  First   Last String");
		for(final CharSequenceSuffixTree.Edge edge : suffixTree.getEdges())
		{
			printStream.format("%6d %6d %6d %6d %6d %s\n", edge.getParentNodeIndex(), edge.getChildNodeIndex(), suffixTree.getNode(edge.getChildNodeIndex())
					.getSuffixNodeIndex(), edge.getStart(), edge.getEnd(), edge.getSubSequence());
		}
	}


}
