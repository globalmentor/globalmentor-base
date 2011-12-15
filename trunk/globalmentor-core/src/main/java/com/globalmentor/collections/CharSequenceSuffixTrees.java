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

import static com.globalmentor.collections.SuffixTrees.*;

import com.globalmentor.collections.CharSequenceSuffixTree.*;
import com.globalmentor.model.ObjectHolder;

/**
 * Utilities for working with suffix trees of sequences of characters.
 * 
 * @author Garret Wilson
 */
public class CharSequenceSuffixTrees
{

	/**
	 * Determines the longest subsequence that is repeated in the given subsequence. TODO explain algorithm
	 * @param charSequence The character sequence to check.
	 * @return The longest repeated subsequence in the given character sequence, or <code>null</code> if no subsequence is repeated.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	public static CharSequence getLongestRepeatedSubsequence(final CharSequence charSequence)
	{
		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create(charSequence); //create a suffix tree
		final ObjectHolder<String> result = new ObjectHolder<String>(); //create an object to hold the resulting string
		visit(suffixTree, new AbstractCharSequenceVisitor()
		{
			int maxLength = 0; //keep track of the longest length

			@Override
			public boolean visit(final SuffixTree suffixTree, final CharSequenceNode node, final CharSequenceEdge parentEdge, final CharSequence charSequence)
			{
				if(!node.isLeaf()) //ignore leaf nodes---they aren't repeated sequences
				{
					if(charSequence.length() > maxLength) //if this depth is farther than any before
					{
						maxLength = charSequence.length(); //update our max length
						result.setObject(charSequence.toString()); //make a copy and keep track of the resulting string
					}
				}
				return true;
			}
		});
		return result.getObject(); //return the result, if any
	}

	/**
	 * Determines the longest subsequence that is repeated in the given subsequence. TODO explain algorithm
	 * @param charSequence The character sequence to check.
	 * @return The longest repeated subsequence in the given character sequence, or <code>null</code> if no subsequence is repeated.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	public static CharSequence getLongestSequentialRepeatedSubsequence(final CharSequence charSequence)
	{
		final CharSequenceSuffixTree suffixTree = CharSequenceSuffixTree.create(charSequence); //create a suffix tree
		final ObjectHolder<String> result = new ObjectHolder<String>(); //create an object to hold the resulting string
		visit(suffixTree, new AbstractCharSequenceVisitor()
		{
			int maxLength = 0; //keep track of the longest length

			@Override
			public boolean visit(final SuffixTree suffixTree, final CharSequenceNode node, final CharSequenceEdge parentEdge, final CharSequence charSequence)
			{
				if(!node.isLeaf()) //ignore leaf nodes---they aren't repeated sequences
				{
					if(charSequence.length() > maxLength) //if this depth is farther than any before, see if the repeat sequence is sequential
					{
						if(parentEdge.getChildNode().startsWith(charSequence)) //if the same sequence appears starting with the edge's child node
						{
							maxLength = charSequence.length(); //update our max length
							result.setObject(charSequence.toString()); //make a copy and keep track of the sequentially repeated sequence
						}
					}
				}
				return true;
			}
		});
		return result.getObject(); //return the result, if any
	}

	/**
	 * An abstract implementation of a visitor for character sequences. This implementation keeps track of the current sequence being visited for each node. Child
	 * classes must override {@link #visit(SuffixTree, CharSequenceNode, CharSequenceEdge, CharSequence)}.
	 * 
	 * @author Garret Wilson
	 */
	public static abstract class AbstractCharSequenceVisitor implements Visitor<CharSequenceNode, CharSequenceEdge>
	{
		/** The string builder to keep track of the current sequence. */
		final StringBuilder sequenceBuilder;

		/** Default constructor starting an empty sequence. */
		public AbstractCharSequenceVisitor()
		{
			this("");
		}

		/**
		 * Character sequence constructor. This constructor is useful for creating a visitor that will begin on a non-root node.
		 * @param charSequence The initial character sequence.
		 * @throws NullPointerException if the given character sequence is <code>null</code>.
		 */
		public AbstractCharSequenceVisitor(final CharSequence charSequence)
		{
			sequenceBuilder = new StringBuilder(charSequence);
		}

		/**
		 * {@inheritDoc} This version first updates the current sequence and then calls {@link #visit(SuffixTree, CharSequenceNode, CharSequenceEdge, CharSequence)}
		 * .
		 */
		@Override
		public final boolean visit(final SuffixTree suffixTree, final CharSequenceNode node, final CharSequenceEdge parentEdge, final int length)
		{
			if(parentEdge != null) //if this isn't the root node
			{
				sequenceBuilder.replace(length - parentEdge.getLength(), sequenceBuilder.length(), parentEdge.getSubSequence().toString()); //append this edge's subsequence to our current position (the string builder will be filled sequentially)
			}
			return visit(suffixTree, node, parentEdge, sequenceBuilder); //visit the node with the current sequence
		}

		/**
		 * Visits the given node. The provided sequence will be modified on further visits; if it is desired that the sequence should be stored, a copy of it should
		 * first be made.
		 * @param suffixTree The suffix tree being visited.
		 * @param node The node being visited.
		 * @param parentEdge The parent edge of the node being visited, or <code>null</code> if the node has no parent.
		 * @param charSequence The current sequence from the root to the node being visited.
		 * @return <code>true</code> if visiting should continue to other nodes.
		 */
		public abstract boolean visit(final SuffixTree suffixTree, final CharSequenceNode node, final CharSequenceEdge parentEdge, final CharSequence charSequence);
	}

}
