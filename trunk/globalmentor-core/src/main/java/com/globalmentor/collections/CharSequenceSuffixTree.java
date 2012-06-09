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

import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Objects.*;

import java.util.*;

import com.globalmentor.collections.iterators.*;
import com.globalmentor.java.*;
import com.globalmentor.model.Filter;

/**
 * A suffix tree for a sequence of characters.
 * 
 * <p>
 * This class builds a suffix tree from a sequence of characters in O(N) time following Ukkonen's algorithm. The first version of this algorithm was followed by
 * closely following Mark Nelson's explanation and C++ algorithm presented in <a href="http://marknelson.us/1996/08/01/suffix-trees/">Fast String Searching With
 * Suffix Trees</a>.
 * </p>
 * <p>
 * Notes:
 * </p>
 * <ul>
 * <li>This implementation uses exclusive end positions rather than inclusive end positions, which are more intuitive, make calculations easier, and interact
 * nicely with the Java API.</li>
 * <li>The original implementation needlessly tied the edge splitting logic to a particular suffix. While this doesn't affect functionality, it doesn't
 * logically isolate the process of splitting an edge at a particular location, which is completely independent from a suffix. It merely needs to be known at
 * what point along the edge the split should occur. This implementation also splits an edge by creating two new edges rather than merely modifying one.</li>
 * <li>The original article used a "suffix" class that with a node and character indexes. This implementation reverts to the more general "state" terminology
 * used by Ukkonen. Furthermore, the end character index has been changed to exclusive, allowing a state "length" property to be more natural. It also allows
 * the "explicit" state to be more readily apparently---the state in which <code>start==end</code>. Finally, these modifications reduce the state canonization
 * logic to simply "consume edges until the next edge is not small enough to consume or the state is explicit".</li>
 * <li>The original implementation kept a record of the current last character being added. With every iteration the suffix/state had its endpoint incremented,
 * making a separate last-character variable redundant.</li>
 * <li>The algorithm here checks to see when the state start has gone past the end; this signals that the current iteration is finished, and there is no need
 * loop around and check for an edge emanating from the current active node, because the state was explicit in the previous iteration so an edge had to have
 * been created.</li>
 * <li>The traditional algorithm has been modified slightly to construct an explicit suffix tree on the last round without the need of a unique
 * "dummy character" appended to the string. This may result in some edges that are empty, as well as an extra, empty edge emanating from the root node
 * representing the empty string suffix.</li>
 * </ul>
 * 
 * @author Garret Wilson
 * 
 * @see <a href="http://marknelson.us/1996/08/01/suffix-trees/">Mark Nelson: Fast String Searching With Suffix Trees</a>
 * @see <a href="http://marknelson.us/code-use-policy/">Mark Nelson: Liberal Code Use Policy</a>
 */
public class CharSequenceSuffixTree extends AbstractSuffixTree<CharSequenceSuffixTree.CharSequenceEdge>
{

	/** The character sequence represented by the suffix tree. */
	private final CharSequence charSequence;

	/** @return The character sequence represented by the suffix tree. */
	public CharSequence getCharSequence()
	{
		return charSequence;
	}

	@Override
	public CharSequenceNode getRootNode()
	{
		return (CharSequenceNode)super.getRootNode();
	}

	@Override
	public CharSequenceNode getNode(final int nodeIndex)
	{
		return (CharSequenceNode)super.getNode(nodeIndex);
	}

	@Override
	protected AbstractNode createNode(final int index)
	{
		return new CharSequenceNode(index);
	}

	/** Private reusable key for looking up edges. */
	private final LookupEdgeKey LOOKUP_EDGE_KEY = new LookupEdgeKey();

	/**
	 * The map of edges, keyed to their parent node index and first character in the character sequence. Because edges are their own keys, they should be removed
	 * from the map before being updated and replaced.
	 */
	private final Map<EdgeKey, CharSequenceEdge> edgeMap = new HashMap<EdgeKey, CharSequenceEdge>();

	/** @return A read-only iterable of edges in the tree. */
	@Override
	public Collection<? extends Edge> getEdges()
	{
		return java.util.Collections.unmodifiableCollection(edgeMap.values());
	}

	/**
	 * {@inheritDoc}
	 * @throws ClassCastException if the given parent node and/or child node is not an instance of {@link CharSequenceNode}.
	 */
	@Override
	protected CharSequenceEdge createEdge(final Node parentNode, final Node childNode, final int start, final int end)
	{
		return new CharSequenceEdge((CharSequenceNode)parentNode, (CharSequenceNode)childNode, start, end); //create a new edge
	}

	/**
	 * Adds an edge to the tree.
	 * @param edge The edge to add.
	 * @throws NullPointerException if the given edge is <code>null</code>.
	 * @throws IllegalStateException if there already exists an edge with the same parent node and first character.
	 */
	@Override
	protected void addEdge(final CharSequenceEdge edge)
	{
		checkState(!edgeMap.containsKey(checkInstance(edge)), "Duplicate edge: " + edge);
		edgeMap.put(edge, edge); //an edge is its own key
	}

	/**
	 * Removes an edge from the tree. If the edge does not exist, no action occurs.
	 * @param edge The edge to remove.
	 * @throws NullPointerException if the given edge is <code>null</code>.
	 */
	@Override
	protected void removeEdge(final CharSequenceEdge edge)
	{
		edgeMap.remove(checkInstance(edge));
	}

	@Override
	protected CharSequenceNode splitEdge(final CharSequenceEdge edge, final int length)
	{
		return (CharSequenceNode)super.splitEdge(edge, length);
	}

	/**
	 * Character sequence constructor with a single root note. This class cannot be publicly instantiated, and must be created using a factory/builder method.
	 * @param charSequence The character sequence the suffix tree represents.
	 * @param explicit Whether the suffix tree is explicit, with every suffix ending on a leaf node.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	private CharSequenceSuffixTree(final CharSequence charSequence, final boolean explicit)
	{
		super(explicit);
		this.charSequence = checkInstance(charSequence);
	}

	/**
	 * Compares a character sequence with the characters starting at the root node and continuing along child edges.
	 * @param charSequence The character sequence to compare.
	 * @return <code>true</code> if there is a path matching the given character sequence starting at this node and continuing along child edges.
	 * @see CharSequenceNode#startsWith(CharSequence)
	 * @see CharSequenceEdge#startsWith(CharSequence)
	 */
	public boolean startsWith(final CharSequence charSequence)
	{
		return startsWith(charSequence, 0, charSequence.length());
	}

	/**
	 * Compares part of a character sequence with the characters starting at the root node and continuing along child edges.
	 * @param charSequence The character sequence to compare.
	 * @param start The start of the character sequence to compare, inclusive.
	 * @param end The end of the character sequence to compare, exclusive.
	 * @return <code>true</code> if there is a path matching the given character sequence starting at the root node and continuing along child edges.
	 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> are negative or greater than <code>length()</code>, or <code>start</code>
	 *           is greater than <code>end</code>.
	 * @see CharSequenceNode#startsWith(CharSequence, int, int)
	 * @see CharSequenceEdge#startsWith(CharSequence, int, int)
	 */
	public boolean startsWith(final CharSequence charSequence, final int start, final int end)
	{
		return getRootNode().startsWith(charSequence, start, end);
	}

	/**
	 * Represents an edge between a parent node and a child node in a suffix tree.
	 * 
	 * @author Garret Wilson
	 */
	protected class CharSequenceNode extends AbstractSuffixTree<CharSequenceSuffixTree.CharSequenceEdge>.AbstractNode
	{
		/**
		 * Index constructor.
		 * @param index The index of the node.
		 */
		public CharSequenceNode(final int index)
		{
			super(index);
		}

		@Override
		public CharSequenceNode getParentNode()
		{
			return (CharSequenceNode)super.getParentNode();
		}

		@Override
		public CharSequenceNode getSuffixNode()
		{
			return (CharSequenceNode)super.getSuffixNode();
		}

		@Override
		public Iterable<CharSequenceEdge> getChildEdges()
		{
			return new NodeEdgeIterable(this);
		}

		/**
		 * Retrieves the edge that extends from this and starts with the given character.
		 * @param firstChar The first character along the edge.
		 * @return The edge extending from the node starting with the given character, or <code>null</code> if the node has no such edge.
		 */
		public CharSequenceEdge getEdge(final char firstChar)
		{
			return edgeMap.get(LOOKUP_EDGE_KEY.forEdge(this, firstChar)); //retrieve an edge from the map, reusing the existing edge key
		}

		/**
		 * Compares a character sequence with the characters starting at this node and continuing along child edges.
		 * @param charSequence The character sequence to compare.
		 * @return <code>true</code> if there is a path matching the given character sequence starting at this node and continuing along child edges.
		 * @see CharSequenceEdge#startsWith(CharSequence)
		 */
		public boolean startsWith(final CharSequence charSequence)
		{
			return startsWith(charSequence, 0, charSequence.length());
		}

		/**
		 * Compares part of a character sequence with the characters starting at this node and continuing along child edges.
		 * @param charSequence The character sequence to compare.
		 * @param start The start of the character sequence to compare, inclusive.
		 * @param end The end of the character sequence to compare, exclusive.
		 * @return <code>true</code> if there is a path matching the given character sequence starting at this node and continuing along child edges.
		 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> are negative or greater than <code>length()</code>, or
		 *           <code>start</code> is greater than <code>end</code>.
		 * @see CharSequenceEdge#startsWith(CharSequence, int, int)
		 */
		public boolean startsWith(final CharSequence charSequence, final int start, final int end)
		{
			checkBounds(charSequence, start, end);
			if(isLeaf()) //leaves match no strings, not even the empty string
			{
				return false;
			}
			final int count = end - start;
			if(count == 0) //branch nodes can always match the empty string
			{
				return true;
			}
			final char firstChar = charSequence.charAt(start); //get the starting character
			final CharSequenceEdge childEdge = getEdge(firstChar); //see if we have an edge starting with this character
			if(childEdge == null) //if there is no such edge, we can't compare further along the sequence 
			{
				return false;
			}
			return childEdge.startsWith(charSequence, start, end); //delegate to the child edge for comparison
		}
	}

	/**
	 * A key identifying an edge of a node, uniquely identified by its parent node and first character (as no node in a suffix tree contains more than one edge
	 * starting with the same character).
	 * 
	 * @author Garret Wilson
	 */
	protected interface EdgeKey
	{
		/** @return The parent node. */
		public Node getParentNode();

		/** @return The first character of the edge, or {@link Characters#UNDEFINED_CHAR} if the edge is empty. */
		public char getFirstChar();
	}

	/**
	 * An abstract base class that implements hashing and equality for an edge key.
	 * 
	 * @author Garret Wilson
	 */
	protected static abstract class AbstractEdgeKey implements EdgeKey
	{

		/**
		 * {@inheritDoc} This version hashes the index of the parent node, if any, and first character.
		 * @see #getParentNode()
		 * @see #getFirstChar()
		 */
		@Override
		public int hashCode()
		{
			final Node parentNode = getParentNode();
			return Objects.getLongHashCode(parentNode != null ? getParentNode().getIndex() : 0, getFirstChar());
		}

		/**
		 * {@inheritDoc} This version compares parent node, if any, and first character.
		 * @see #getParentNode()
		 * @see #getFirstChar()
		 */
		@Override
		public boolean equals(final Object object)
		{
			if(object == this) //identity always implies equality
			{
				return true;
			}
			if(!(object instanceof EdgeKey))
			{
				return false;
			}
			final EdgeKey edgeKey = (EdgeKey)object;
			return Objects.equals(getParentNode(), edgeKey.getParentNode()) && getFirstChar() == edgeKey.getFirstChar();
		}
	}

	/**
	 * Reusable key for looking up edges.
	 * @author Garret Wilson
	 */
	private class LookupEdgeKey extends AbstractEdgeKey
	{

		private Node parentNode;

		@Override
		public Node getParentNode()
		{
			return parentNode;
		}

		private char firstChar;

		@Override
		public char getFirstChar()
		{
			return firstChar;
		}

		/**
		 * Initializes the edge key for the given edge description.
		 * @param parentNode The parent node representing the root end of the edge.
		 * @param firstChar The first character of the edge.
		 * @return This edge key, updated with the given values.
		 * @throws NullPointerException if the given parent node is <code>null</code>.
		 */
		public EdgeKey forEdge(final Node parentNode, final char firstChar)
		{
			this.parentNode = checkInstance(parentNode);
			this.firstChar = firstChar;
			return this;
		}
	}

	/**
	 * Represents an edge between a parent node and a child node in a suffix tree.
	 * 
	 * @author Garret Wilson
	 */
	protected class CharSequenceEdge extends AbstractEdgeKey implements SuffixTree.Edge
	{
		private final Node parentNode;

		@Override
		public Node getParentNode()
		{
			return parentNode;
		}

		private final CharSequenceNode childNode;

		@Override
		public CharSequenceNode getChildNode()
		{
			return childNode;
		}

		private final int start;

		/** @return The position of the start character, inclusive. */
		@Override
		public int getStart()
		{
			return start;
		}

		private final int end;

		/** @return The position of the last character, exclusive. */
		@Override
		public int getEnd()
		{
			return end;
		}

		/**
		 * Returns the length of the edge, i.e. <code>end</code>-<code>start</code>.
		 * @return The number of characters on the edge.
		 */
		@Override
		public int getLength()
		{
			return end - start;
		}

		/** @return <code>true</code> if this edge is empty and has no characters. */
		@Override
		public boolean isEmpty()
		{
			return end == start;
		}

		/** @return The first character of the edge. */
		@Override
		public char getFirstChar()
		{
			return isEmpty() ? UNDEFINED_CHAR : getCharSequence().charAt(getStart()); //return the undefined character if the edge is empty
		}

		/**
		 * Constructor.
		 * @param parentNode The parent node representing the root end of the edge.
		 * @param childNode The child node representing the leaf end of the edge.
		 * @param start The position of the start character, inclusive.
		 * @param end The position of the end character, exclusive.
		 * @throws NullPointerException if the given parent node and/or child node is <code>null</code>.
		 * @throws IllegalArgumentException if the given end is less than the start.
		 */
		public CharSequenceEdge(final CharSequenceNode parentNode, final CharSequenceNode childNode, final int start, final int end)
		{
			this.parentNode = checkInstance(parentNode);
			this.childNode = checkInstance(childNode);
			this.start = start;
			this.end = checkArgumentMinimum(end, start);
		}

		/** @return The subsequence of characters this edge represents. */
		public CharSequence getSubSequence()
		{
			return getCharSequence().subSequence(getStart(), getEnd());
		}

		/** @return An iterable to the child edges of this edge's child node. */
		public Iterable<CharSequenceEdge> getChildEdges()
		{
			return new NodeEdgeIterable(getChildNode());
		}

		/**
		 * Compares a character sequence with the characters at the start of this child edge and continuing along child edges.
		 * @param charSequence The character sequence to compare.
		 * @return <code>true</code> if there is a path matching the given character sequence starting at this edge and continuing along child edges.
		 * @see CharSequenceNode#startsWith(CharSequence)
		 */
		public boolean startsWith(final CharSequence charSequence)
		{
			return startsWith(charSequence, 0, charSequence.length());
		}

		/**
		 * Compares part of a character sequence with the characters at the start of this child edge and continuing along child edges.
		 * @param charSequence The character sequence to compare.
		 * @param start The start of the character sequence to compare, inclusive.
		 * @param end The end of the character sequence to compare, exclusive.
		 * @return <code>true</code> if there is a path matching the given character sequence starting at this edge and continuing along child edges.
		 * @throws StringIndexOutOfBoundsException if <code>start</code> or <code>end</code> are negative or greater than <code>length()</code>, or
		 *           <code>start</code> is greater than <code>end</code>.
		 * @see CharSequenceNode#startsWith(CharSequence, int, int)
		 */
		public boolean startsWith(final CharSequence charSequence, final int start, final int end)
		{
			checkBounds(charSequence, start, end);
			final int count = end - start;
			if(count == 0) //edges can always match the empty string
			{
				return true;
			}
			final int compareCount = Math.min(count, getLength()); //find out how many characters to compare; we can't compare more characters than we have
			final CharSequence edgeCharSequence = getCharSequence(); //we'll compare with the underlying character sequence
			if(!CharSequences.equals(edgeCharSequence, getStart(), getStart() + compareCount, charSequence, start, start + compareCount)) //compare just this portion; if they don't match
			{
				return false; //there is no other alternative for matching in a suffix tree (there is only one unique path from any given node)
			}
			if(compareCount == count) //if we compared all the remaining characters
			{
				return true; //we finished with a match
			}
			return getChildNode().startsWith(charSequence, start + compareCount, end); //compare the remaining characters with child edges
		}

		@Override
		public String toString()
		{
			return new StringBuilder().append(getParentNode()).append(' ').append(getSubSequence()).append(' ').append(getChildNode()).toString();
		}
	};

	/**
	 * Encapsulation of the current processing state of a suffix tree.
	 * <p>
	 * Before each processing iteration, the state indicates whether there are more characters to process using {@link State#hasNextChar()}. The character to
	 * process on each iteration is found at the index {@link State#getCharIndex()}. On each iteration the state is set to the active point and moved to smaller
	 * and smaller suffixes until the end point is reached using {@link State#nextSmallerSuffix()}. After each iteration, {@link State#nextChar()} is called and
	 * the next character can be retrieved again using {@link State#getCharIndex()}.
	 * </p>
	 * <p>
	 * The state end is exclusive, representing one character past the last character in the current suffix. The state start is inclusive, and represents the
	 * first character of an implicit suffix extending past the node. If the start is equal to or greater than the end, then there is no implicit suffix and the
	 * state is explicit.
	 * </p>
	 * 
	 * @author Garret Wilson
	 */
	private static class State
	{
		/** The suffix tree being processed. */
		private final CharSequenceSuffixTree suffixTree;

		private CharSequenceNode node;

		/** @return The active node of the state. */
		public CharSequenceNode getNode()
		{
			return node;
		}

		private int start;

		/** @return The starting character of the state, inclusive. */
		public int getStart()
		{
			return start;
		}

		private int end;

		/** @return The ending character of the state, exclusive. */
		public int getEnd()
		{
			return end;
		}

		/**
		 * Returns The index of the character currently being processed. This is a convenience method that delegates to {@link #getEnd()}.
		 * @return The index of the character currently being processed.
		 * @see #getEnd()
		 */
		public final int getCharIndex()
		{
			return getEnd();
		}

		/**
		 * The last position for the end; usually the length of the character sequence or, if an explicit suffix tree is being created, one past the end of the
		 * string.
		 */
		private final int finish;

		/**
		 * @return The last position for the end, which may be past the end of the character sequence depending on whether an explicit suffix tree is being
		 *         constructed.
		 */
		protected int getFinish()
		{
			return finish;
		}

		/**
		 * Constructor. Starts the state on the first node at the first position with no characters.
		 * @param suffixTree The suffix tree being processed.
		 * @throws NullPointerException if the given suffix tree is <code>null</code>.
		 */
		public State(final CharSequenceSuffixTree suffixTree, final boolean explicit)
		{
			this.suffixTree = checkInstance(suffixTree);
			this.node = suffixTree.getNode(0); //start on the root node
			this.start = 0;
			this.end = 0;
			this.finish = suffixTree.getCharSequence().length() + (explicit ? 1 : 0); //add an extra phantom character if we are creating an explicit suffix tree
		}

		/**
		 * Returns the length of the state, i.e. <code>end</code>-<code>start</code>.
		 * @return The number of characters between the start and end.
		 */
		public int getLength()
		{
			return end - start;
		}

		/**
		 * Determines whether the state is explicit (that is, it has no character length beyond the node).
		 * @return <code>true</code> if this is an explicit state, ending on the state node.
		 */
		public boolean isExplicit()
		{
			return start >= end; //this algorithm implementation allows the start to temporarily become greater than the end
		}

		/**
		 * Navigates to the next position in the suffix tree, which will represent the next smallest suffix. This method is to be called after each suffix
		 * extension.
		 * @return <code>true</code> if there exist smaller suffixes, or <code>false</code> if this pass is finished and the next character, if any, should be
		 *         processed.
		 * @throws IllegalStateException if the state is on the first node and the start is already past the end, reflecting that logically the algorithm will never
		 *           require the start to extend more than one position past the end.
		 */
		public boolean nextSmallerSuffix()
		{
			if(node.getIndex() == 0) //if we're on the first node, there is no linked suffix node
			{
				checkState(start <= end, "Cannot increment start of state when it is already past its end.");
				start++; //just increment the start of the state to get a smaller suffix; this algorithm allows the start to temporarily pass the end
				if(start > end)
				{
					return false;
				}
			}
			else
			//for all other suffixes, we can navigate to the smaller suffix by traversing suffix links
			{
				node = node.getSuffixNode(); //navigate to the previous suffix
			}
			canonize(); //canonize the state by consuming edges if we can
			return true;
		}

		/**
		 * Determines whether there are more characters to be processed.
		 * @return <code>false</code> if there remain unprocessed characters.
		 */
		public boolean hasNextChar()
		{
			return getEnd() < getFinish(); //compare to the finish, which could be longer than the string
		}

		/**
		 * Navigates to the next character. This method is to be called after navigation to the end point for each character.
		 * @throws IllegalStateException if there are no more characters in the character sequence.
		 * @see #hasNextChar()
		 */
		public void nextChar()
		{
			checkState(hasNextChar(), "No more characters to process.");
			end++; //go to the next character
			canonize();
		}

		/**
		 * Canonizes the state by "eating" all complete edges contained between the start and the end of the state. After this operation, the length of the state
		 * will be smaller than the length of the edge, if any, extending from the state node.
		 */
		public void canonize()
		{
			//keep eating edges until there are no more edges small enough to eat 
			while(!isExplicit()) //if the state is explicit, there's no way we could eat an edge
			{
				final CharSequenceEdge edge = getNode().getEdge(suffixTree.getCharSequence().charAt(getStart())); //get the edge coming from our node
				final int edgeLength = edge.getLength(); //get the length of that edge
				if(edgeLength > getLength()) //if this edge is larger than our implicit characters
				{
					break; //we found an edge to large to eat, so stop eating
				}
				start += edgeLength; //eat the edge
				node = edge.getChildNode(); //move the state to the end node of the edge
			}
		}

	}

	/**
	 * An iterable that returns an iterator to edges for a given node.
	 * 
	 * @author Garret Wilson
	 */
	private class NodeEdgeIterable implements Iterable<CharSequenceEdge>
	{

		/** The node serving as the parent of all edges to return. */
		private final Node parentNode;

		/**
		 * Parent node constructor.
		 * @param parentNode The node serving as the parent of all edges to return.
		 * @throws NullPointerException if the given parent node is <code>null</code>.
		 */
		public NodeEdgeIterable(final Node parentNode)
		{
			this.parentNode = checkInstance(parentNode);
		}

		@Override
		public Iterator<CharSequenceEdge> iterator()
		{
			return new MapEntryNodeEdgeIterator(parentNode);
		}
	}

	/**
	 * An iterator that iterates through all edges for a given node by iterating through all entries in the edge map. This is an expensive operation, guaranteeing
	 * that all node edges are found by a brute force search.
	 * 
	 * @author Garret Wilson
	 */
	private class MapEntryNodeEdgeIterator extends FilteredIterator<CharSequenceEdge> //TODO add per-node linked lists or other shortcuts to speed iteration
	{
		/**
		 * Parent node constructor.
		 * @param parentNode The node serving as the parent of all edges to return.
		 */
		public MapEntryNodeEdgeIterator(final Node parentNode)
		{
			super(edgeMap.values().iterator(), new Filter<CharSequenceEdge>() //we'll filter the edges
					{
						public boolean isPass(final CharSequenceEdge edge)
						{
							return edge.getParentNode().getIndex() == parentNode.getIndex(); //TODO add equals() method
						}
					});
		}
	}

	/**
	 * Suffix tree builder factory method which creates a new, explicit suffix tree for a given character sequence. The created suffix tree will have one more
	 * leaf node than the number of characters in the sequence, because there will exist an empty edge from the root indicating the empty string.
	 * @param charSequence The character sequence for which a suffix tree should be built.
	 * @return The new suffix tree for the given character sequence.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	public static CharSequenceSuffixTree create(final CharSequence charSequence)
	{
		return create(charSequence, true);
	}

	/**
	 * Suffix tree builder factory method which creates a new suffix tree for a given character sequence. If an explicit suffix tree is requested, the created
	 * suffix tree will have one more leaf node than the number of characters in the sequence, because there will exist an empty edge from the root indicating the
	 * empty string.
	 * @param charSequence The character sequence for which a suffix tree should be built.
	 * @param explicit Whether an explicit suffix tree should be constructed.
	 * @return The new suffix tree for the given character sequence.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	protected static CharSequenceSuffixTree create(final CharSequence charSequence, final boolean explicit)
	{
		final int charSequenceLength = charSequence.length();
		final CharSequenceSuffixTree suffixTree = new CharSequenceSuffixTree(charSequence, explicit); //create a new suffix tree that hasn't yet been built
		final State state = new State(suffixTree, explicit); //create a new state for processing the suffix tree
		while(state.hasNextChar()) //while there are more characters to process
		{
			final int nextCharIndex = state.getCharIndex(); //get the index of the next character to process
			assert explicit ? nextCharIndex <= charSequenceLength : nextCharIndex < charSequenceLength; //explicit tree construction allows us to go one character past the end of the sequence
			final boolean isExplicitRound = nextCharIndex == charSequenceLength; //see if we're on the final, explicit round, in which the possibly implicit suffix tree is turned into an explicit suffix tree
			final char nextChar = isExplicitRound ? UNDEFINED_CHAR : charSequence.charAt(nextCharIndex); //get the next character (or some dummy character, if we are on the explicit round)

			CharSequenceNode parentNode;
			CharSequenceNode lastParentNode = null;
			do
			{
				parentNode = state.getNode(); //start at the active node
				//try to find an appropriate edge coming out of the active node; if there is such an edge (either explicit or implicit), we are finished

				if(state.isExplicit()) //if the active state ends on a node (it doesn't end in the middle of an edge)
				{
					if(!isExplicitRound && state.getNode().getEdge(nextChar) != null) //if there is already an edge from the state node starting with the next character (the explicit round always results in a new edge)
					{
						break; //there's nothing to do
					}
				}
				else
				//if the state is implicit, ending in the middle of an edge
				{
					final CharSequenceEdge edge = state.getNode().getEdge(charSequence.charAt(state.getStart())); //get the edge at which the implicit part of the state starts
					final int stateLength = state.getLength();
					if(!isExplicitRound && charSequence.charAt(edge.getStart() + stateLength) == nextChar) //if the next character along the edge is the character we're extending  (the explicit round always results in a new edge)
					{
						break; //there's nothing to do---we simply have another implicit suffix
					}
					else
					//if a different character appears in the middle of the edge, we'll need to split the edge to add another edge
					{
						final CharSequenceNode newNode = suffixTree.splitEdge(edge, stateLength); //split out a new edge from the existing edge; the new node will become the new parent to which we'll add a new edge
						newNode.setSuffixNode(state.getNode()); //the new node is an extension of the old extension---so point its suffix back to the node that is the original suffix
						parentNode = newNode;
					}
				}
				//if there is no matching edge (or we split an existing edge), we'll need to create a new edge
				//since we are going to smaller and smaller suffixes, if we created an edge for a larger suffix earlier, link that parent back to this one
				suffixTree.addEdge(parentNode, suffixTree.addNode(), nextCharIndex, charSequenceLength); //create a new edge from the next character to the end of the sequence
				if(lastParentNode != null) //if we already created an edge for a larger suffix
				{
					lastParentNode.setSuffixNode(parentNode); //link the node for the larger suffix back to this node, which is for a smaller suffix
				}
				lastParentNode = parentNode; //move to the next parent node
			}
			while(state.nextSmallerSuffix()); //move to the next smaller suffix
			if(lastParentNode != null)
			{
				lastParentNode.setSuffixNode(parentNode);
			}
			state.nextChar(); //go to the next character
		}

		return suffixTree; //return the suffix tree we created and built
	};
}
