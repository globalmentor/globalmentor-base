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

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Objects.*;

import java.io.PrintStream;
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
 * Suffix Trees</a>. Note that:
 * <ul>
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
 * </ul>
 * </p>
 * 
 * @author Garret Wilson
 * 
 * @see <a href="http://marknelson.us/1996/08/01/suffix-trees/">Mark Nelson: Fast String Searching With Suffix Trees</a>
 * @see <a href="http://marknelson.us/code-use-policy/">Mark Nelson: Liberal Code Use Policy</a>
 */
public class CharSequenceSuffixTree
{

	/** The character sequence represented by the suffix tree. */
	private final CharSequence charSequence;

	/** @return The character sequence represented by the suffix tree. */
	public CharSequence getCharSequence()
	{
		return charSequence;
	}

	/** The nodes in the suffix tree. */
	private final List<Node> nodes = new ArrayList<Node>();

	/** @return A read-only list of the nodes in the tree. */
	protected List<Node> getNodes()
	{
		return java.util.Collections.unmodifiableList(nodes);
	}

	/** @return The number of nodes in the suffix tree. */
	public int getNodeCount()
	{
		return nodes.size();
	}

	/**
	 * Retrieves the identified node.
	 * @param nodeIndex The index of the node to retrieve.
	 * @return The identified node.
	 * @throws IndexOutOfBoundsException if the given node index does not identify a node in this suffix tree.
	 */
	public Node getNode(final int nodeIndex)
	{
		return nodes.get(nodeIndex);
	}

	/**
	 * Creates a new node and adds it to the internal list of nodes.
	 * @return The index of the newly created node.
	 */
	private int createNode()
	{
		final int nodeIndex = nodes.size();
		nodes.add(nodeIndex, new Node());
		return nodeIndex;
	}

	/** Private reusable key for looking up edges. */
	private final LookupEdgeKey LOOKUP_EDGE_KEY = new LookupEdgeKey();

	/**
	 * The map of edges, keyed to their parent node index and first character in the character sequence. Because edges are their own keys, they should be removed
	 * from the map before being updated and replaced.
	 */
	private final Map<EdgeKey, Edge> edgeMap = new HashMap<EdgeKey, Edge>();

	/** @return A read-only iterable of edges in the tree. */
	public Collection<Edge> getEdges()
	{
		return java.util.Collections.unmodifiableCollection(edgeMap.values());
	}

	/**
	 * Creates a new edge and adds it to the tree.
	 * @param parentNodeIndex The index of the parent node representing the root end of the edge.
	 * @param childNodeIndex The index of the child node representing the leaf end of the edge.
	 * @param start The position of the start character, inclusive.
	 * @param end The position of the end character, exclusive.
	 * @throws IllegalArgumentException if the given end is equal to or less than the start.
	 * @throws IllegalStateException if there already exists an edge with the same parent node and first character.
	 */
	protected Edge addEdge(final int parentNodeIndex, final int childNodeIndex, final int start, final int end)
	{
		final Edge edge = new Edge(parentNodeIndex, childNodeIndex, start, end); //create a new edge
		addEdge(edge); //add the edge
		return edge; //return the edge
	}

	/**
	 * Adds an edge to the tree.
	 * @param edge The edge to add.
	 * @throws NullPointerException if the given edge is <code>null</code>.
	 * @throws IllegalStateException if there already exists an edge with the same parent node and first character.
	 */
	private void addEdge(final Edge edge)
	{
		checkState(!edgeMap.containsKey(checkInstance(edge)), "Duplicate edge: " + edge);
		edgeMap.put(edge, edge); //an edge is its own key
	}

	/**
	 * Retrieves the edge that extends from the given parent node and starts with the given character.
	 * <p>
	 * This method is not thread safe.
	 * </p>
	 * @param parentNodeIndex The parent node of the edge.
	 * @param firstChar The first character along the edge.
	 * @return The edge extending from the given parent node starting with the given character, or <code>null</code> if the given parent node has no such edge.
	 * @throws IndexOutOfBoundsException if the given parent node index does not represent a valid node.
	 */
	public Edge getEdge(final int parentNodeIndex, final char firstChar)
	{
		checkIndexBounds(parentNodeIndex, getNodeCount());
		return edgeMap.get(LOOKUP_EDGE_KEY.forEdge(parentNodeIndex, firstChar)); //retrieve an edge from the map, reusing the existing edge key
	}

	/**
	 * Removes an edge from the tree. If the edge does not exist, no action occurs.
	 * @param edge The edge to remove.
	 * @throws NullPointerException if the given edge is <code>null</code>.
	 */
	protected void removeEdge(final Edge edge)
	{
		edgeMap.remove(checkInstance(edge));
	}

	/**
	 * Character sequence constructor with a single root note. This class cannot be publicly instantiated, and must be created using a factory/builder method.
	 * @param charSequence The character sequence the suffix tree represents.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	private CharSequenceSuffixTree(final CharSequence charSequence)
	{
		this.charSequence = checkInstance(charSequence);
		createNode(); //create the root node
	}

	/**
	 * Splits an edge into two. The first, near edge will be of the given length; the second, far edge will be of the remaining length (that is, the length of the
	 * original edge minus the given length). A new node will be created as the mid-point between the original edge nodes, becoming the child node of the first
	 * edge and the parent node of the second edge.
	 * @param edge The edge to split.
	 * @param length The position at which to split the edge.
	 * @return The index of the created node splitting the edge.
	 */
	private int splitEdge(final Edge edge, final int length)
	{
		removeEdge(edge); //remove the existing edge from our map, because we're going to create two edges to replace it
		final int newNodeIndex = createNode(); //create a new node with which to split the edge into two
		final int split = edge.getStart() + length; //the character location at which to make the split
		addEdge(edge.getParentNodeIndex(), newNodeIndex, edge.getStart(), split); //the near edge will start at the same place as the old edge, but only go part way---to the new node
		addEdge(newNodeIndex, edge.childNodeIndex, split, edge.getEnd()); //the far edge starts where the new near edge ends
		return newNodeIndex; //return the index of the new node
	}

	/**
	 * Returns an iterable to the child edges of the suffix tree's root node.
	 * @return An iterable to the child edges of the root node.
	 */
	public Iterable<Edge> getRootEdges()
	{
		return getChildEdges(0); //the root node is always the first node in the list
	}

	/**
	 * Returns an iterable to the child edges of a node in the suffix tree.
	 * @param parentNodeIndex The index of the parent node.
	 * @return An iterable to the child edges of the indicated node.
	 * @throws IndexOutOfBoundsException if the given parent node index does not represent a valid node.
	 */
	public Iterable<Edge> getChildEdges(final int parentNodeIndex)
	{
		return new NodeEdgeIterable(checkIndexBounds(parentNodeIndex, getNodeCount()));
	}

	/**
	 * Suffix tree builder factory method which creates a new suffix tree for a given character sequence.
	 * @param charSequence The character sequence for which a suffix tree should be built.
	 * @return The new suffix tree for the given character sequence.
	 * @throws NullPointerException if the given character sequence is <code>null</code>.
	 */
	public static CharSequenceSuffixTree create(final CharSequence charSequence)
	{
		final CharSequenceSuffixTree suffixTree = new CharSequenceSuffixTree(charSequence); //create a new suffix tree that hasn't yet been built
		final State state = new State(suffixTree); //create a new state for processing the suffix tree
		while(state.hasNextChar()) //while there are more characters to process
		{
			final int nextCharIndex = state.getCharIndex(); //get the index of the next character to process
			final char nextChar = charSequence.charAt(nextCharIndex); //get the next character

			int parentNodeIndex;
			int lastParentNodeIndex = -1;
			do
			{
				parentNodeIndex = state.getNodeIndex(); //start at the active node
				//try to find an appropriate edge coming out of the active node; if there is such an edge (either explicit or implicit), we are finished
				final Edge edge;
				if(state.isExplicit()) //if the active state ends on a node (it doesn't end in the middle of an edge)
				{
					edge = suffixTree.getEdge(state.getNodeIndex(), nextChar); //the node at which the state ends has an edge starting with the next character
					if(edge != null) //if there is an edge
					{
						break; //there's nothing to do
					}
				}
				else
				//if the state is implicit, ending in the middle of an edge
				{
					edge = suffixTree.getEdge(state.getNodeIndex(), charSequence.charAt(state.getStart())); //get the edge at which the implicit part of the state starts
					final int stateLength = state.getLength();
					if(charSequence.charAt(edge.getStart() + stateLength) == nextChar) //if the next character along the edge is the character we're extending
					{
						break; //there's nothing to do---we simply have another implicit suffix
					}
					else
					//if a different character appears in the middle of the edge, we'll need to split the edge to add another edge
					{
						parentNodeIndex = suffixTree.splitEdge(edge, stateLength); //split out a new edge from the existing edge; the new node will become the new parent to which we'll add a new edge
						suffixTree.getNode(parentNodeIndex).setSuffixNodeIndex(state.getNodeIndex()); //the new node is an extension of the old extension---so point its suffix back to the node that is the original suffix
					}
				}

				//if there is no matching edge (or we split an existing edge), we'll need to create a new edge
				//since we are going to smaller and smaller suffixes, if we created an edge for a larger suffix earlier, link that parent back to this one
				suffixTree.addEdge(parentNodeIndex, suffixTree.createNode(), nextCharIndex, charSequence.length()); //create a new edge from the next character to the end of the sequence
				if(lastParentNodeIndex > 0) //if we already created an edge for a larger suffix
				{
					suffixTree.getNode(lastParentNodeIndex).setSuffixNodeIndex(parentNodeIndex); //link the node for the larger suffix back to this node, which is for a smaller suffix
				}
				lastParentNodeIndex = parentNodeIndex; //move to the next parent node
			}
			while(state.nextSmallerSuffix()); //move to the next smaller suffix
			if(lastParentNodeIndex > 0)
			{
				suffixTree.getNode(lastParentNodeIndex).setSuffixNodeIndex(parentNodeIndex);
			}
			state.nextChar(); //go to the next character
		}

		return suffixTree; //return the suffix tree we created and built
	};

	/**
	 * Prints a character representation of the tree and its branches, starting from the root node.
	 * @param printStream The destination to which the tree should be printed.
	 * @throws NullPointerException if the given print stream is <code>null</code>.
	 */
	protected void printTree(final PrintStream printStream)
	{
		printTree(printStream, getRootEdges(), 0); //print the root edges at a level of zero
	}

	/**
	 * Prints a character representation of the child edges of the given edge.
	 * @param printStream The destination to which the tree should be printed.
	 * @param edge The edge the child edges of which to print.
	 * @param level The zero-based level of the tree from the root.
	 * @throws NullPointerException if the given print stream and/or edge is <code>null</code>.
	 */
	private void printTree(final PrintStream printStream, final Edge edge, final int level)
	{
		printStream.println(Strings.createString('\t', level) + edge.toString()); //indent and print the edge
		printTree(printStream, edge.getChildEdges(), level + 1); //print the edge's child edges at one more level down

	}

	/**
	 * Prints a character representation of the given child edges.
	 * @param printStream The destination to which the tree should be printed.
	 * @param edges The child edges to print.
	 * @param level The zero-based level of the tree from the root.
	 * @throws NullPointerException if the given print stream and/or edges is <code>null</code>.
	 */
	private void printTree(final PrintStream printStream, final Iterable<Edge> edges, final int level)
	{
		for(final Edge edge : edges) //look at all the given edges
		{
			printTree(printStream, edge, level); //print each edge at the requested level
		}
	}

	/**
	 * Represents a node in a suffix tree. Each node defaults to having no suffix node.
	 * 
	 * @author Garret Wilson
	 */
	public class Node
	{
		private int suffixNodeIndex = -1;

		/** The index of the node representing the next smaller suffix, or -1 if there is no known smaller suffix node. */
		public int getSuffixNodeIndex()
		{
			return suffixNodeIndex;
		}

		/**
		 * Sets the index of the node representing the next smaller suffix.
		 * @param suffixNodeIndex The index of the node representing the next smaller suffix.
		 * @throws IndexOutOfBoundsException if the suffix node index does not represent a valid node.
		 */
		public void setSuffixNodeIndex(final int suffixNodeIndex)
		{
			this.suffixNodeIndex = checkIndexBounds(suffixNodeIndex, getNodeCount());
		}

	};

	/**
	 * A key identifying an edge of a node, uniquely identified by its parent node and first character (as no node in a suffix tree contains more than one edge
	 * starting with the same character).
	 * 
	 * @author Garret Wilson
	 */
	public interface EdgeKey
	{
		/** @return The index of the parent node. */
		public int getParentNodeIndex();

		/** @return The first character of the edge. */
		public char getFirstChar();
	}

	/**
	 * An abstract base class that implements hashing and equality for an edge key.
	 * 
	 * @author Garret Wilson
	 */
	protected static abstract class AbstractEdgeKey implements EdgeKey
	{

		@Override
		public int hashCode()
		{
			return Objects.getLongHashCode(getParentNodeIndex(), getFirstChar());
		}

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
			return getParentNodeIndex() == edgeKey.getParentNodeIndex() && getFirstChar() == edgeKey.getFirstChar();
		}
	}

	/**
	 * Reusable key for looking up edges.
	 * @author Garret Wilson
	 */
	private class LookupEdgeKey extends AbstractEdgeKey
	{

		private int parentNodeIndex;

		@Override
		public int getParentNodeIndex()
		{
			return parentNodeIndex;
		}

		private char firstChar;

		@Override
		public char getFirstChar()
		{
			return firstChar;
		}

		/**
		 * Initializes the edge key for the given edge description.
		 * @param parentNodeIndex The index of the parent node representing the root end of the edge.
		 * @param firstChar The first character of the edge.
		 * @return This edge key, updated with the given values.
		 */
		public EdgeKey forEdge(final int parentNodeIndex, final char firstChar)
		{
			this.parentNodeIndex = parentNodeIndex;
			this.firstChar = firstChar;
			return this;
		}
	}

	/**
	 * Represents an edge between a parent node and a child node in a suffix tree. Every edge must span at least one character.
	 * 
	 * @author Garret Wilson
	 */
	public class Edge extends AbstractEdgeKey
	{
		private final int parentNodeIndex;

		/** @return The index of the parent node representing the root end of the edge. */
		@Override
		public int getParentNodeIndex()
		{
			return parentNodeIndex;
		}

		private final int childNodeIndex;

		/** @return The index of the child node representing the leaf end of the edge. */
		public int getChildNodeIndex()
		{
			return childNodeIndex;
		}

		private final int start;

		/** @return The position of the start character, inclusive. */
		public int getStart()
		{
			return start;
		}

		private final int end;

		/** @return The position of the last character, exclusive. */
		public int getEnd()
		{
			return end;
		}

		/**
		 * Returns the length of the edge, i.e. <code>end</code>-<code>start</code>.
		 * @return The number of characters on the edge.
		 */
		public int getLength()
		{
			return end - start;
		}

		/** @return The first character of the edge. */
		@Override
		public char getFirstChar()
		{
			return getCharSequence().charAt(getStart());
		}

		/** @return The subsequence of characters this edge represents. */
		public CharSequence getSubSequence()
		{
			return getCharSequence().subSequence(getStart(), getEnd());
		}

		/**
		 * Constructor.
		 * @param parentNodeIndex The index of the parent node representing the root end of the edge.
		 * @param childNodeIndex The index of the child node representing the leaf end of the edge.
		 * @param start The position of the start character, inclusive.
		 * @param end The position of the end character, exclusive.
		 * @throws IllegalArgumentException if the given end is equal to or less than the start.
		 */
		public Edge(final int parentNodeIndex, final int childNodeIndex, final int start, final int end)
		{
			this.parentNodeIndex = parentNodeIndex;
			this.childNodeIndex = childNodeIndex;
			this.start = start;
			this.end = checkArgumentMinimum(end, start + 1);
		}

		/** @return An iterable to the child edges of this edge's child node. */
		public Iterable<Edge> getChildEdges()
		{
			return new NodeEdgeIterable(getChildNodeIndex());
		}

		@Override
		public String toString()
		{
			final StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.append('(').append(getParentNodeIndex()).append(')');
			stringBuilder.append(' ').append(getSubSequence()).append(' ');
			stringBuilder.append('(').append(getChildNodeIndex()).append(')');
			return stringBuilder.toString();
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

		private int nodeIndex;

		/** @return The index of the node of the state. */
		public int getNodeIndex()
		{
			return nodeIndex;
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
		 * Constructor. Starts the state on the first node at the first position with no characters.
		 * @param suffixTree The suffix tree being processed.
		 * @throws NullPointerException if the given suffix tree is <code>null</code>.
		 */
		public State(final CharSequenceSuffixTree suffixTree)
		{
			this.suffixTree = checkInstance(suffixTree);
			this.nodeIndex = 0;
			this.start = 0;
			this.end = 0;
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
			if(nodeIndex == 0) //if we're on the first node, there is no linked suffix node
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
				nodeIndex = suffixTree.getNode(nodeIndex).getSuffixNodeIndex(); //navigate to the previous suffix
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
			return getEnd() < suffixTree.getCharSequence().length();
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
				final Edge edge = suffixTree.getEdge(getNodeIndex(), suffixTree.getCharSequence().charAt(getStart())); //get the edge coming from our node
				final int edgeLength = edge.getLength(); //get the length of that edge
				if(edgeLength > getLength()) //if this edge is larger than our implicit characters
				{
					break; //we found an edge to large to eat, so stop eating
				}
				start += edgeLength; //eat the edge
				nodeIndex = edge.getChildNodeIndex(); //move the state to the end node of the edge
			}
		}

	}

	/**
	 * An iterable that returns an iterator to edges for a given node.
	 * 
	 * @author Garret Wilson
	 */
	private class NodeEdgeIterable implements Iterable<Edge>
	{

		/** The index of the node serving as the parent of all edges to return. */
		private final int parentNodeIndex;

		/**
		 * Parent node index constructor.
		 * @param parentNodeIndex The index of the node serving as the parent of all edges to return.
		 */
		public NodeEdgeIterable(final int parentNodeIndex)
		{
			this.parentNodeIndex = parentNodeIndex;
		}

		@Override
		public Iterator<Edge> iterator()
		{
			return new MapEntryNodeEdgeIterator(parentNodeIndex);
		}
	}

	/**
	 * An iterator that iterates through all edges for a given node by iterating through all entries in the edge map. This is an expensive operation, guaranteeing
	 * that all node edges are found by a brute force search.
	 * 
	 * @author Garret Wilson
	 */
	private class MapEntryNodeEdgeIterator extends FilteredIterator<Edge>
	{
		/**
		 * Parent node index constructor.
		 * @param parentNodeIndex The index of the node serving as the parent of all edges to return.
		 */
		public MapEntryNodeEdgeIterator(final int parentNodeIndex)
		{
			super(edgeMap.values().iterator(), new Filter<Edge>() //we'll filter the edges
					{
						public boolean isPass(final Edge edge)
						{
							return edge.getParentNodeIndex() == parentNodeIndex;
						}
					});
		}
	}
}
