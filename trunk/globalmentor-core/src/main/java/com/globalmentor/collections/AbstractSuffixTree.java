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

import java.util.*;

/**
 * An abstract base implementation of a suffix tree for a sequence of elements (most commonly characters).
 * 
 * @author Garret Wilson
 */
public abstract class AbstractSuffixTree<N extends SuffixTree.Node, E extends SuffixTree.Edge> implements SuffixTree
{

	/** Whether the suffix tree is explicit, with every suffix ending on a leaf node. */
	private final boolean explicit;

	/** @return Whether the suffix tree is explicit, with every suffix ending on a leaf node. */
	public boolean isExplicit()
	{
		return explicit;
	}

	/** The nodes in the suffix tree. */
	private final List<N> nodes = new ArrayList<N>();

	@Override
	public Iterable<N> getNodes()
	{
		return java.util.Collections.unmodifiableList(nodes);
	}

	@Override
	public int getNodeCount()
	{
		return nodes.size();
	}

	@Override
	public N getNode(final int nodeIndex)
	{
		return nodes.get(nodeIndex);
	}

	/**
	 * Creates a new node and adds it to the internal list of nodes. This implementation delegates to {@link #createNode()}.
	 * @return The index of the newly created node.
	 */
	protected final int addNode()
	{
		final int nodeIndex = nodes.size();
		nodes.add(nodeIndex, createNode(nodeIndex));
		return nodeIndex;
	}

	/**
	 * Creates a new node.
	 * @param index The index of the node to create.
	 * @return The index of the newly created node.
	 */
	protected abstract N createNode(final int index);

	/**
	 * Creates a new edge and adds it to the tree. This method delegates to {@link #createEdge(int, int, int, int)}.
	 * @param parentNodeIndex The index of the parent node representing the root end of the edge.
	 * @param childNodeIndex The index of the child node representing the leaf end of the edge.
	 * @param start The position of the start element, inclusive.
	 * @param end The position of the end element, exclusive.
	 * @throws IllegalArgumentException if the given end is less than the start.
	 * @throws IllegalStateException if there already exists an edge with the same parent node and first element.
	 */
	protected final E addEdge(final int parentNodeIndex, final int childNodeIndex, final int start, final int end)
	{
		final E edge = createEdge(parentNodeIndex, childNodeIndex, start, end); //create a new edge
		addEdge(edge); //add the edge
		return edge; //return the edge
	}

	/**
	 * Creates a new edge and adds it to the tree. This method delegates to {@link #addEdge(Edge)}.
	 * @param parentNodeIndex The index of the parent node representing the root end of the edge.
	 * @param childNodeIndex The index of the child node representing the leaf end of the edge.
	 * @param start The position of the start element, inclusive.
	 * @param end The position of the end element, exclusive.
	 * @throws IllegalArgumentException if the given end is less than the start.
	 * @throws IllegalStateException if there already exists an edge with the same parent node and first element.
	 */
	protected abstract E createEdge(final int parentNodeIndex, final int childNodeIndex, final int start, final int end);

	/**
	 * Adds an edge to the tree.
	 * @param edge The edge to add.
	 * @throws NullPointerException if the given edge is <code>null</code>.
	 * @throws IllegalStateException if there already exists an edge with the same parent node and first element.
	 */
	protected abstract void addEdge(final E edge);

	/**
	 * Removes an edge from the tree. If the edge does not exist, no action occurs.
	 * @param edge The edge to remove.
	 * @throws NullPointerException if the given edge is <code>null</code>.
	 */
	protected abstract void removeEdge(final E edge);

	/**
	 * Splits an edge into two. The first, near edge will be of the given length; the second, far edge will be of the remaining length (that is, the length of the
	 * original edge minus the given length). A new node will be created as the mid-point between the original edge nodes, becoming the child node of the first
	 * edge and the parent node of the second edge.
	 * @param edge The edge to split.
	 * @param length The position at which to split the edge.
	 * @return The index of the created node splitting the edge.
	 */
	protected int splitEdge(final E edge, final int length)
	{
		removeEdge(edge); //remove the existing edge from our map, because we're going to create two edges to replace it
		final int newNodeIndex = addNode(); //create a new node with which to split the edge into two
		final int split = edge.getStart() + length; //the element location at which to make the split
		addEdge(edge.getParentNodeIndex(), newNodeIndex, edge.getStart(), split); //the near edge will start at the same place as the old edge, but only go part way---to the new node
		addEdge(newNodeIndex, edge.getChildNodeIndex(), split, edge.getEnd()); //the far edge starts where the new near edge ends
		return newNodeIndex; //return the index of the new node
	}

	/**
	 * Constructor.
	 * @param explicit Whether the suffix tree is explicit, with every suffix ending on a leaf node.
	 */
	protected AbstractSuffixTree(final boolean explicit)
	{
		this.explicit = explicit;
		addNode(); //create the root node
	}

	@Override
	public Iterable<? extends Edge> getRootEdges()
	{
		return getChildEdges(0); //the root node is always the first node in the list
	}

	/**
	 * Represents a node in a suffix tree. Each node defaults to having no suffix node.
	 * 
	 * @author Garret Wilson
	 */
	protected class Node implements SuffixTree.Node
	{

		private final int index;

		@Override
		public int getIndex()
		{
			return index;
		}

		private int suffixNodeIndex = -1;

		@Override
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

		/**
		 * Index constructor.
		 * @param index The index of the node.
		 */
		public Node(final int index)
		{
			this.index = index;
		}
	};

}
