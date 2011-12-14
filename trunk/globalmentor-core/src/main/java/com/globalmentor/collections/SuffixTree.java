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

/**
 * A general interface for a suffix tree for a sequence of elements (most commonly characters).
 * 
 * @author Garret Wilson
 */
public interface SuffixTree
{

	/** @return Whether the suffix tree is explicit, with every suffix ending on a leaf node. */
	public boolean isExplicit();

	/** @return A read-only iterable of the nodes in the tree. */
	public Iterable<? extends Node> getNodes();

	/** @return The number of nodes in the suffix tree. */
	public int getNodeCount();

	/**
	 * Retrieves the root node of the tree. This is a convenience method to retrieve the node with index zero.
	 * @param nodeIndex The index of the node to retrieve.
	 * @return The identified node.
	 */
	public Node getRootNode();

	/**
	 * Retrieves the identified node.
	 * @param nodeIndex The index of the node to retrieve.
	 * @return The identified node.
	 * @throws IndexOutOfBoundsException if the given node index does not identify a node in this suffix tree.
	 */
	public Node getNode(final int nodeIndex);

	/** @return A read-only iterable of edges in the tree. */
	public Iterable<? extends Edge> getEdges();

	/**
	 * Represents a node in a suffix tree. Each node defaults to having no suffix node.
	 * 
	 * @author Garret Wilson
	 */
	public interface Node
	{
		/** @return The index of the node. */
		public int getIndex();

		/** @return Whether this node is a leaf node in the suffix tree. */
		public boolean isLeaf();
		
		/**@return The parent node of this node, or <code>null</code> if this node has no parent node (i.e. it is the root node).*/
		public Node getParentNode();

		/** @return The node representing the next smaller suffix, or <code>null</code> if there is no known smaller suffix node. */
		public Node getSuffixNode();

		/**@return An iterable to the child edges of this node.*/
		public Iterable<? extends Edge> getChildEdges();

	};

	/**
	 * Represents an edge between a parent node and a child node in a suffix tree. Some edges may be empty.
	 * 
	 * @author Garret Wilson
	 */
	public interface Edge
	{

		/** @return The parent node representing the root end of the edge. */
		public Node getParentNode();

		/** @return The child node representing the leaf end of the edge. */
		public Node getChildNode();

		/** @return The position of the start element, inclusive. */
		public int getStart();

		/** @return The position of the last element, exclusive. */
		public int getEnd();

		/**
		 * Returns the length of the edge, i.e. <code>end</code>-<code>start</code>.
		 * @return The number of elements on the edge.
		 */
		public int getLength();

		/** @return <code>true</code> if this edge is empty and has no elements. */
		public boolean isEmpty();

		/** @return An iterable to the child edges of this edge's child node. */
		public Iterable<? extends Edge> getChildEdges();

	};

}
