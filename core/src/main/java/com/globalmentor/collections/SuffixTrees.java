/*
 * Copyright © 2011 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

import java.io.PrintStream;

import static java.util.Objects.*;

import com.globalmentor.java.*;
import com.globalmentor.collections.SuffixTree.*;

/**
 * Utilities for working with suffix trees.
 * 
 * @author Garret Wilson
 */
public final class SuffixTrees {

	private SuffixTrees() {
	}

	/**
	 * Prints a character representation of the tree and its branches, starting from the root node.
	 * @param suffixTree The suffix tree to print.
	 * @param printStream The destination to which the tree should be printed.
	 * @throws NullPointerException if the given suffix tree and/or print stream is <code>null</code>.
	 */
	public static void print(final SuffixTree suffixTree, final PrintStream printStream) {
		print(suffixTree, printStream, suffixTree.getRootNode(), 0); //print the root edges at a level of zero
	}

	/**
	 * Prints a character representation of the given node.
	 * @param suffixTree The suffix tree to print.
	 * @param printStream The destination to which the tree should be printed.
	 * @param node The node the child edges to print.
	 * @param level The zero-based level of the tree from the root.
	 * @throws NullPointerException if the given suffix tree, print stream, and/or node is <code>null</code>.
	 */
	protected static void print(final SuffixTree suffixTree, final PrintStream printStream, final Node node, final int level) {
		for(final Edge edge : node.getChildEdges()) { //look at all the child edges
			print(suffixTree, printStream, edge, level); //print each edge at the requested level
		}
	}

	/**
	 * Prints a character representation of the child edges of the given edge.
	 * @param suffixTree The suffix tree to print.
	 * @param printStream The destination to which the tree should be printed.
	 * @param edge The edge the child edges of which to print.
	 * @param level The zero-based level of the tree from the root.
	 * @throws NullPointerException if the given suffix tree and/or print stream and/or edge is <code>null</code>.
	 */
	protected static void print(final SuffixTree suffixTree, final PrintStream printStream, final Edge edge, final int level) {
		printStream.println(Strings.createString('\t', level) + edge.toString()); //indent and print the edge
		print(suffixTree, printStream, edge.getChildNode(), level + 1); //print the edge's child edges at one more level down
	}

	/**
	 * Recursively visits all the nodes in a given suffix tree.
	 * @param <N> The type of node.
	 * @param <E> The type of edge.
	 * @param suffixTree The suffix tree to visit.
	 * @param visitor The visitor to visit the nodes of the suffix tree.
	 * @return <code>true</code> if visiting completed all the nodes.
	 * @throws NullPointerException if the given suffix tree and/or visitor is <code>null</code>.
	 */
	@SuppressWarnings("unchecked")
	public static <N extends Node, E extends Edge> boolean visit(final SuffixTree suffixTree, final Visitor<N, E> visitor) {
		return visit(requireNonNull(suffixTree), (N)suffixTree.getRootNode(), null, 0, requireNonNull(visitor));
	}

	/**
	 * Recursively visits a given node and all descendant nodes in a subtree of a suffix tree.
	 * @param <N> The type of node.
	 * @param <E> The type of edge.
	 * @param suffixTree The suffix tree to visit.
	 * @param node The node being visited.
	 * @param parentEdge The parent edge of the node being visited, or <code>null</code> if the node has no parent.
	 * @param length The length of elements up to the visited node, including the length of the parent edge.
	 * @param visitor The visitor to visit the nodes of the suffix tree.
	 * @return <code>true</code> if visiting completed all the nodes.
	 */
	public static <N extends Node, E extends Edge> boolean visit(final SuffixTree suffixTree, final N node, final E parentEdge, final int length,
			final Visitor<N, E> visitor) {
		if(!visitor.visit(suffixTree, node, parentEdge, length)) { //visit this node, stopping if requested
			return false;
		}
		return visitChildren(suffixTree, node, length, visitor); //visit the children
	}

	/**
	 * Recursively visits all descendant nodes in a subtree of a suffix tree.
	 * @param <N> The type of node.
	 * @param <E> The type of edge.
	 * @param suffixTree The suffix tree to visit.
	 * @param node The node being visited.
	 * @param length The length of elements up to the visited node, including the length of the parent edge.
	 * @param visitor The visitor to visit the nodes of the suffix tree.
	 * @return <code>true</code> if visiting completed all the nodes.
	 */
	@SuppressWarnings("unchecked")
	public static <N extends Node, E extends Edge> boolean visitChildren(final SuffixTree suffixTree, final N node, final int length,
			final Visitor<N, E> visitor) {
		for(final Edge childEdge : node.getChildEdges()) { //iterate the child edges
			if(!visit(suffixTree, (N)childEdge.getChildNode(), (E)childEdge, length + childEdge.getLength(), visitor)) { //visit each child node, stopping if requested
				return false;
			}
		}
		return true;
	}

	/**
	 * A general visitor strategy for visiting nodes. This can be used to implement a pure visitor pattern, in which the destination is aware of and accepts the
	 * visitor, or a strategy visitor pattern in which a third object does the traversal.
	 * 
	 * @author Garret Wilson
	 * 
	 * @param <N> The type of node.
	 * @param <E> The type of edge.
	 */
	public interface Visitor<N extends Node, E extends Edge> {

		/**
		 * Visits the given node.
		 * @param suffixTree The suffix tree being visited.
		 * @param node The node being visited.
		 * @param parentEdge The parent edge of the node being visited, or <code>null</code> if the node has no parent.
		 * @param length The length of elements up to the visited node, including the length of the parent edge.
		 * @return <code>true</code> if visiting should continue to other nodes.
		 */
		public boolean visit(final SuffixTree suffixTree, final N node, final E parentEdge, final int length);
	}
}
