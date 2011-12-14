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

import java.io.PrintStream;

import com.globalmentor.java.*;
import com.globalmentor.collections.SuffixTree.Edge;

/**
 * Utilities for working with suffix trees.
 * 
 * @author Garret Wilson
 */
public class SuffixTrees
{

	/**
	 * Prints a character representation of the tree and its branches, starting from the root node.
	 * @param suffixTree The suffix tree to print.
	 * @param printStream The destination to which the tree should be printed.
	 * @throws NullPointerException if the given suffix tree and/or print stream is <code>null</code>.
	 */
	public static void print(final SuffixTree suffixTree, final PrintStream printStream)
	{
		print(suffixTree, printStream, suffixTree.getRootEdges(), 0); //print the root edges at a level of zero
	}

	/**
	 * Prints a character representation of the child edges of the given edge.
	 * @param suffixTree The suffix tree to print.
	 * @param printStream The destination to which the tree should be printed.
	 * @param edge The edge the child edges of which to print.
	 * @param level The zero-based level of the tree from the root.
	 * @throws NullPointerException if the given suffix tree and/or print stream and/or edge is <code>null</code>.
	 */
	protected static void print(final SuffixTree suffixTree, final PrintStream printStream, final Edge edge, final int level)
	{
		printStream.println(Strings.createString('\t', level) + edge.toString()); //indent and print the edge
		print(suffixTree, printStream, edge.getChildEdges(), level + 1); //print the edge's child edges at one more level down

	}

	/**
	 * Prints a character representation of the given child edges.
	 * @param suffixTree The suffix tree to print.
	 * @param printStream The destination to which the tree should be printed.
	 * @param edges The child edges to print.
	 * @param level The zero-based level of the tree from the root.
	 * @throws NullPointerException if the given suffix tree and/or print stream and/or edges is <code>null</code>.
	 */
	protected static void print(final SuffixTree suffixTree, final PrintStream printStream, final Iterable<? extends Edge> edges, final int level)
	{
		for(final Edge edge : edges) //look at all the given edges
		{
			print(suffixTree, printStream, edge, level); //print each edge at the requested level
		}
	}

//	public static void walk(final SuffixTree)
}
