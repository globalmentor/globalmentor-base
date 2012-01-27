/*
 * Copyright © 2012 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.model;

/**
 * Represents a visitor that can visit each node during traversal (e.g. visiting every node in a tree).
 * 
 * @author Garret Wilson
 * 
 * @param <N> The type of node being visited.
 */
public interface Visitor<N>
{

	/**
	 * Visits the given node.
	 * @param node The node being visited.
	 * @return <code>true</code> if traversal should continue to other nodes.
	 */
	public boolean visit(final N node);

}
