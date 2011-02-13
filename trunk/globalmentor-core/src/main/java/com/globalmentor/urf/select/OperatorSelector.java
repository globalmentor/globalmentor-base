/*
 * Copyright © 2007 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.urf.select;

/**A selector that works as an operator on the results of other selections.
@author Garret Wilson
*/
public interface OperatorSelector extends Selector
{

	/**@return This operator selector's select declarations.*/
	public Iterable<Selector> getSelectors();

	/**Adds a selector to this operator selector.
	@param selector The selector to add.
	*/
	public void addSelector(final Selector selector);

}