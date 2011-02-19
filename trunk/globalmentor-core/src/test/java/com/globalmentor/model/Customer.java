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

package com.globalmentor.model;

import java.util.*;

import com.globalmentor.java.Objects;

/**
 * Test model class for a customer.
 * 
 * @author Garret Wilson
 * 
 */
public class Customer
{

	private String name;

	public String getName()
	{
		return name;
	}

	public void setName(final String name)
	{
		this.name = Objects.checkInstance(name);
	}

	private Set<String> aliases = new HashSet<String>();

	public Set<String> getAliases()
	{
		return aliases;
	}

	public void setAliases(final Set<String> aliases)
	{
		this.aliases = Objects.checkInstance(aliases);
	}

	private int age;

	public int getAge()
	{
		return age;
	}

	public void setAge(final int age)
	{
		this.age = age;
	}
}
