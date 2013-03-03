/*
 * Copyright © 2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.java;

/* "enum generics not recognized; breaks compiler; works in Eclipse"

I have "public interface MyInterface<T extends Enum<T> & OtherInterface>" as the generic type of an interface, and one of the methods of the interface returns type T, which is obviously an enum. However, a separate static method that takes an enum as an argument will not recognize the returned type T as an enum.

See attached test case.

The attached test case fails to compile both in JDK 6 and JDK 7. It compiles fine in Eclipse 4.2.1.

BugEnum.java:24: error: method getSerializationName in class BugEnum cannot be applied to given types;
              System.out.println(getSerializationName(testMessage.getCommand()));
                                 ^
required: E
found: INT#1
reason: inferred type does not conform to declared bound(s)
  inferred: INT#1
  bound(s): Enum<INT#1>
where E is a type-variable:
  E extends Enum<E> declared in method <E>getSerializationName(E)
where CAP#1 is a fresh type-variable:
  CAP#1 extends INT#1 from capture of ?
where INT#1 is an intersection type:
  INT#1 extends Enum<CAP#1>,Command
1 error	
*/

/**
 * Illustration of Java Enum-related compile bug, reproduced in JDK 1.7.0_09-b05.
 * @author Garret Wilson
 */
public class BugEnum
{

	public interface Command
	{
	}

	public interface CommandMessage<C extends Enum<C> & Command>
	{
		public C getCommand();
	}

	public enum TestCommand implements Command
	{
		SUCCESS, FAILURE
	}

	public static class TestCommandMessage implements CommandMessage<TestCommand>
	{
		@Override
		public TestCommand getCommand()
		{
			return TestCommand.SUCCESS;
		}
	}

	public static <E extends Enum<E>> String getSerializationName(final E e)
	{
		return e.name().toString().toLowerCase();
	}

	public static void main(String... args)
	{
		@SuppressWarnings("unused")
		final CommandMessage<?> testMessage = new TestCommandMessage();
		//bug:		System.out.println(getSerializationName(testMessage.getCommand()));
	}

}