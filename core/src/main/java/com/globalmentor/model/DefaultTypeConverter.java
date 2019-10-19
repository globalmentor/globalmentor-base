/*
 * Copyright © 2007-2013 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.util.regex.Pattern;

import com.globalmentor.net.URIPath;

/**
 * A default implementation of a type converter.
 * 
 * <p>
 * The following types can be converted to the listed types:
 * </p>
 * <dl>
 * <dt>{@link Boolean}</dt>
 * <dd><code>boolean</code></dd>
 * <dt>{@link Character}</dt>
 * <dd><code>char</code></dd>
 * <dt>{@link Number}</dt>
 * <dd>{@link Long}, <code>long</code>, {@link Integer}, <code>int</code>, {@link Double}, <code>double</code>, {@link Float}, <code>float</code></dd>
 * <dt>{@link String}</dt>
 * <dd><code>char[]</code>, {@link Enum}, {@link Pattern}, {@link URIPath}</dd>
 * </dl>
 * 
 * @author Garret Wilson
 */
public class DefaultTypeConverter implements TypeConverter {

	/** The default, shared instance of the converter. */
	public static final DefaultTypeConverter INSTANCE = new DefaultTypeConverter();

	/** Default constructor; used only for the singleton instance and for extension. */
	protected DefaultTypeConverter() {
	}

	@Override
	@SuppressWarnings("unchecked")
	public <T> T convert(final Object object, final Class<T> requiredType) throws IllegalArgumentException { //TODO search for a string constructor or a static valueOf() method
		final Class<?> objectType = object.getClass(); //get the type of the object
		if(requiredType.isAssignableFrom(objectType)) { //if we expect this type (this algorithm could be improved to first try to find an exact match and then find a convertible match)
			return requiredType.cast(object); //use the object as-is
		} else { //if we expect another object type
			if(object instanceof Boolean) { //if the object is a Boolean
				if(boolean.class.isAssignableFrom(requiredType)) { //if the required type is boolean (we already checked for Boolean when we checked to see if the types were the same)
					return (T)object; //return the Boolean object
				}
			} else if(object instanceof Character) { //if the object is a Character
				if(char.class.isAssignableFrom(requiredType)) { //if the required type is char (we already checked for Character when we checked to see if the types were the same)
					return (T)object; //return the Character object
				}
			} else if(object instanceof Number) { //if the object is a Number
				if(long.class.isAssignableFrom(requiredType) || Long.class.isAssignableFrom(requiredType)) { //if the required type is long or Long 
					return (T)(object instanceof Long ? object : Long.valueOf(((Number)object).longValue())); //return a Long version of the object
				} else if(int.class.isAssignableFrom(requiredType) || Integer.class.isAssignableFrom(requiredType)) { //if the required type is integer or integer 
					return (T)(object instanceof Integer ? object : Integer.valueOf(((Number)object).intValue())); //return an Integer version of the object
				} else if(double.class.isAssignableFrom(requiredType) || Double.class.isAssignableFrom(requiredType)) { //if the required type is double or Double
					return (T)(object instanceof Double ? object : Double.valueOf(((Number)object).doubleValue())); //return a Double version of the object
				} else if(float.class.isAssignableFrom(requiredType) || Float.class.isAssignableFrom(requiredType)) { //if the required type is float or Float
					return (T)(object instanceof Double ? object : Float.valueOf(((Number)object).floatValue())); //return a Float version of the object
				}
				//TODO add BigInteger and BigDecimal types
			} else if(object instanceof String) { //if the object is a string, see if we can convert it to the correct type
				final String stringObject = (String)object; //cast the value to a String
				if(requiredType.isArray() && char.class.isAssignableFrom(requiredType.getComponentType())) { //if the required type is a character array
					return (T)stringObject.toCharArray(); //return the string as a character array
				} else if(Pattern.class.isAssignableFrom(requiredType)) { //if the required type is Pattern
					return (T)Pattern.compile(stringObject); //compile a pattern from the string
				} else if(URIPath.class.isAssignableFrom(requiredType)) { //if the required type is URIPath
					return (T)URIPath.of(stringObject); //create a URI path from the string
				}
			}
		}
		return null; //indicate we couldn't get an object of the correct type
	}

}
