/*
 * Copyright © 1996-2013 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.java;

import java.lang.reflect.Method;
import java.util.List;

import static java.util.Arrays.*;
import static java.util.Objects.*;

import com.globalmentor.model.AbstractHashObject;

/**
 * A lightweight encapsulation of a method's signature, without regard to its access restrictions or the class to which it belongs. The signature instance
 * supports {@link #hashCode()} and {@link #equals(Object)}.
 * @author Garret Wilson
 */
public class MethodSignature extends AbstractHashObject {

	private final String name;

	/**
	 * Returns the name of the method.
	 * @return The name of the method.
	 */
	public String getName() {
		return name;
	}

	private final Class<?> returnType;

	/**
	 * Returns the return type of the method.
	 * @return The return type of the method.
	 */
	public Class<?> getReturnType() {
		return returnType;
	}

	private final List<Class<?>> parameterTypes;

	/**
	 * Returns the parameter types of the method.
	 * @return The parameter types of the method.
	 */
	public List<Class<?>> getParameterTypes() {
		return parameterTypes;
	}

	private final boolean returnTypeSignificant;

	/**
	 * Returns whether, for purposes of equality, the return type is significant.
	 * @return <code>true</code> if, for purposes of equality, the return type is significant.
	 */
	public boolean isReturnTypeSignificant() {
		return returnTypeSignificant;
	}

	private MethodSignature(final String name, final Class<?> returnType, final List<Class<?>> parameterTypes, final boolean returnTypeSignificant) {
		this.name = requireNonNull(name);
		this.returnType = returnType;
		this.parameterTypes = requireNonNull(parameterTypes);
		this.returnTypeSignificant = returnTypeSignificant;
	}

	@Override
	public int hashCode() {
		return hash(name, parameterTypes); //don't hash the return type, as some method signature instances won't consider it significant
	}

	@Override
	public boolean equals(final Object object) {
		if(this == object) {
			return true;
		}
		if(!(object instanceof MethodSignature)) {
			return false;
		}
		final MethodSignature methodSignature = (MethodSignature)object;
		//if one or the other method signature feels return type is significant, compare the return type
		return getName().equals(methodSignature.getName()) && ((!isReturnTypeSignificant() && !methodSignature.isReturnTypeSignificant())
				|| java.util.Objects.equals(getReturnType(), methodSignature.getReturnType())) && getParameterTypes().equals(methodSignature.getParameterTypes());
	}

	/**
	 * Creates and returns a method signature from the given method. The return type is considered significant for purposes of equality.
	 * @param method The method for which a signature should be returned.
	 * @return An object representing the signature of the given methods.
	 */
	public static MethodSignature forMethod(final Method method) {
		return forMethod(method, true);
	}

	/**
	 * Creates and returns a method signature from the given method.
	 * @param method The method for which a signature should be returned.
	 * @param returnTypeSignificant Whether, for purposes of equality, the return type is significant.
	 * @return An object representing the signature of the given methods.
	 */
	public static MethodSignature forMethod(final Method method, final boolean returnTypeSignificant) {
		return new MethodSignature(method.getName(), method.getReturnType(), asList(method.getParameterTypes()), returnTypeSignificant);
	}
}
