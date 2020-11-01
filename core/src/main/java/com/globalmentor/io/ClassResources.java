/*
 * Copyright © 1996-2020 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.io;

import static java.nio.file.Files.*;

import java.io.*;
import java.net.URL;
import java.nio.file.*;
import java.util.*;

import javax.annotation.*;

import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Java.*;

/**
 * Utilities for accessing Java class resources loaded from the classpath.
 * @author Garret Wilson
 */
public final class ClassResources {

	/** The slash character (<code>'/'</code>) which separates components in a resource path. */
	public static final char PATH_SEPARATOR = '/';

	/** This class cannot be publicly instantiated. */
	private ClassResources() {
	}

	//## resource paths

	/**
	 * Determines the base path necessary to access a named resource using the class loader of the given context class.
	 * @param contextClass The class in relation to which the resource name should be resolved.
	 * @return The full relative base path, ending with a path separator, necessary to access resources using the resource loader of the given class.
	 * @see #resolveResourcePath(Class, String)
	 * @see ClassLoader#getResource(String)
	 * @see ClassLoader#getResourceAsStream(String)
	 */
	public static String getResourceBasePath(@Nonnull final Class<?> contextClass) {
		return contextClass.getPackage().getName().replace(PACKAGE_SEPARATOR, PATH_SEPARATOR) + PATH_SEPARATOR;
	}

	/**
	 * Determines the path necessary to access a named resource using the class loader of the given context class.
	 * <p>
	 * Accessing a resource via e.g. {@link Class#getResource(String)} for the class <code>com.example.Foo</code> may be accomplished using a resource name such
	 * as <code>"bar"</code>, relative to the class package directory structure; but loading the same resource via {@link ClassLoader#getResource(String)} using
	 * the class loader for the same class requires the full path to the resource, such as <code>com/example/bar</code>. This method determines the full path that
	 * would need to be used to access a resource using a class loader for a class. Thus given class <code>com.example.Foo</code> and resource name
	 * <code>bar</code>, this method will return <code>"com/example/bar"</code>.
	 * </p>
	 * <p>
	 * This method performs functionality equivalent to that performed internally to methods such as {@link Class#getResource(String)} before they delegate to the
	 * class loader.
	 * </p>
	 * @param contextClass The class in relation to which the resource name should be resolved
	 * @param resourcePath The relative path of the resource to access.
	 * @return The full relative path of the resource necessary to access it using the resource loader of the given class.
	 * @see ClassLoader#getResource(String)
	 * @see ClassLoader#getResourceAsStream(String)
	 */
	public static String resolveResourcePath(@Nonnull final Class<?> contextClass, @Nonnull final String resourcePath) {
		return getResourceBasePath(contextClass) + resourcePath;
	}

	/**
	 * Retrieves the filename for a resource given its path. The filename is guaranteed never to be the empty string.
	 * @param resourcePath The path to the resource.
	 * @return The filename of the resource, or {@link Optional#empty()} if the path ends with a separator.
	 * @throws IllegalArgumentException if the given resource path is empty.
	 */
	public static Optional<String> findResourceName(@Nonnull final String resourcePath) {
		checkArgument(!resourcePath.isEmpty(), "An empty resource path is not accepted.");
		final int lastPathSeparatorIndex = resourcePath.lastIndexOf(PATH_SEPARATOR);
		if(lastPathSeparatorIndex < 0) { //if there is no path separator, the whole path is the filename
			return Optional.of(resourcePath);
		}
		if(lastPathSeparatorIndex == resourcePath.length() - 1) { //if the resource path ends with a slash
			return Optional.empty();
		}
		return Optional.of(resourcePath.substring(lastPathSeparatorIndex + 1)); //return everything after the last path separator
	}

	//## resource contents

	/**
	 * Copies all the bytes of a bytes from a class resource to a file in a file system.
	 * @implSpec This method delegates to {@link #copy(ClassLoader, String, Path, CopyOption...)}.
	 * @param contextClass The class the class loader of which to use for retrieving the resource.
	 * @param resourcePath The path of the resource <em>relative to the context class</em>.
	 * @param targetFile The path to the destination to where the resource should be copied. Any target parent directories will be created as needed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The number of bytes copied.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copy(@Nonnull final Class<?> contextClass, @Nonnull final String resourcePath, @Nonnull final Path targetFile,
			final CopyOption... options) throws IOException {
		return copy(contextClass.getClassLoader(), resolveResourcePath(contextClass, resourcePath), targetFile, options);
	}

	/**
	 * Copies all the bytes of a bytes from a class resource to a file in a file system.
	 * @apiNote This method requires the full classpath-relative path to the resource, unlike {@link #copy(Class, String, Path, CopyOption...)}, which
	 *          requires a path relative to a class.
	 * @implSpec This method delegates to {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @param classLoader The class loader to use for retrieving the resource.
	 * @param resourcePath The full relative path of the resource in relation to the class loader.
	 * @param targetFile The path to the destination to where the resource should be copied. Any target parent directories will be created as needed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The number of bytes copied.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copy(@Nonnull final ClassLoader classLoader, @Nonnull final String resourcePath, @Nonnull final Path targetFile,
			final CopyOption... options) throws IOException {
		final URL resourceUrl = classLoader.getResource(resourcePath); //getResourceAsStream() does this same thing, and this allows us not to throw away an IOException
		if(resourceUrl == null) {
			throw new FileNotFoundException("Resource not found: " + resourcePath);
		}
		final Path targetParent = targetFile.getParent();
		if(targetParent != null) {
			createDirectories(targetParent);
		}
		try (final InputStream resourceInputStream = resourceUrl.openStream()) {
			return java.nio.file.Files.copy(resourceInputStream, targetFile, options);
		}
	}

}
