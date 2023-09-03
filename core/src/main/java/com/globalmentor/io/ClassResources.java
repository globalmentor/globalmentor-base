/*
 * Copyright © 1996-2020 GlobalMentor, Inc. <https://www.globalmentor.com/>
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

package com.globalmentor.io;

import static java.nio.file.Files.*;
import static java.util.Arrays.*;
import static java.util.Collections.*;

import java.io.*;
import java.net.URL;
import java.nio.file.*;
import java.util.*;

import javax.annotation.*;

import static com.globalmentor.io.Paths.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Conditions.*;
import static com.globalmentor.java.Java.*;

/**
 * Utilities for accessing Java class resources loaded from the classpath.
 * @author Garret Wilson
 */
public final class ClassResources {

	/** The slash character (<code>'/'</code>) which separates components in a resource path. */
	public static final char PATH_SEPARATOR = '/';

	/**
	 * The resource path separator as a string.
	 * @see #PATH_SEPARATOR
	 */
	static final String PATH_SEPARATOR_STRING = String.valueOf(PATH_SEPARATOR);

	/** This class cannot be publicly instantiated. */
	private ClassResources() {
	}

	//## resource paths

	/**
	 * Determines whether a resource path is absolute.
	 * @param resourcePath The path to the class resource.
	 * @return <code>true</code> if the resource path begins with {@value #PATH_SEPARATOR}.
	 * @see #isPathRelative(String)
	 */
	public static boolean isPathAbsolute(@Nonnull final String resourcePath) {
		return startsWith(resourcePath, PATH_SEPARATOR);
	}

	/**
	 * Checks to see if a resource path path is absolute. If the resource path is not absolute, an exception is thrown.
	 * @param resourcePath The path to the class resource.
	 * @return The given resource path.
	 * @throws IllegalArgumentException if the resource path is not absolute.
	 * @see #isPathAbsolute(String)
	 */
	public static String checkArgumentPathAbsolute(@Nonnull final String resourcePath) throws IllegalArgumentException {
		checkArgument(isPathAbsolute(resourcePath), "The resource path %s is not absolute.", resourcePath);
		return resourcePath;
	}

	/**
	 * Determines whether a resource path is relative.
	 * @param resourcePath The path to the class resource.
	 * @return <code>true</code> if the resource path does not begin with {@value #PATH_SEPARATOR}.
	 * @see #isPathAbsolute(String)
	 */
	public static boolean isPathRelative(@Nonnull final String resourcePath) {
		return !isPathAbsolute(resourcePath);
	}

	/**
	 * Checks to see if a resource path is relative. If the path is not relative, an exception is thrown.
	 * @param resourcePath The path to the class resource.
	 * @return The given resource path.
	 * @throws IllegalArgumentException if the resource path is absolute.
	 * @see #isPathRelative(String)
	 */
	public static String checkArgumentPathRelative(@Nonnull final String resourcePath) throws IllegalArgumentException {
		checkArgument(isPathRelative(resourcePath), "The path %s is not relative.", resourcePath);
		return resourcePath;
	}

	/**
	 * Returns the segments (the characters appearing between {@value #PATH_SEPARATOR} characters) in the given resource path. This method does not take into
	 * consideration whether the resource path is relative or absolute; or whether it ends with a path separator. Thus <code>com/example/foo/bar</code>,
	 * <code>/com/example/foo/bar</code>, <code>com/example/foo/bar/</code>, and <code>com/example/foo/bar/</code> will all return the segments
	 * <code>"com"</code>, <code>"example"</code>, <code>"foo"</code>, and <code>"bar"</code>. An empty string will result in an empty list.
	 * @apiNote Because this method for the most part does not consider whether the path is relative or absolute, if this is relevant it must be checked by the
	 *          caller.
	 * @param resourcePath The resource path to divide into segments.
	 * @return The segments of the given resource path.
	 * @throws IllegalArgumentException if there are more than one segment and a segment is empty, indicating subsequent path separators.
	 * @see #PATH_SEPARATOR
	 */
	public static List<String> getPathSegments(@Nonnull final String resourcePath) {
		if(resourcePath.isEmpty()) { //empty string
			return emptyList();
		}
		checkArgument(!resourcePath.equals(PATH_SEPARATOR_STRING), "Cannot get path segments of resource path %s consisting solely of a path separator.");
		//check for beginning and ending slashes
		int beginIndex = 0;
		if(resourcePath.charAt(beginIndex) == PATH_SEPARATOR) {
			beginIndex++; //we know the string must have at least two characters or it would have matched a single separator above
			checkArgument(resourcePath.charAt(beginIndex) != PATH_SEPARATOR, "Cannot get path segments of resource path %s which begins with two path separators.");
		}
		final int length = resourcePath.length();
		int endIndex = length;
		if(endIndex - 1 > beginIndex) { //if we haven't checked the end yet
			if(resourcePath.charAt(endIndex - 1) == PATH_SEPARATOR) {
				endIndex--;
				if(endIndex - 1 > beginIndex) {
					checkArgument(resourcePath.charAt(endIndex - 1) != PATH_SEPARATOR,
							"Cannot get path segments of resource path %s which ends with two path separators.");
				}
			}
		}
		if(beginIndex > 0 || endIndex < length) {
			return getPathSegments(resourcePath.substring(beginIndex, endIndex)); //get the path segments for the relative form of the path
		}
		final List<String> segments = asList(resourcePath.split(PATH_SEPARATOR_STRING, -1)); //returned list doesn't need to be immutable
		checkArgument(segments.size() < 2 || !segments.stream().anyMatch(String::isEmpty), "Empty path segments not allowed in resource path %s.", resourcePath);
		return segments;
	}

	/**
	 * Determines the base path necessary to access a named resource using the class loader of the given context class.
	 * @param contextClass The class in relation to which the resource name should be resolved.
	 * @return The full <em>relative</em> base path, ending with a path separator, necessary to access resources using the resource loader of the given class.
	 * @see #getClassLoaderResourcePath(Class, String)
	 * @see ClassLoader#getResource(String)
	 * @see ClassLoader#getResourceAsStream(String)
	 */
	public static String getClassLoaderResourceBasePath(@Nonnull final Class<?> contextClass) {
		return contextClass.getPackage().getName().replace(PACKAGE_SEPARATOR, PATH_SEPARATOR) + PATH_SEPARATOR;
	}

	/**
	 * Determines the path necessary to access a named resource using the class loader of the given context class.
	 * <p>
	 * Accessing a resource via e.g. {@link Class#getResource(String)} for the class <code>com.example.Foo</code> may be accomplished using a resource name such
	 * as <code>"bar"</code>, relative to the class package directory structure; but loading the same resource via {@link ClassLoader#getResource(String)} using
	 * the class loader for the same class requires the full path to the resource, such as <code>com/example/bar</code>. This method determines the full path that
	 * would need to be used to access a resource using a class loader for a class. Thus given class <code>com.example.Foo</code> and resource name
	 * <code>bar</code>, this method will return <code>"com/example/bar"</code>. But if the absolute path <code>/bar</code> is passed, <code>bar</code> will be
	 * returned.
	 * </p>
	 * <p>
	 * This method performs functionality equivalent to that performed internally to methods such as {@link Class#getResource(String)} before they delegate to the
	 * class loader.
	 * </p>
	 * @param contextClass The class in relation to which the resource name should be resolved
	 * @param resourcePath The path of the resource to access, relative to the context class; or an absolute path that will be made relative to the class loader.
	 * @return The full <em>relative</em> path of the resource necessary to access it using the resource loader of the given class.
	 * @see ClassLoader#getResource(String)
	 * @see ClassLoader#getResourceAsStream(String)
	 * @throws IllegalArgumentException if the given resource path begins with two path separators (i.e. two forward slashes).
	 */
	public static String getClassLoaderResourcePath(@Nonnull final Class<?> contextClass, @Nonnull final String resourcePath) {
		if(isPathAbsolute(resourcePath)) {
			if(resourcePath.length() > 1) { //prevent returning an absolute path
				checkArgument(resourcePath.charAt(1) != PATH_SEPARATOR, "Resource path %s must not begin with two path separators.");
			}
			return resourcePath.substring(1);
		}
		return getClassLoaderResourceBasePath(contextClass) + resourcePath;
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

	//## resources

	/**
	 * Finds a resource with a given name.
	 * @apiNote This method is equivalent to {@link Class#getResource(String)} except that it returns an {@link Optional} value instead of a nullable value.
	 * @param contextClass The class in relation to which to find the resource.
	 * @param resourceName The name of the desired resource.
	 * @return A URL to the resource, which will not be present if no resource with this name is found, the resource cannot be located by a URL, the resource is
	 *         in a package that is not open to at least the caller module, or access to the resource is denied by the security manager.
	 * @see Class#getResource(String)
	 */
	public static Optional<URL> findResource(@Nonnull final Class<?> contextClass, @Nonnull final String resourceName) {
		return Optional.ofNullable(contextClass.getResource(resourceName));
	}

	/**
	 * Finds the resource with the given name using a class loader.
	 * @apiNote This method is equivalent to {@link ClassLoader#getResource(String)} except that it returns an {@link Optional} value instead of a nullable value.
	 * @param classLoader The class loader to use to find the resource.
	 * @param resourceName The name of the desired resource.
	 * @return A URL to the resource, which will not be present if the resource could not be found, a URL could not be constructed to locate the resource, the
	 *         resource is in a package that is not opened unconditionally, or access to the resource is denied by the security manager.
	 * @see ClassLoader#getResource(String)
	 */
	public static Optional<URL> findResource(@Nonnull final ClassLoader classLoader, @Nonnull final String resourceName) {
		return Optional.ofNullable(classLoader.getResource(resourceName));
	}

	/**
	 * Finds a resource of the specified name from the search path used to load classes.
	 * @apiNote This method is equivalent to {@link ClassLoader#getSystemResource(String)} except that it returns an {@link Optional} value instead of a nullable
	 *          value.
	 * @param resourceName The name of the desired resource.
	 * @return A URL to the resource, which will not be present if the resource could not be found, a URL could not be constructed to locate the resource, the
	 *         resource is in a package that is not opened unconditionally or access to the resource is denied by the security manager.
	 * @see ClassLoader#getSystemResource(String)
	 */
	public static Optional<URL> findSystemResource(@Nonnull final String resourceName) {
		return Optional.ofNullable(ClassLoader.getSystemResource(resourceName));
	}

	/**
	 * Finds a resource with a given name.
	 * @apiNote This method is equivalent to calling {@link Class#getResource(String)} and then opening a stream to the URL, except that it returns an
	 *          {@link Optional} value instead of a nullable value. This method is similar to {@link Class#getResourceAsStream(String)} except that any
	 *          {@link IOException} when opening the stream is throw instead of being discarded.
	 * @param contextClass The class in relation to which to find the resource.
	 * @param resourceName The name of the desired resource.
	 * @return An input stream to the resource, which will not be present if no resource with this name is found, the resource is in a package that is not open to
	 *         at least the caller module, or access to the resource is denied by the security manager.
	 * @throws IOException if there is an error opening the stream.
	 * @see Class#getResource(String)
	 * @see Class#getResourceAsStream(String)
	 */
	public static Optional<InputStream> findResourceAsStream(@Nonnull final Class<?> contextClass, @Nonnull final String resourceName) throws IOException {
		final URL resourceUrl = contextClass.getResource(resourceName);
		return Optional.ofNullable(resourceUrl != null ? resourceUrl.openStream() : null);
	}

	/**
	 * Returns an input stream for reading the specified resource using a class loader.
	 * @apiNote This method is equivalent to calling {@link ClassLoader#getResource(String)} and then opening a stream to the URL, except that it returns an
	 *          {@link Optional} value instead of a nullable value.
	 * @apiNote This method is similar to {@link ClassLoader#getResourceAsStream(String)} except that any {@link IOException} when opening the stream is throw
	 *          instead of being discarded.
	 * @param classLoader The class loader to use to find the resource.
	 * @param resourceName The name of the desired resource.
	 * @return An input stream for reading the resource, which will not be present if the resource could not be found, the resource is in a package that is not
	 *         opened unconditionally, or access to the resource is denied by the security manager.
	 * @throws IOException if there is an error opening the stream.
	 * @see ClassLoader#getResource(String)
	 * @see ClassLoader#getResourceAsStream(String)
	 */
	public static Optional<InputStream> findResourceAsStream(@Nonnull final ClassLoader classLoader, @Nonnull final String resourceName) throws IOException {
		final URL resourceUrl = classLoader.getResource(resourceName);
		return Optional.ofNullable(resourceUrl != null ? resourceUrl.openStream() : null);
	}

	/**
	 * Open for reading a resource of the specified name from the search path used to load classes.
	 * @apiNote This method is equivalent to calling {@link ClassLoader#getSystemResource(String)} and then opening a stream to the URL, except that it returns an
	 *          {@link Optional} value instead of a nullable value.
	 * @apiNote This method is similar to {@link ClassLoader#getSystemResourceAsStream(String)} except that any {@link IOException} when opening the stream is
	 *          throw instead of being discarded.
	 * @param resourceName The name of the desired resource.
	 * @return An input stream for reading the resource, which will not be present if the resource could not be found, the resource is in a package that is not
	 *         opened unconditionally, or access to the resource is denied by the security manager.
	 * @throws IOException if there is an error opening the stream.
	 * @see ClassLoader#getSystemResource(String)
	 * @see ClassLoader#getSystemResourceAsStream(String)
	 */
	public static Optional<InputStream> findSystemResourceAsStream(@Nonnull final String resourceName) throws IOException {
		final URL resourceUrl = ClassLoader.getSystemResource(resourceName);
		return Optional.ofNullable(resourceUrl != null ? resourceUrl.openStream() : null);
	}

	//## resource contents

	/**
	 * Copies several class resource to a base directory in a file system, maintaining the relative directory hierarchy.
	 * @apiNote This is a convenience method for passing multiple resource paths as varargs. It provides no facility to indicate copy options, so if the operation
	 *          needs to specify an option, to replace existing target files using {@link StandardCopyOption#REPLACE_EXISTING} for example,
	 *          {@link #copy(Class, Path, Iterable, CopyOption...)} should be used instead.
	 * @implSpec This method delegates to {@link #copy(Class, Path, Iterable, CopyOption...)}.
	 * @param contextClass The class the class loader of which to use for retrieving the resources.
	 * @param targetBaseDirectory The base directory of the destination to where the resources should be copied. Any target parent directories will be created as
	 *          needed.
	 * @param resourcePaths The paths of the resources, each relative to the context class; or each an absolute path that will be made relative to the class
	 *          loader. The paths must not end in {@value #PATH_SEPARATOR}, and an empty path is not allowed.
	 * @return The total number of bytes copied.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copy(@Nonnull final Class<?> contextClass, @Nonnull final Path targetBaseDirectory, @Nonnull String... resourcePaths) throws IOException {
		return copy(contextClass, targetBaseDirectory, asList(resourcePaths));
	}

	/**
	 * Copies several class resources to a base directory in a file system, maintaining the relative directory hierarchy.
	 * <p>
	 * For example copying the files <code>example.txt</code> and <code>foo/bar.txt</code> in the context of class <code>com.example.Test</code> to the target
	 * base directory <code>/path/to/dest</code> would copy the files to <code>/path/to/dest/example.txt</code> and <code>/path/to/dest/foo/bar.txt</code>.
	 * </p>
	 * @apiNote This operation is not atomic; it may fail having copied only some of the resources.
	 * @implSpec This method delegates to {@link #copy(Class, String, Path, CopyOption...)} for each resource to copy.
	 * @param contextClass The class the class loader of which to use for retrieving the resources.
	 * @param targetBaseDirectory The base directory of the destination to where the resources should be copied. Any target parent directories will be created as
	 *          needed.
	 * @param resourcePaths The paths of the resources, each relative to the context class; or each an absolute path that will be made relative to the class
	 *          loader. The paths must not end in {@value #PATH_SEPARATOR}, and an empty path is not allowed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The total number of bytes copied.
	 * @throws FileNotFoundException if the indicated resource cannot be found.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copy(@Nonnull final Class<?> contextClass, @Nonnull final Path targetBaseDirectory, @Nonnull Iterable<String> resourcePaths,
			final CopyOption... options) throws IOException {
		long totalByteCount = 0;
		for(final String resourcePath : resourcePaths) {
			checkArgument(!resourcePath.isEmpty(), "Cannot copy empty resource path.");
			checkArgument(!endsWith(resourcePath, PATH_SEPARATOR), "Cannot copy resource path %s because it ends with a %s.", resourcePath, PATH_SEPARATOR_STRING);
			final List<String> pathSegments = getPathSegments(resourcePath);
			assert !pathSegments.isEmpty() : "Checked for empty path; expected at least one path segment.";
			final Path targetFile = resolve(targetBaseDirectory, pathSegments); //resolve the path segments against the base directory to get the target file
			totalByteCount += copy(contextClass.getClassLoader(), getClassLoaderResourcePath(contextClass, resourcePath), targetFile, options);
		}
		return totalByteCount;
	}

	/**
	 * Copies all the bytes of a class resource to a file in a file system.
	 * @implSpec This method delegates to {@link #copy(ClassLoader, String, Path, CopyOption...)}.
	 * @param contextClass The class the class loader of which to use for retrieving the resource.
	 * @param resourcePath The path of the resource to access, relative to the context class; or an absolute path that will be made relative to the class loader.
	 * @param targetFile The path to the destination to where the resource should be copied. Any target parent directories will be created as needed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The number of bytes copied.
	 * @throws FileNotFoundException if the indicated resource cannot be found.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copy(@Nonnull final Class<?> contextClass, @Nonnull final String resourcePath, @Nonnull final Path targetFile, final CopyOption... options)
			throws IOException {
		return copy(contextClass.getClassLoader(), getClassLoaderResourcePath(contextClass, resourcePath), targetFile, options);
	}

	/**
	 * Copies all the bytes of a class resource to a file in a file system.
	 * @apiNote This method requires the full classpath-relative path to the resource, unlike {@link #copy(Class, String, Path, CopyOption...)}, which requires a
	 *          path relative to a class.
	 * @implSpec This method delegates to {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @param classLoader The class loader to use for retrieving the resource.
	 * @param resourcePath The full relative path of the resource in relation to the class loader.
	 * @param targetFile The path to the destination to where the resource should be copied. Any target parent directories will be created as needed.
	 * @param options Options specifying how the copy should be performed.
	 * @return The number of bytes copied.
	 * @throws FileNotFoundException if the indicated resource cannot be found.
	 * @throws IOException if an I/O error occurs when reading or writing. The exception may be a subclass of {@link FileSystemException} as per
	 *           {@link java.nio.file.Files#copy(InputStream, Path, CopyOption...)}.
	 * @throws UnsupportedOperationException if {@code options} contains a copy option that is not supported.
	 * @throws SecurityException If the security manager does not permit the operation.
	 */
	public static long copy(@Nonnull final ClassLoader classLoader, @Nonnull final String resourcePath, @Nonnull final Path targetFile,
			final CopyOption... options) throws IOException {
		//getResourceAsStream() does this same thing, but opening the stream manually prevents discarding any IOException during opening
		final URL resourceUrl = findResource(classLoader, resourcePath).orElseThrow(() -> new FileNotFoundException("Resource not found: " + resourcePath));
		final Path targetParent = targetFile.getParent();
		if(targetParent != null) {
			createDirectories(targetParent);
		}
		try (final InputStream resourceInputStream = resourceUrl.openStream()) {
			return java.nio.file.Files.copy(resourceInputStream, targetFile, options);
		}
	}

	/**
	 * Reads all the bytes of a class resource.
	 * @apiNote This method is analogous to the {@link java.nio.file.Files#readAllBytes(Path)} utility for paths.
	 * @implSpec This method delegates to {@link #readBytes(ClassLoader, String)}.
	 * @param contextClass The class the class loader of which to use for retrieving the resource.
	 * @param resourcePath The path of the resource to access, relative to the context class; or an absolute path that will be made relative to the class loader.
	 * @return An array of all the bytes read from the resource.
	 * @throws FileNotFoundException if the indicated resource cannot be found.
	 * @throws IOException if there is an error reading the bytes.
	 * @see InputStreams#readBytes(InputStream)
	 */
	public static byte[] readBytes(@Nonnull final Class<?> contextClass, @Nonnull final String resourcePath) throws IOException {
		return readBytes(contextClass.getClassLoader(), getClassLoaderResourcePath(contextClass, resourcePath));
	}

	/**
	 * Reads all the bytes of a class resource.
	 * @apiNote This method requires the full classpath-relative path to the resource, unlike {@link #readBytes(Class, String)}, which requires a path relative to
	 *          a class.
	 * @apiNote This method is analogous to the {@link java.nio.file.Files#readAllBytes(Path)} utility for paths.
	 * @param classLoader The class loader to use for retrieving the resource.
	 * @param resourcePath The full relative path of the resource in relation to the class loader.
	 * @return An array of all the bytes read from the resource.
	 * @throws FileNotFoundException if the indicated resource cannot be found.
	 * @throws IOException if there is an error reading the bytes.
	 * @see InputStreams#readBytes(InputStream)
	 */
	public static byte[] readBytes(@Nonnull final ClassLoader classLoader, @Nonnull final String resourcePath) throws IOException {
		//getResourceAsStream() does this same thing, but opening the stream manually prevents discarding any IOException during opening
		final URL resourceUrl = findResource(classLoader, resourcePath).orElseThrow(() -> new FileNotFoundException("Resource not found: " + resourcePath));
		try (final InputStream inputStream = resourceUrl.openConnection().getInputStream()) {
			return InputStreams.readBytes(inputStream);
		}
	}

}
