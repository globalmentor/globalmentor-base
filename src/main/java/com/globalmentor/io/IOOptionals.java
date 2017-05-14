/*
 * Copyright © 2017 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.io.IOException;
import java.util.Optional;

import javax.annotation.*;

import com.globalmentor.io.function.*;
import com.globalmentor.util.Optionals;

/**
 * Utilities to use with {@link Optional} that allow {@link IOException} to be propagated.
 * @author Garret Wilson
 * @see IOException
 * @see Optionals
 */
public class IOOptionals {

	/**
	 * If a value is present, invokes the given consumer with the optional value; otherwise, does nothing.
	 * @param <T> The type of value contained in the optional.
	 * @param ioConsumer The object to receive the value if a value is present.
	 * @param optional The {@link Optional} with the optional value on which the consumer will be invoked.
	 * @throws NullPointerException if the optional is <code>null</code>; or if the value is present but the consumer is <code>null</code>.
	 * @throws IOException if there was an I/O exception while consuming the value.
	 */
	public static <T> void ifPresent(@Nonnull final Optional<T> optional, @Nonnull final IOConsumer<? super T> ioConsumer) throws IOException {
		if(optional.isPresent()) {
			ioConsumer.accept(optional.get());
		}
	}

}
