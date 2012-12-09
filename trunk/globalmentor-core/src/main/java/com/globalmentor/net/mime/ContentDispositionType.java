/*
 * Copyright © 1996-2008 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.net.mime;

import com.globalmentor.lex.Identifier;

/**Represents a MIME content disposition type as defined in
	<a href="http://www.rfc-editor.org/rfc/rfc1806.txt">RFC 1806</a>,
	"Communicating Presentation Information in Internet Messages: The Content-Disposition Header".
@author Garret Wilson
@see <a href="http://www.rfc-editor.org/rfc/rfc1806.txt">Communicating Presentation Information in Internet Messages - The Content-Disposition Header</a>
@author Garret Wilson
*/
public enum ContentDispositionType implements Identifier
{

	/**Indicates a bodypart is intended to be displayed automatically upon display of the message.*/	
	INLINE,

	/**Indicates a bodypart is separate from the main body of the mail message.*/	
	ATTACHMENT;

}
