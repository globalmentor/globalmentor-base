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

package com.globalmentor.text.xml.stylesheets.css;

import com.globalmentor.lex.Identifier;

/**
 * Standard cursors supported by CSS.
 * @author Garret Wilson
 * @see <a href="http://www.w3.org/TR/CSS21/ui.html">CSS 2.1 User Interface: Cursors</a>
 */
public enum XMLCSSCursor implements Identifier {

	/** The UA determines the cursor to display based on the current context. */
	AUTO,
	/** A simple crosshair (e.g., short line segments resembling a "+" sign). */
	CROSSHAIR,
	/** The platform-dependent default cursor. Often rendered as an arrow. */
	DEFAULT,
	/** The cursor is a pointer that indicates a link. */
	POINTER,
	/** Indicates something is to be moved. */
	MOVE,
	/** Indicate that some edge is to be moved from the east of the box. */
	E_RESIZE,
	/** Indicate that some edge is to be moved from the north-east corner of the box. */
	NE_RESIZE,
	/** Indicate that some edge is to be moved from the north-west corner of the box. */
	NW_RESIZE,
	/** Indicate that some edge is to be moved from the north of the box. */
	N_RESIZE,
	/** Indicate that some edge is to be moved from the south-east corner of the box. */
	SE_RESIZE,
	/** Indicate that some edge is to be moved from the south-west corner of the box. */
	SW_RESIZE,
	/** Indicate that some edge is to be moved from the south corner of the box. */
	S_RESIZE,
	/** Indicate that some edge is to be moved from the west of the box. */
	W_RESIZE,
	/** Indicates text that may be selected. Often rendered as an I-beam. */
	TEXT,
	/** Indicates that the program is busy and the user should wait. Often rendered as a watch or hourglass. */
	WAIT,
	/**
	 * A progress indicator. The program is performing some processing, but is different from {@link #WAIT} in that the user may still interact with the program.
	 * Often rendered as a spinning beach ball, or an arrow with a watch or hourglass.
	 */
	PROGRESS,
	/** Help is available for the object under the cursor. Often rendered as a question mark or a balloon. */
	HELP;
}
