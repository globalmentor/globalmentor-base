package com.garretwilson.util;

/**An object that can appear in a sequence, able to provide the next object in
	the sequence.
<p>This interface differs from <code>Iterator</code> in the semantics of the
	"get next" functionality. An <code>Iterator</code> is an independent object
	that returns different next objects after successive calls. A
	<code>Sequenceable</code> is an object that is part of a sequence, which will
	always return the successive object after <code>this</code> (which will
	seldom change).</p>
@author Garret Wilson
@see Iterator
*/
public interface Sequenceable
{

	/**@return <code>true</code> if there is an object after this one in the sequence.*/
	public boolean hasNext();	

	/**@return The next object in the sequence, which need not be constant.*/
	public Object getNext();	
	
}
