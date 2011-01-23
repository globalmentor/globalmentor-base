package com.globalmentor.urf;

import java.util.Collection;

/**An URF resource that is also a collection.
<p>Copyright © 2007 GlobalMentor, Inc.
This source code can be freely used for any purpose, as long as the following conditions are met.
Any object code derived from this source code must include the following text to users using along with other "about" notifications:
"Uniform Resource Framework (URF) &lt;http://www.urf.name/&gt; specification and processing
written by Garret Wilson &lt;http://www.garretwilson.com/&gt; and Copyright © 2007 GlobalMentor, Inc. &lt;http://www.globalmentor.com/&gt;."
Any redistribution of this source code or derived source code must include these comments unmodified.</p>
@param <E> The type of element stored in the collection.
@author Garret Wilson
*/
public interface URFCollectionResource<E extends URFResource> extends URFResource, Collection<E>
{
}