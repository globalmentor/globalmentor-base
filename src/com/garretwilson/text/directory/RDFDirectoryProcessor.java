package com.garretwilson.text.directory;

import java.io.*;
import java.net.URI;
import java.net.MalformedURLException;	//G***del when works
import java.net.URISyntaxException;
import java.util.*;
import com.garretwilson.io.*;
import com.garretwilson.net.URLUtilities;
import com.garretwilson.rdf.*;
import com.garretwilson.util.*;

/**Class that is able to construct an RDF data model from a directory of type
	<code>text/directory</code> as defined in
	<a href="http://www.ietf.org/rfc/rfc2425.txt">RFC 2425</a>,
	"A MIME Content-Type for Directory Information".
@author Garret Wilson
*/
public class RDFDirectoryProcessor extends AbstractRDFProcessor
{

	/**Default constructor.*/
	public RDFDirectoryProcessor()
	{
	}

	/**Constructor that specifies an existing data model to continue filling.
	@param rdf The RDF data model to use.
	*/
	public RDFDirectoryProcessor(final RDF rdf)
	{
		super(rdf);  //construct the parent class
	}

	/**Processes a directory and converts content line a property of the
		given RDF resource.
	@param resource The resource the directory represents.
	@param reader The reader that contains the lines of the directory.
	@return The RDF data model resulting from this processing and any previous
		processing.
	@exception IOException Thrown if there is an error reading the directory.
	@exception ParseIOException Thrown if there is a an error interpreting the directory.
	*/
	public RDF process(final RDFResource resource, final LineUnfoldParseReader reader) throws IOException, ParseIOException
	{
		final RDF rdf=getRDF(); //get the RDF data model we're using



		return rdf;  //return the RDF data model
	}
	
	
	/**Retrieves a line of content from the directory.
	 * 
	 * @param reader
	 * @return ContentLine
	 */
	public ContentLine processContentLine(final LineUnfoldParseReader reader)
	{
	}


	/**A line in a directory of type <code>text/directory</code> as defined in
		"RFC 2425: A MIME Content-Type for Directory Information".
	@author Garret Wilson
	*/
	protected static class ContentLine extends NameValuePair
	{

		/**The group specification, or <code>null</code> if there is no group.*/
		private String group=null;	
	
			/**@return The group specification, or <code>null</code> if there is no group.*/
			public String getGroup() {return group;}

			/**Sets the group.
			@param group The group specification, or <code>null</code> if there is no group.
			*/
			public void setGroup(final String group) {this.group=group;}
		
		/**The list of parameters.*/
		private List paramList=new ArrayList();
	
	
		
		
//	G***fix	contentline  = [group "."] name *(";" param) ":" value CRLF

		public ContentLine(final String name, final Object value)
		{
			super(name, value);	//construct the parent class
		}
	}
}