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

package com.globalmentor.util;

import java.io.*;
import java.net.URI;
import java.util.*;
import java.util.prefs.Preferences;

import com.globalmentor.net.*;
import com.globalmentor.net.http.HTTPClient;
import com.globalmentor.rdf.*;
import com.globalmentor.rdf.dublincore.RDFDublinCore;

/**An application that by default is a console application.
<p>Every application provides a default preference node based upon the
	implementing application class.</p>
@author Garret Wilson
*/
public abstract class Application extends DefaultRDFResource
{

	/**An array containing no arguments.*/
	protected final static String[] NO_ARGUMENTS=new String[0];
	
	/**The authenticator object used to retrieve client authentication.*/
	private Authenticable authenticator=null;

		/**@return The authenticator object used to retrieve client authentication.*/
		public Authenticable getAuthenticator() {return authenticator;}
	
		/**Sets the authenticator object used to retrieve client authentication.
		This version updates the authenticator of the default HTTP client.
		@param authenticable The object to retrieve authentication information regarding a client.
		@see com.globalmentor.net.http.HTTPClient
		*/
		public void setAuthenticator(final Authenticable authenticable)
		{
			if(authenticator!=authenticable)	//if the authenticator is really changing
			{
				authenticator=authenticable;	//update the authenticator
				HTTPClient.getInstance().setAuthenticator(authenticable);	//update the authenticator for HTTP connections on the default HTTP client			
			}
		}
	
	/**Whether the object has been modified; the default is not modified.*/
	private boolean modified=false;

	/**The command-line arguments of the application.*/
	private final String[] args;

		/**@return The command-line arguments of the application.*/
		public String[] getArgs() {return args;}

	/**@return The default user preferences for this application.
	@exception SecurityException Thrown if a security manager is present and
		it denies <code>RuntimePermission("preferences")</code>.
	*/
	public Preferences getPreferences() throws SecurityException
	{
		return Preferences.userNodeForPackage(getClass());	//return the user preferences node for whatever class extends this one 
	}

	/**@return The title of the application, or <code>null</code> if there is no title.*/
	public RDFObject getTitle()
	{
		return RDFDublinCore.getTitle(this);	//return the title object if there is one
	}

	/**The expiration date of the application, or <code>null</code> if there is
		no expiration.
	*/
	private Calendar expiration=null;

		/**The expiration date of the application, or <code>null</code> if there is
			no expiration.
		*/
		public Calendar getExpiration() {return expiration;}

		/**Sets the expiration of the application.
		@param newExpiration The new expiration date, or <code>null</code> if there
			is no expiration.
		*/
		public void setExpiration(final Calendar newExpiration) {expiration=newExpiration;}

	/**The RDF data model of the application, lazily created.*/
	private RDF rdf=null;

	/**@return The RDF data model of the application, lazily created.*/
	public RDF getRDF()
	{
		if(rdf==null)	//if there is no RDF data model
		{
			rdf=new RDF();	//create a new RDF data model
		}
		return rdf;	//return the RDF data model
	}

	/**Reference URI constructor.
	@param referenceURI The reference URI of the application.
	*/
	public Application(final URI referenceURI)
	{
		this(referenceURI, NO_ARGUMENTS);	//construct the class with no arguments
	}

	/**Reference URI and arguments constructor.
	@param referenceURI The reference URI of the application.
	@param args The command line arguments.
	*/
	public Application(final URI referenceURI, final String[] args)
	{
		super(referenceURI);	//construct the parent class
		this.args=args;	//save the arguments
	}

	/**Initializes the application.
	This method is called after construction but before application execution.
	This version does nothing.
	@exception Exception Thrown if anything goes wrong.
	*/
	public void initialize() throws Exception	//TODO create a flag that only allows initialization once
	{
	}

	/**The main application method.
	@return The application status.
	*/ 
	public int main()
	{
		return 0;	//default to returning no error
	}

	/**Checks requirements, permissions, and expirations before starting.
	@return <code>true</code> if the checks succeeded.	
	*/
	public boolean canStart()
	{
			//check the expiration
		if(getExpiration()!=null)	//if there is an expiration date
		{
			final Calendar now=Calendar.getInstance();	//get the current date
			if(now.after(getExpiration()))	//if the application has expired
			{
					//TODO get the web site from dc:source to
					//TODO display a title
				displayError("This version of "+(getTitle()!=null ? getTitle().toString() : "")+" has expired.\nPlease visit http://www.globalmentor.com/software/marmot/\nto download a new copy.");	//G***i18n
				return false;
			}
		}
		return true;	//show that everything went OK
	}

	/**Displays an error message to the user for an exception.
	@param throwable The condition that caused the error.
	*/
	public void displayError(final Throwable throwable)
	{
		Debug.trace(throwable);	//log the error
		displayError(getDisplayErrorMessage(throwable));	//display an error to the user for the throwable
	}
	
	/**Displays the given error to the user
	@param message The error to display. 
	*/
	public void displayError(final String message)
	{
		System.err.println(message);	//display the error in the error output
	}

	/**Constructs a user-presentable error message based on an exception.
	In most cases this is <code>Throwable.getMessage()</code>.
	@param throwable The condition that caused the error.
	@see Throwable#getMessage()
	*/
	public static String getDisplayErrorMessage(final Throwable throwable)
	{
		if(throwable instanceof FileNotFoundException)	//if a file was not found
		{
			return "File not found: "+throwable.getMessage();	//create a message for a file not found G***i18n
		}
/*TODO this throws a security exception with Java WebStart; see if it's even needed anymore
		else if(throwable instanceof sun.io.MalformedInputException)	//if there was an error converting characters; G***put this elsewhere, fix for non-Sun JVMs
		{
			return "Invalid character encountered for file encoding.";	//G***i18n
		}
*/
		else  //for any another error
		{
			return throwable.getMessage()!=null ? throwable.getMessage() : throwable.getClass().getName();  //get the throwable message or, on last resource, the name of the class
		}
	}

	/**Starts an application.
	@param application The application to start. 
	@param args The command line arguments.
	@return The application status.
	*/
	public static int run(final Application application, final String[] args)
	{
		int result=0;	//start out assuming a neutral result TODO use a constant and a unique value
		try
		{
			initialize(application, args);	//initialize the environment
			application.initialize();	//initialize the application
			if(application.canStart())	//perform the pre-run checks; if everything went OK
			{
				result=application.main();	//run the application
			}
			else	//if something went wrong
			{
				result=-1;	//show that we couldn't start TODO use a constant and a unique value
			}
		}
		catch(final Throwable throwable)  //if there are any errors
		{
			result=-1;	//show that there was an error TODO use a constant and a unique value
			application.displayError(throwable);	//report the error
		}
		if(result<0)	//if we something went wrong, exit (if everything is going fine, keep running, because we may have a server or frame running)
		{
			try
			{
				application.exit(result);	//exit with the result (we can't just return, because the main frame, if initialized, will probably keep the thread from stopping)
			}
			catch(final Throwable throwable)  //if there are any errors
			{
				result=-1;	//show that there was an error during exit TODO use a constant and a unique value
				application.displayError(throwable);	//report the error
			}
			finally
			{
				System.exit(result);	//provide a fail-safe way to exit		
			}
		}
		return result;	//always return the result		
	}

	/**Initializes the environment for the application.
	@param application The application to start. 
	@param args The command line arguments.
	@exception Exception Thrown if anything goes wrong.
	*/
	protected static void initialize(final Application application, final String[] args) throws Exception
	{
		CommandLineArguments.configureDebug(args); //configure debugging based upon the command line arguments
	}

	/**Responds to a throwable error.
	@param throwable The throwable object that caused the error 
	*/
/*G***del if not needed
	protected static void error(final Throwable throwable)
	{
		Debug.error(throwable); //report the error
	}
*/

	
	/**Determines whether the application can exit.
	This method may query the user.
	If the application has been modified, the configuration is saved if possible.
	If there is no configuration I/O kit, no action is taken.
	If an error occurs, the user is notified.
	@return <code>true</code> if the application can exit, else <code>false</code>.
	*/
	protected boolean canExit()
	{
		return true;	//show that we can exit
	}

	/**Exits the application with no status.
	Convenience method which calls <code>exit(int)</code>.
	@see #exit(int)
	*/
	public final void exit()
	{
		exit(0);	//exit with no status
	}
	
	/**Exits the application with the given status.
	This method first checks to see if exit can occur.
	To add to exit functionality, <code>performExit()</code> should be overridden rather than this method.
	@param status The exit status.
	@see #canExit()
	@see #performExit(int)
	*/
	public final void exit(final int status)
	{
		if(canExit())	//if we can exit
		{
			try
			{
				performExit(status);	//perform the exit
			}
			catch(final Throwable throwable)  //if there are any errors
			{
				displayError(throwable);	//report the error
			}			
			System.exit(-1);	//provide a fail-safe way to exit, indicating an error occurred		
		}
	}

	/**Exits the application with the given status without checking to see if exit should be performed.
	@param status The exit status.	
	@exception Exception Thrown if anything goes wrong.
	*/
	protected void performExit(final int status) throws Exception
	{
		System.exit(status);	//close the program with the given exit status		
	}

}
