/*
 * Copyright © 2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

package com.globalmentor.text.directory.vcard;

import static com.globalmentor.application.CommandLineArguments.*;
import static com.globalmentor.java.Characters.*;

import java.io.*;
import java.net.URI;

import com.globalmentor.application.AbstractApplication;
import com.globalmentor.io.Files;
import com.globalmentor.lex.Identifier;
import com.globalmentor.log.Log;

/**
 * Application and routines for concatenating the contents of one or more VCards into a single output file.
 * <p>
 * One benefit of concatenating, if even for a single input file, is that the file contents will be normalized, compensating for variations in the output of
 * various producers.
 * </p>
 * @author Garret Wilson
 */
public class VCardCat extends AbstractApplication
{

	/** The application URI. */
	public final static URI APPLICATION_URI = URI.create("http://globalmentor.com/software/vcardcat");

	/** The application title. */
	public final static String TITLE = "VCardCat" + TRADE_MARK_SIGN_CHAR;

	/** The application copyright. */
	public final static String COPYRIGHT = "Copyright " + COPYRIGHT_SIGN + " 2011 GlobalMentor, Inc. All Rights Reserved."; //TODO i18n

	/** The version of the application. */
	public final static String VERSION = "1.0-SNAPSHOT build 2010-04-23";

	/** Application command-line parameters. */
	public enum Parameter implements Identifier
	{
		/** The input file or directory. */
		INPUT,
		/** The output file. */
		OUTPUT;
	}

	/**
	 * Argument constructor.
	 * @param args The command line arguments.
	 */
	public VCardCat(final String[] args)
	{
		super(APPLICATION_URI, TITLE, args); //construct the parent class
		//TODO set version somehow
		//TODO set the copyright DCMI.setRights(this, COPYRIGHT); //set the application copyright
	}

	/**
	 * The main application method.
	 * @return The application status.
	 */
	public int main()
	{
		final String[] args = getArgs(); //get the arguments
		final String inputString = getOption(args, Parameter.INPUT); //get the input parameter
		if(inputString == null) //if the source parameter is missing
		{
			System.out.println(TITLE);
			System.out.println(VERSION);
			System.out.println(COPYRIGHT);
			System.out.println("Usage: VCardCat --input <file[wildcard]> [--output file]");
			return 0;
		}
		final String outputString = getOption(args, Parameter.OUTPUT); //get the output parameter, if any
		try
		{
			final OutputStream outputStream = outputString != null ? new BufferedOutputStream(new FileOutputStream(outputString)) : System.out;
			try
			{
				final VCardIO vcardIO = new VCardIO();
				vcardIO.setSerializationSingleValueNames(VCard.NOTE_TYPE); //combine notes TODO make this optional
				for(final File file : Files.listWildcards(new File(inputString))) //look at all the specified files
				{
					try
					{
						//read the card from resources
						final VCard vcard = Files.read(file, vcardIO); //read this VCard
						vcardIO.write(outputStream, null, vcard); //write the VCard to the output
					}
					catch(final Throwable throwable)
					{
						Log.error("Error processing VCard file", file, throwable);
						return 1;
					}
				}
			}
			finally
			{
				if(outputString != null) //if we opened a special file
				{
					outputStream.close(); //close the file
				}
			}
		}
		catch(final Throwable throwable)
		{
			Log.error(throwable);
			return 1;
		}
		return 0;
	}

	/**
	 * The main routine that starts the application.
	 * @param args The command line arguments.
	 */
	public static void main(final String[] args)
	{
		run(new VCardCat(args), args); //start a new application
	}
}
