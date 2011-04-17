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

package com.globalmentor.text;

import java.util.StringTokenizer;

import com.globalmentor.collections.Arrays;
import com.globalmentor.java.*;
import static com.globalmentor.java.CharSequences.*;
import static com.globalmentor.java.Characters.*;

/**Constants and utilities for working with the structure of text.
<p>Work in progress.</p>
@author Garret Wilson
*/
public class Prose
{

	/*TODO add these values from OEBPS 1.x, which are from the 13th Edition of _The Chicago Manual of Style_
	cover the book cover(s), jacket information, etc.
	title-page page with possibly title, author, publisher, and other metadata
	toc table of contents
	index back-of-book style index
	glossary glossary
	acknowledgements
	bibliography
	colophon
	copyright-page
	dedication
	epigraph
	foreword
	loi list of illustrations
	lot list of tables
	notes
	preface
	*/
	/**Indicates no heading.*/
	public final static int NO_HEADING=0;	//TODO convert all these to enums
	//ordered hierarchical headings, with lower number indicating higher in the hierarchy
	public final static int VOLUME_HEADING=1;
	public final static int BOOK_HEADING=2;
	public final static int PART_HEADING=3;
	public final static int CHAPTER_HEADING=4;
	public final static int ACT_HEADING=5;
	public final static int SCENE_HEADING=6;
	public final static int ARTICLE_HEADING=7;
	public final static int SUB_HEADING=8;
	public final static int TITLE_HEADING=9;
	/**The significant ordered heading with the highest value.*/
	public final static int MAX_SIGNIFICANT_HEADING=SCENE_HEADING;
	/**The ordered heading with the highest value.*/
	public final static int MAX_HEADING=TITLE_HEADING;
	//these are non-hierarchical headings, always considered to be the highest
	//  in the hierarchy; they have negative values not only for convenience, but
	//  also to indicate that they always take precedence over the ordered
	//  headings
	public final static int CONTENTS_HEADING=-1;
	public final static int PREFACE_HEADING=-2;
	public final static int FOREWORD_HEADING=-3;
	public final static int INTRODUCTION_HEADING=-4;
	public final static int AFTERWORD_HEADING=-5;
	public final static int BIBLIOGRAPHY_HEADING=-6;
	public final static int GLOSSARY_HEADING=-7;
	public final static int INDEX_HEADING=-8;
	public final static int GOSPEL_HEADING=-9;
	public final static int PAGE_BREAK_HEADING=-10;

	/**Checks to see which type of heading is represented by the given text.
		<p>The following types of headings are checked as numbered sections:</p>
		<ul>
			<li><code>VOLUME_HEADING</code></li>
			<li><code>BOOK_HEADING</code></li>
			<li><code>ARTICLE_HEADING</code></li>
			<li><code>PART_HEADING</code></li>
			<li><code>CHAPTER_HEADING</code></li>
			<li><code>ACT_HEADING</code></li>
			<li><code>SCENE_HEADING</code></li>
		</ul>
		<p>The following types of headings are checked by the presence of title
			labels:</p>
		<ul>
			<li><code>CONTENTS_HEADING</code></li>
			<li><code>PREFACE_HEADING</code></li>
			<li><code>FOREWORD_HEADING</code></li>
			<li><code>INTRODUCTION_HEADING</code></li>
			<li><code>AFTERWORD_HEADING</code></li>
			<li><code>BIBLIOGRAPHY_HEADING</code></li>
			<li><code>GLOSSARY_HEADING</code></li>
			<li><code>INDEX_HEADING</code></li>
			<li><code>GOSPEL_HEADING</code></li>
		</ul>
		<p>The following type of headings is recognized by being in uppercase and
			not consisting entirely of digits:</p>
		<ul>
			<li><code>SUB_HEADING</code></li>
		</ul>
		<p>The following type of headings is recognized by being correctly capitalized
			on a single line:</p>
		<ul>
			<li><code>TITLE_HEADING</code></li>
		</ul>
		<p>The following type of headings is recognized by its containing only the
			characters '*' and/or '-', or the string "page" after punctuation is
			removed, and appears only on a single line:</p>
		<ul>
			<li><code>PAGE_BREAK_HEADING</code></li>
		</ul>
		@param text The text to check for heading type.
		@return The type of heading, or <code>NO_HEADING</code> if the heading could
			not be determined.
		@see #containsTitleLabel
		@see #getSectionNumber
		*/
		public static int getHeadingType(final String text)  //TODO write comment about mistaking signatures for headings, such as "A. LINCOLN"
		{
			if(text.length()>0) //if there is text at all
			{
					//find out how many lines there are TODO testing
				final int lineCount=(new StringTokenizer(text, EOL_CHARACTERS.toString(), true).countTokens()+1)/2;
				if(lineCount<4) //if there are less than four lines
				{
					if(isBreak(text))  //if this is text for a page break
						return PAGE_BREAK_HEADING;
					else if(getSectionNumber(text, "volume")>=0)  //TODO fix; use constant
						return VOLUME_HEADING;
					else if(getSectionNumber(text, "book")>=0)  //TODO fix; use constant
					{
						return BOOK_HEADING;
					}
					else if(getSectionNumber(text, "article")>=0)  //TODO fix; use constant
						return ARTICLE_HEADING;
					else if(getSectionNumber(text, "part")>=0)  //TODO fix; use constant
						return PART_HEADING;
					else if(getSectionNumber(text, "chapter")>=0 || getSectionNumber(text, "c")>=0)  //TODO fix; use constant
						return CHAPTER_HEADING;
					else if(getSectionNumber(text, "act")>=0)  //TODO fix; use constant
						return ACT_HEADING;
	/*TODO fix
					else if(getSectionNumber(text, "akt")>=0)  //(German) TODO fix; use constant
						return ACT_HEADING;
	*/
					else if(getSectionNumber(text, "scene")>=0)  //TODO fix; use constant
						return SCENE_HEADING;
	/*TODO fix
					else if(getSectionNumber(text, "szene")>=0)  //TODO fix; use constant
						return SCENE_HEADING;
	*/
					else
					{
						final String line=Strings.removeAfterFirstChar(text, EOL_CHARACTERS);  //get the first line of text
							//see if this is one of the fixed hierarchical headings
						if(isTitleHeading(line))  //if this is a title heading
						{
								//if this is a table of contents heading TODO use constants
							if(containsOnlyIgnoreCase(line, new String[]{"contents"}, new String[]{"table", "of"})) //TODO use constants
							{
								return CONTENTS_HEADING;
							}
								//if this is a preface heading
							else if(containsOnlyIgnoreCase(line, new String[]{"preface"}, new String[]{}))  //G***use a constant
							{
								return PREFACE_HEADING;
							}
								//if this is a foreword heading
							else if(containsOnlyIgnoreCase(line, new String[]{"foreword"}, new String[]{}))  //G***use a constant
							{
								return FOREWORD_HEADING;
							}
								//if this is an introduction heading
							else if(containsOnlyIgnoreCase(line, new String[]{"index"}, new String[]{}))  //G***use a constant
							{
								return INTRODUCTION_HEADING;
							}
								//if this is an afterword heading
							else if(containsOnlyIgnoreCase(line, new String[]{"afterword"}, new String[]{}))  //G***use a constant
							{
								return AFTERWORD_HEADING;
							}
								//if this is a bibliography heading
							else if(containsOnlyIgnoreCase(line, new String[]{"bibliography"}, new String[]{}))  //G***use a constant
							{
								return BIBLIOGRAPHY_HEADING;
							}
								//if this is a  glossary heading
							else if(containsOnlyIgnoreCase(line, new String[]{"glossary"}, new String[]{}))  //G***use a constant
							{
								return GLOSSARY_HEADING;
							}
								//if this is an index heading
							else if(containsOnlyIgnoreCase(line, new String[]{"index"}, new String[]{}))  //G***use a constant
							{
								return INDEX_HEADING;
							}
								//if this is a gospel heading
							if(containsOnlyIgnoreCase(line, new String[]{"Gospel", "According to"}, new String[]{})) //G***use constants
							{
								return GOSPEL_HEADING;
							}
						}
						if(isSubHeading(line)) //if this is a normal subheading
						{
							return SUB_HEADING;
						}
						//if this is text for a title (note that capitalized words are so
						//  prevalent, we won't think this is a title unless all of the lines
						//  are capitalized correctly)
						else if(isTitleHeading(text))
						{
							return TITLE_HEADING;
						}
					}
				}
			}
			return NO_HEADING;  //show that this doesn't appear to be a heading
		}

	/**Determines if the given text is a page break.
			<p>A break is one of the following conditions:</p>
			<ul>
			  <li>Text comprised solely of the following characters: '*', '-', '_',
					em-dash, and/or en-dash.</li>
			  <li>The word "page" surrounded by only punctuation and/or whitespace.</li>
			</ul>
		@param text The text to check.
		@return <code>true</code> if the text is a page break heading.
		*/
		public static boolean isBreak(final String text)
		{
			  //if the string is only made up of asterisks and hyphens
			if(text.length()>2 && isAllChars(text, new Characters('*', '-', '_', EM_DASH_CHAR, EN_DASH_CHAR)))  //TODO use constants
				return true;  //this is a page break heading
					//if this line contains "page" surrounded by only punctuation
			else if("page".equalsIgnoreCase(Strings.trim(text, PUNCTUATION_CHARS.add(TRIM_CHARACTERS))))
				return true;  //this is a page break indication
			else  //if this is not a page break heading
				return false; //show that we don't think this is a page break heading
		}

	/**Determines if the given text is a page number.
			A page number appears on a single line and has a page indicator and a
			number, along with an optional select set of symbols but no other letters.
		@param text The text to check.
		@return <code>true</code> if the text is a page number.
		*/
		public static boolean isPageNumber(final String text) //TODO put this in some common routine in a common package
		{
			if(CharSequences.charIndexOf(text, EOL_CHARACTERS)>0)  //if the text is not on a single line
				return false; //this isn't a page number
			/**The strings that count as page indications.*/
			final String[] pageStrings=new String[]{"p", "P", "pg", "Pg", "PG", "page", "Page", "PAGE"};
			int pageCount=0;  //the number of times we find a representation for "page" on the line
			int numberCount=0;  //the number of times we find a representation of a number
			final String pageNumberDelimiters=TRIM_CHARACTERS+"-*.[]<>";  //these characters separate the page numbers
			final StringTokenizer stringTokenizer=new StringTokenizer(text, pageNumberDelimiters);  //tokenize the page number
			while(stringTokenizer.hasMoreTokens())  //while there are more tokens
			{
				final String token=stringTokenizer.nextToken();  //get the next token
				if(Arrays.indexOf(pageStrings, token)>=0)  //if this is a page indicator
				{
					++pageCount;  //show we found another page indicator
				}
					//if this is a number
				else if(isDigits(token) || isRomanNumerals(token))
				{
				  ++numberCount;  //show that we found another number
				}
				else if(isLetters(token)) //if this token is letters (but not the special "page" sequence, for which we already checked)
				{
					return false; //it's probably not page number if other characters are present---at least we can't take that chance
				}
			}
			return pageCount==1 && numberCount==1;  //this is a page number if we find a single page indicator with a single number
		}

	/**Determines if the given text is a subheading.
		A subheading is in all uppercase, is not composed completely of digits, and
		is not surrounded by quotation marks.
	@param text The text to check.
	@return <code>true</code> if the text is a subheading.
	*/
	public static boolean isSubHeading(final String text)
	{
		if(text.length()>0 && !isQuoted(text)) //if there is text and it's not quoted
		{
				//if the text is in uppercase, and there is text TODO a hasLetters() would be equivalent and more efficient
			if(containsLetter(text) && isUpperCase(text))
			{
				return true;  //the text has pased our tests
			}
		}
		return false; //we don't think this is a subheading
	}

	/**Determines if the given text is a title heading.
			A title heading appears on a single line and capitalizes each word, except
			for some exception words (such as "the" and "of").
			<p>Popular prepositions used from Heather MacFadyen, University of Ottawa, at
			<a href="http://www.uottawa.ca/academic/arts/writcent/hypergrammar/preposit.html">
			http://www.uottawa.ca/academic/arts/writcent/hypergrammar/preposit.html</a></p>
		@param text The text to check.
		@return <code>true</code> if the text is a title heading.
		*/
		public static boolean isTitleHeading(final String text)
		{
			if(isQuoted(text) || !containsLetterOrDigit(text))  //if this text is quoted or has no letters or digits
			  return false; //quoted strings are not headings
	/*TODO do we really need this? check
			if(StringUtilities.charIndexOf(text, EOL_CHARS)>0)  //if the text is not on a single line
				return false; //this isn't a title heading
	*/
			int wordCount=0;  //we'll keep track of the number of words
			int exceptionCount=0; //we'll keep track of the number of exceptions
			final StringTokenizer wordTokenizer=new StringTokenizer(text, WORD_DELIMITER_CHARACTERS.toString());  //tokenize the title into words
			while(wordTokenizer.hasMoreTokens())  //while there are more tokens
			{
				final String word=wordTokenizer.nextToken();  //get the next word
				++wordCount;  //show that we found another word
			  final char firstChar=word.charAt(0);  //get the first character (the tokenizer should never return an empty string)
					//if the word isn't capitalized and isn't an exception word
			  if(Character.isLetter(firstChar) && !Character.isUpperCase(firstChar))
				{
			    boolean isException=!isTitleCapitalizationRequired(word); //see if this is one of the exceptions
					if(isException) //if we've found an exception
					{
						++exceptionCount; //show that we found another exception
					}
					else  //if this isn't one of our exception words
					{
						return false; //this is not a correct title
					}
				}
			}
			if(exceptionCount==wordCount) //if every word is an exception (e.g. "under on the")
				return false; //title must have at least one non-exception word
			return true;  //if the text passed all our tests, it's probably a title
		}

	/**Determines if the given string should be capitalized if appearing in a
		title.
	@param word The word to test for title capitalization.
	@return <code>true</code> if the word should be capitalized in a title.
	*/
	public static boolean isTitleCapitalizationRequired(final String word)
	{
		/**The strings that do not have to be capitalized in a title.*/
		final String[] exceptionWords=new String[]{"and", "but", "or", "nor",
			"a", "an", "the", //articles
		  //prepositions
		"about", "above", "across", "after", "against", "along", "among", "around",
		"at", "before", "behind", "below", "beneath", "beside", "between", "beyond",
		"but", "by", "despite", "down", "during", "except", "for", "from", "in",
		"inside", "into", "like", "near", "of", "off", "on", "onto", "out",
		"outside", "over", "past", "since", "through", "throughout", "till", "to",
		"toward", "under", "underneath", "until", "up", "upon", "with", "within", "without",
		"de", "du", "des", //foreign prepositions thrown in for good measure (used in names, for instance)
		"la", "le", "las", "les", "l", //foreign articles thrown in for good measure (used in names, for instance)
		"s"}; //when a title shows possession, the 's' of "'s" will get tokenized as well
		return Arrays.indexOf(exceptionWords, word)<0; //the word should be capitalized if it is not one of the exceptions
	}

	/**Determines if the given line contains a title label.
		The given label is matched case insensitively, but its first character must
		be capitalized.
	@param text The text to check.
	@param label The label to match.
	@return <code>true</code> if the given text contains the given title label.
	*/
	public static boolean containsTitleLabel(final String text, final String label)
	{
		final int labelIndex=Strings.indexOfIgnoreCase(text, label);  //see if the label appears in the text
		if(labelIndex>=0) //if the label appears in any case
		{
			if(Character.isUpperCase(text.charAt(labelIndex)))  //if the first character of the label is uppercase
				return true;  //show that this is a title label
		}
		return false; //we didn't find the title label in the string
	}

	/**Determines if the given line contains several required and optional labels,
		compared without case sensitivity.
	@param text The text to check.
	@param requiredLabels The labels that must be present.
	@param optionalLabels The labels that are optional.
	@return <code>true</code> if the given text contains the required labels and
		only the required and optional labels.
	*/
	public static boolean containsOnlyIgnoreCase(final String text, final String[] requiredLabels, final String[] optionalLabels)
	{
		for(int i=requiredLabels.length-1; i>=0; --i) //look at each of the required labels
		{
			final String requiredLabel=requiredLabels[i]; //get this required label
			boolean isMatch=false; //show that we haven't found a match for the required label yet
			final StringTokenizer wordTokenizer=new StringTokenizer(text, WORD_DELIMITER_CHARACTERS.toString()); //look at each word
			while(wordTokenizer.hasMoreTokens())  //while there are more words
			{
				final String word=wordTokenizer.nextToken();  //get the next word
				if(requiredLabel.equalsIgnoreCase(word))  //if this word is the required label
				{
					isMatch=true; //we found a match
					break; //stop looking for this label
				}
			}
			if(!isMatch)  //if we didn't match this required label
				return false; //show that a required label wasn't found
		}
		final StringTokenizer wordTokenizer=new StringTokenizer(text, WORD_DELIMITER_CHARACTERS.toString()); //look at each word
		while(wordTokenizer.hasMoreTokens())  //while there are more words
		{
			final String word=wordTokenizer.nextToken();  //get the next word
			boolean foundMatch=false; //we'll try to find a required or optional match for this word
			for(int i=requiredLabels.length-1; i>=0; --i) //look at each of the required labels TODO maybe put this in StringUtiliites.indexOfIgnoreCase(String[], String)
			{
				if(requiredLabels[i].equalsIgnoreCase(word))  //if this word is the required label
				{
					foundMatch=true; //we found a match
					break; //stop looking for this label
				}
			}
			if(!foundMatch) //if we didn't find a match in the required labels, try the optional ones
			{
				for(int i=optionalLabels.length-1; i>=0; --i) //look at each of the optional labels
				{
					if(optionalLabels[i].equalsIgnoreCase(word))  //if this word is the optional label
					{
						foundMatch=true; //we found a match
						break; //stop looking for this label
					}
				}
			}
			if(!foundMatch) //if we haven't found this word
				return false;   //any word that we don't find means this string doesn't fit the qualifications
		}
		return true;  //this string passed all our tests
	}

	/**Determines whether the given text is a numbered section heading, based upon
			the specified heading label. If so, the section number is returned.
			<p>A numbered section appears in one of the following formats on a single
				line, with "chapter" used to represent the section label:</p>
			<ul>
			  <li>Chapter 11</li>
			  <li>Chapter 11: The Frightened Goat</li>
			  <li>Chapter XI</li>
			  <li>Chapter XI: The Frightened Goat</li>
			  <li>Eleventh Chapter</li>
			  <li>The Eleventh Chapter</li>
			</ul>
		@param text The string to test for a section heading.
		@param sectionLabel The label used for this type of section, case-insensitive.
		@return The value of this section, or -1 if the string does not appear to be
			of the given section type.
		*/
	public static int getSectionNumber(final String text, final String sectionLabel)
		{
			final int eolIndex=CharSequences.charIndexOf(text, EOL_CHARACTERS);  //find the end of the line
			final String line=text.substring(0, eolIndex>=0 ? eolIndex : text.length());  //get the text up to our delimiter, if there is one
			if(isQuoted(line))  //if this line is quoted
			  return -1; //quoted strings are not headings
			  //tokenize the string on whitespace and punctuation
			final StringTokenizer tokenizer=new StringTokenizer(line, WORD_DELIMITER_CHARACTERS.toString());
			if(tokenizer.hasMoreTokens()) //if there is a token
			{
				String firstToken=tokenizer.nextToken();  //get the first token
				if(sectionLabel.equalsIgnoreCase(firstToken))  //if this token is a section label
				{
					if(tokenizer.hasMoreTokens()) //if there is another token
					{
						final String numberString=tokenizer.nextToken();  //get the next token
						if(isDigits(numberString))  //if the section label is followed by a number
						{
							return Integer.parseInt(numberString);  //return the value of the number
						}
						else if(isRomanNumerals(numberString))  //if the section label is followed by a Roman numeral
						{
							return RomanNumerals.parseRomanNumerals(numberString);  //return the value of the Roman numerals TODO maybe modularize the Roman numeral routines better at some point
						}
						else  //see if this is a number string
						{
							final int number=Integers.parseNumberTextValue(numberString); //see if this is a number in text
							if(number>=0) //if the string has a valid number
								return number;  //return the number
						}
					}
				}
				else  //if this is not a section label, check for an ordered section (e.g. "first section")
				{
					if(firstToken.equalsIgnoreCase("the") && tokenizer.hasMoreTokens()) //if this is the word "the"
						firstToken=tokenizer.nextToken(); //skip any beginning "the"
					final int order=Integers.parseOrdinalValue(firstToken);  //see if this is an order
					if(order>=0)  //if this token is a valid order
					{
						if(tokenizer.hasMoreTokens()) //if there is another token
						{
							final String labelToken=tokenizer.nextToken();  //get the next token
							if(sectionLabel.equalsIgnoreCase(labelToken))  //if this token is a section label
							{
								return order; //return the order
							}
						}
					}
				}
			}
			return -1; //this doesn't appear to be the requested type of section
		}

	/**Determines if the given string is quoted.
	@param string The string to check.
	@return <code>true</code> if the string's first or last non-whitespace
		character is some sort of quotation mark.
	*/
	public static boolean isQuoted(final String string)
	{
		  //get the index of the first non-whitespace character
		final int firstCharIndex=CharSequences.notCharIndexOf(string, TRIM_CHARACTERS);
		if(firstCharIndex>=0) //if there is a first character (which also means there's a last character
		{
			if(LEFT_QUOTE_CHARS.indexOf(string.charAt(firstCharIndex))>=0) //if the line starts with a quote character
				return true; //show that we found a quote
				//get the index of the last non-whitespace character (we don't need to make sure it's valid--if there's a first character, there's a last character)
			final int lastCharIndex=CharSequences.notCharLastIndexOf(string, TRIM_CHARACTERS);
			if(RIGHT_QUOTE_CHARS.indexOf(string.charAt(lastCharIndex))>=0) //if the line ends with a quote character
				return true; //show that we found a quote
		}
		return false; //we couldn't find a beginning or ending quote
	}

}
