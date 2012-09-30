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

package com.globalmentor.model;

import java.util.*;

import com.globalmentor.java.Objects;

import static com.globalmentor.java.Objects.*;

/**The encapsulation of text and the locale that indicates the text language.
@author Garret Wilson
*/
public class LocaledText extends DefaultLocaleable implements CharSequence, Comparable<LocaledText>
{

	/**The non-<code>null</code> text.*/
	private String text;

		/**@return The non-<code>null</code> text represented.*/
		public String getText()
		{
			return text;
		}

		/**Sets the represented text.
		@param text The non-<code>null</code> text string to represent.
		@exception NullPointerException if the given text is <code>null</code>.
		*/
		public void setText(final String text) throws NullPointerException
		{
			this.text=checkInstance(text, "Text cannot be null.");	//set the text
		}
	
	/**Copy constructor.
	@param localeText The non-<code>null</code> object containing the text and optional locale to represent.
	@exception NullPointerException if the given locale text is <code>null</code>.
	*/
	public LocaledText(final LocaledText localeText) throws NullPointerException
	{
		this(localeText.getText(), localeText.getLocale());	//construct the class with the local text's text and locale
	}

	/**Text constructor with no locale specified.
	@param text The non-<code>null</code> text string to represent.
	@exception NullPointerException if the given text is <code>null</code>.
	*/
	public LocaledText(final String text) throws NullPointerException
	{
		this(text, null);	//create locale text with no locale
	}
	
	/**Full constructor.
	@param text The non-<code>null</code> text string to represent.
	@param locale The locale that represents the language of the text, or <code>null</code> if no language should be indicated.
	@exception NullPointerException if the given text is <code>null</code>.
	*/
	public LocaledText(final String text, final Locale locale) throws NullPointerException
	{
		super(locale);	//construct the parent class
		this.text=checkInstance(text, "Text cannot be null.");	//set the text
	}

	/**Converts an array of strings to an array of <code>LocaleText</code>s.
	@param strings The array of strings to be converted to an array of
		<code>LocaleText</code>s.
	@param locale The locale that represents the language of the text, or
		<code>null</code> if no language should be indicated.
	*/
	public static LocaledText[] toLocaleTextArray(final String[] strings, final Locale locale)
	{
		final LocaledText[] localeTexts=new LocaledText[strings.length];	//create an array the same size as the string array
		for(int i=strings.length-1; i>=0; --i)	//look at each string
		{
			localeTexts[i]=new LocaledText(strings[i], locale);	//create a LocaleText from this string and the locale
		}
		return localeTexts;	//return the array of LocaleTexts
	}

	/**Determines if the given object is a {@link LocaledText} with the same text and locale.
	@param object The object with which to compare this object; should be {@link LocaledText}.
	@return <code>true<code> if this object equals that specified in <var>object</var>.
	@see #getLocale()
	@see #getText()
	*/
	public boolean equals(final Object object)
	{
		if(object instanceof LocaledText)	//if we're being compared with another object of the same type
		{
			final LocaledText localeText=(LocaledText)object;	//get the object as a LocaleText
				//compare the text and locale, taking into account that the locale may be null
			return getText().equals(localeText.getText()) && Objects.equals(getLocale(), localeText.getLocale());
		}
		return false;	//if the object isn't a LocaleText, it isn't equal
	}

	/**Compares this object to another object.
	This method determines order based upon the text and locale of the object.
	If both objects have the same text but one locale is <code>null</code>, the object with the <code>null</code> locale is considered less than the other.
	@param object The object with which to compare the component. This must be
		another {@link LocaledText} object.
	@return A negative integer, zero, or a positive integer as this text and locale is
		less than, equal to, or greater than the text and locale of the specified object,
		respectively.
	@exception ClassCastException if the specified object's type prevents it from being compared to this object.
	@see #getLocale()
	@see #getText()
	*/
	public int compareTo(final LocaledText localeText) throws ClassCastException
	{
		int result=getText().compareTo(localeText.getText());	//compare text
		if(result==0)	//if the text matches
		{
			final Locale locale=getLocale();	//get our locale
			final Locale compareToLocale=localeText.getLocale();	//get the locale with which we are doing the comparison
			if(locale==null)	//if we have no locale 
			{
				result=compareToLocale==null ? 0 : -1;	//we are either less than or equal to the other locale
			}
			else	//if we do have a locale
			{
				return compareToLocale==null ? 1 : locale.toString().compareTo(compareToLocale.toString());	//we're either greater than a null locale, or we can compare the two locales
			}			
		}
		return result;	//return the result of the comparison
	}

	/**@return A hash code value composed from the text and the locale.*/
	public int hashCode()
	{
		return Objects.getHashCode(getText(), getLocale());	//create a hash code from the text and locale
	}

	/**@return  the number of <code>char</code>s in this sequence.*/
	public int length()
  {
  	return getText().length();	//delegate to the text
  }

  /**Returns the <code>char</code> value at the specified index.
	@param index The index of the <code>char</code> value to be returned.
  @return The specified <code>char</code> value.
	@throws IndexOutOfBoundsException if the <var>index</var> argument is negative or not less than {@link #length()}.
	*/
	public char charAt(final int index)
	{
		return getText().charAt(index);	//delegate to the text
	}

	/**Returns a new {@link CharSequence} that is a subsequence of this sequence.
	@param start The start index, inclusive.
	@param end The end index, exclusive.
	@return The specified subsequence.
	@throws IndexOutOfBoundsException
	<ul>
		<li>if <var>start</var> or <var>end</var> are negative,</li>
		<li>if <var>end</var> is greater than {@link #length()},</li>
		<li>or if <var>start</var> is greater than <var>end</var></li>
	</ul>
	*/
	public CharSequence subSequence(final int start, final int end)
	{
		return getText().subSequence(start, end);	//delegate to the text
	}

	/**@return A string representation of this object's text.
	@see #getText()
	*/
	public final String toString()	//this is just a wrapper around a string with some metadata; the original string should always be returned from this method
	{
		return getText();	//return the text
	}

}
