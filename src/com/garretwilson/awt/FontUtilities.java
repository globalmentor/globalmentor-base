package com.garretwilson.awt;

import java.awt.*;
import java.lang.ref.*;
import java.util.*;

import com.globalmentor.util.Debug;

/**Various routines for working with fonts. This class also keeps several static
	maps of references to fonts for quick lookup that will be garbage-collected
	to free memory if needed.
	<p>When needed, this class also creates an array representing an enumeration
	of family names of all fonts stored on the system. This array, once created,
	will never be released from memory.</p>
	This class is not currently thread safe.
@author Garret Wilson
*/
public class FontUtilities
{

	/**Map of font family names, keyed to either a Unicode block or a
		<code>character</code>.
	*/
	protected final static Map characterFontFamilyNameMap=new HashMap();

	/**The sorted list of available font family names.*/
	private static String[] sortedAvailableFontFamilyNameArray=null;

	/**Returns a sorted array of names of available fonts. The returned array is
		shared by other objects in the system, and should not be modified.
		The first call to this method will create and cache the array.
	@return A sorted array of names of available fonts.
	*/
	protected static String[] getAvailableFontFamilyNames()
	{
		if(sortedAvailableFontFamilyNameArray==null)  //if the array of font family names has not been created
		{
			//get the list of available font family names
			sortedAvailableFontFamilyNameArray=GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
			Arrays.sort(sortedAvailableFontFamilyNameArray);  //sort the array of font family names
		}
		return sortedAvailableFontFamilyNameArray;  //return the array of font family names
	}

	/**The shared precreated key used for searching for fonts in the cache.*/
	private transient static FontKey searchFontKey=new FontKey(null, 0, 0);

	/**A synchronized map of references to fonts that have been loaded.*/
	protected transient static Map fontReferenceMap=new HashMap();

	/**This class cannot be publicly instantiated.*/
	private FontUtilities()
	{
	}

	/**Default constructor which queries available fonts.*/
/*G***fix
	public XMLCSSStyleContext()
	{
		if(sortedAvailableFontFamilyNameArray==null)  //if the array of font family names has not been created
		{
			//get the list of available font family names
			sortedAvailableFontFamilyNameArray=GraphicsEnvironment.getLocalGraphicsEnvironment().getAvailableFontFamilyNames();
			Arrays.sort(sortedAvailableFontFamilyNameArray);  //sort the array of font family names
		}
	}
*/

	/**Gets a new font. This returns a <code>Font</code> from a cache if a cached
		font exists and its memory has not been reclaimed. The the font has not
		been created, or its memory has been reclaimed, a new font is created and a
		soft reference to the font is added to the cache.
		This method is not thread safe.
	@param family The font family (such as "Monospaced").
	@param style The style of the font (such as <code>Font.PLAIN</code>).
	@param size The point size (>=1).
	@return A font with the given parameters, from the cache if possible.
	*/
	public static Font getFont(final String family, final int style, final int size)
	{
/*G***del
Debug.trace("Get tont family: ", family);  //G***del
Debug.trace("Get font style italic: ", style & Font.ITALIC);
Debug.trace("Get font style bold: ", style & Font.BOLD);
*/
		searchFontKey.setValue(family, style, size);  //set the values of the font key for searching
		final Reference fontReference=(Reference)fontReferenceMap.get(searchFontKey); //see if this font is already in the map
		Font font=null;  //we'll assign a font to this variable; assume at first that we don't have a font
		if(fontReference!=null) //if we had the font at one time in the cache
		{
			font=(Font)fontReference.get(); //see if the font is still there
			if(font==null)  //if the memory has been reclaimed
			{
			  fontReferenceMap.remove(searchFontKey); //remove the key from the map, since it doesn't contain a reference to a font anymore
			}
		}
		if(font==null)  //if we didn't have a font cached, or if we did but its memory has been reclaimed
		{
		  font=new Font(family, style, size); //create a new font with the desired characteristics
		  final FontKey fontKey=new FontKey(family, style, size); //create a new font key to represent the font
		  fontReferenceMap.put(fontKey, new SoftReference(font));  //store a soft reference to the font in the map, so that the font can be reclaimed, if necessary
		}
/*G***del
if(font!=null)  //G***del
{
Debug.trace("Font family: ", font.getFamily()); //G***del
Debug.trace("Font name: ", font.getFontName()); //G***del
}
*/
		return font;  //return the font we found in the map or created
	}

	/**Gets a new font for the specified character by searching all available fonts
	 	if the character is not supported by the provided font.
	If no font supports the character, the preferred font is returned.
	@param c The character for which a font should be returned.
	@param suggestedFont The preferred font.
	@return The new font, or <code>null</code> if a font could not be found that
		matched this character.
	*/
	public static Font getFont(final char c, final Font preferredFont)
	{
		if(preferredFont.canDisplay(c)) //if the font can display the character
		{
			return preferredFont; //return the font
		}
		else	//if the font cannot display the character
		{
			final Font font=getFont(c, preferredFont.getStyle(), preferredFont.getSize());	//get a font of the same style and size that supports this character
			return font!=null ? font : preferredFont;	//return the new font or, if there is no new font, the preferred font
		}
	}

	/**Gets a new font for the specified character by searching all available fonts.
	<p>Once a font is found for a particular character, that font family name is
		stored with the character's Unicode block so that retrieving a font for the
		same block character will be much faster in the future. The name will also
		be stored keyed to the character itself. If a character is requested for the
		same Unicode block in the future but that specific character is not available,
		the font family name will be searched with the character as the key. This
		speeds searches yet saves space based on the assumption that most fonts, if
		they contain one character in a Unicode block, will contain all characters
		in a Unicode block.</p>
	<p>If no fonts on the system support the given character, this method will
		flag that character as unsupported so that during the next call to this
		method all the fonts will not be searched.</p>		
	<p>This method also keeps a list of recommended fonts for several character
		blocks so that those fonts may be searched first, if the character is not
		represented in the map.</p>
	@param c The character for which a font should be returned.
	@param style The style of the font (such as <code>Font.PLAIN</cod>).
	@param size The point size (>=1).
	@return The new font, or <code>null</code> if a font could not be found that
		matched this character.
	*/
	public static Font getFont(final char c, final int style, final int size)
	{
if(c<32)  //G***testing; used to get around the initial '\n' stored in a paragraph; fix
	return null;
/*G***del
Debug.trace("Font character: "+c);  //G***del
Debug.trace("Font style italic: ", style & Font.ITALIC);
Debug.trace("Font style bold: ", style & Font.BOLD);
*/

//G***del Debug.trace("Looking for character "+Integer.toHexString(c));
		final Character character=new Character(c); //create a character object to use as a key to lookup the character in the map
		//see if we know about a font family name for this character
		final String characterFamilyName=(String)characterFontFamilyNameMap.get(character);
		if(characterFamilyName!=null) //if we found a family name for this character
		{
//G***del Debug.trace("found matching character in map");
		  final Font characterFont=getFont(characterFamilyName, style, size); //create the font for the character
			if(characterFont.canDisplay(c)) //if the font can really display the character
				return characterFont; //return the font
			else  //if the font can't display the character
				characterFontFamilyNameMap.remove(character); //remove the character key from the map; it was misleading
		}
		else	//if we didn't find a family name, this might mean there's no font stored for this character, or we previously stored null, meaning there is no supporting font
		{
			if(characterFontFamilyNameMap.containsKey(character))	//if we actually stored the null value keyed to this character
			{
				return null;	//there's no use searching further---we've searched for this character before, and turned up nothing
			}			
		}
		//see which Unicode block this character is in
		final Character.UnicodeBlock unicodeBlock=Character.UnicodeBlock.of(c);	//TODO user our own Unicode block implementation
//G***del Debug.trace("character in unicode block: "+unicodeBlock); //G***del
		//see if we know about a font family name for this block
		final String blockFamilyName=(String)characterFontFamilyNameMap.get(unicodeBlock);
		if(blockFamilyName!=null) //if we found a family name for this character
		{
//G***del Debug.trace("found matching Unicode block");
		  final Font blockFont=getFont(blockFamilyName, style, size); //create the font for the Unicode block
			if(blockFont.canDisplay(c)) //if the font can really display the character
				return blockFont; //return the font
			else  //if the font can't display the character
				characterFontFamilyNameMap.remove(unicodeBlock); //remove the unicode block key from the map; it was misleading
		}
		Font chosenFont=null; //if we can't find the font in the map, we'll try to find one
		  //try suggestions
		String[] possibleFontFamilyNames=null; //we'll determine several font family names to try
//G***fix for Arabic				if(unicodeBlock.equals(Character.UnicodeBlock.ARABIC) ||  //if this is an unrecognized Arabic character G***what about the other Arabic sections?
//G***fix for Arabic					return getFont(childView, "Lucinda Sans Regular", font.getStyle(), font.getSize()); //use the font we know can display this character correctly G***fix, use constants G***what about the font.getSize2D()?
//G***fix for Arabic				if(unicodeBlock.equals(Character.UnicodeBlock.ARABIC))  //if this is an unrecognized Arabic character
//G***fix for Arabic					return getFont(childView, "Lucinda Sans Regular", font.getStyle(), font.getSize()); //use the font we know can display this character correctly G***fix, use constants G***what about the font.getSize2D()?
		  //G***add Batang and others to the last-resort fonts
		//TODO should these be "Lucida Sans"?
		if(unicodeBlock.equals(Character.UnicodeBlock.ARROWS))  //if this is an arrow
			possibleFontFamilyNames=new String[]{"Lucida Sans Regular", "Berling Antiqua", "Batang"};  //show which font family names we want to try G***use a pre-created static version
		else if(unicodeBlock.equals(Character.UnicodeBlock.LETTERLIKE_SYMBOLS))  //if this is a letter-like symbol
			possibleFontFamilyNames=new String[]{"Lucida Sans Regular", "Berling Antiqua"};  //show which font family names we want to try G***use a pre-created static version
		else if(unicodeBlock.equals(Character.UnicodeBlock.GENERAL_PUNCTUATION))  //if this is general punctuation
			possibleFontFamilyNames=new String[]{"Berling Antiqua", "Lucida Sans Regular"};  //show which font family names we want to try G***use a pre-created static version
		else  //if we have no suggestions
		{
Debug.trace("Font cannot support character: "+Integer.toHexString(c)+", but we have no suggestions");

//G***add lookup for "MS Hei" to the appropriate Unicode blocks, such as for 0x4F60
			possibleFontFamilyNames=new String[]{"Lucida Sans Regular", "Code2000", "Batang", "MS Hei"};  //always try the installed font, along with Code2000 G***use a pre-created static version G***see which fonts Batang contains -- it has, for example, 0x5DE5, which Code2000 does not
		}
			//try the suggested fonts based upon the Unicode block G***we might want to make sure each font is available on the system, first
		if(possibleFontFamilyNames!=null) //if know of several font family names to try
		{
Debug.trace("Font cannot support character: "+Integer.toHexString(c)+"; trying possible fonts");
			for(int i=0; i<possibleFontFamilyNames.length; ++i)  //look at each font family name
			{
				final Font possibleFont=new Font(possibleFontFamilyNames[i], style, size); //create a font with the possible name, but don't get it using getFont() because we're not sure we want to add it to our cache
				if(possibleFont.canDisplay(c))  //if the font can display the character
				{
Debug.trace("Character "+Integer.toHexString(c)+" used suggested font: "+possibleFont);
					chosenFont=getFont(possibleFontFamilyNames[i], style, size); //choose a font after getting it with getFont(), which will add it to our cache for next time
//G***del				  chosenFont=possibleFont;  //show that we've chosen a font
					break;  //stop searching
//G***del					return possibleFont;  //return the font
				}
			}
		}
		//if none of the suggested fonts contain the character we want, search all available fonts
		//(this takes some time and should be done only as a last resort)
		if(chosenFont==null)  //if we haven't found a font, yet
		{
			final String[] availableFontFamilyNames=getAvailableFontFamilyNames(); //get the available font family names
			for(int i=0; i<availableFontFamilyNames.length; ++i)  //look at each available font
			{
				final Font availableFont=new Font(availableFontFamilyNames[i], style, size); //create a the available font, but don't get it using getFont() because we're not sure we want to add it to our cache
Debug.trace("trying font: ", availableFont);  //G***del
//G***del				final Font availableFont=getFont(availableFontFamilyNames[i], style, size); //create the available font
				if(availableFont.canDisplay(c))  //if the font can display the character
				{
Debug.trace("Character "+Integer.toHexString(c)+" not found; had to search available fonts, found: "+availableFont);
					chosenFont=getFont(availableFontFamilyNames[i], style, size); //choose a font after getting it with getFont(), which will add it to our cache for next time
//G***del				  chosenFont=availableFont;  //show that we've chosen a font
					break;  //stop searching
//G***del					return availableFont;  //return the font
				}
			}
		}
		if(chosenFont!=null)  //if we found a font
		{
//G***del Debug.trace("Storing character keyed to character: "+character);
			characterFontFamilyNameMap.put(character, chosenFont.getFamily()); //store the family name in the map keyed to the character
//G***del Debug.trace("Stored object: "+characterFontFamilyNameMap.get(character)); //G***del; testing
//G***del Debug.trace("Stored object from new character: "+characterFontFamilyNameMap.get(new Character(character.charValue())));
			characterFontFamilyNameMap.put(unicodeBlock, chosenFont.getFamily()); //store the name in the map keyed to the Unicode block
Debug.trace("Finally chose font: ", chosenFont); //G***del
		  return chosenFont;  //return the font we chose
		}
		else  //if we could not find a font for the character
		{
			characterFontFamilyNameMap.put(character, null); //remind ourselves next time that we've tried all fonts and couldn't find any to work with this character
			return null;  //show that we could not find a font that can display the specified character
		}
	}

	//G***create the transient fontReferenceMap in a readObject

	/**Constructs transient variables before the object is read from a stream.
	@param objectInputStream The stream from which the object is being read.
	*/
/*G***del
	private void readObject(ObjectInputStream objectInputStream) throws ClassNotFoundException, IOException
	{
		searchFontKey=new FontKey(null, 0, 0);  //create a new font key for searching
		fontReferenceMap=new HashMap(); //create a new map of references to fonts
		objectInputStream.defaultReadObject();  //read the object normally
	}
*/

	/**A key for storing fonts in the font table.
		Modified from <code>javax.swing.text.StyleContext.FontKey</code> version,
		1.65 02/02/00 by Timothy Prinzing, Copyright 1997-2000 Sun Microsystems,
		Inc., because that version is private and inaccessible.
	*/
	protected static class FontKey
	{
		/**The family name of the font.*/
		private String family;

		/**The font's style.*/
		private int style;

		/**The size of the font.*/
		private int size;

		/**Constructs a font key.
		@param fontFamily The font family name.
		@param fontStyle The font style.
		@param fontSize The font size.
		*/
		public FontKey(final String fontFamily, final int fontStyle, final int fontSize)
		{
			setValue(fontFamily, fontStyle, fontSize);  //set the values of the key
		}

		/**Sets the values of the font key.
		@param fontFamily The font family name.
		@param fontStyle The font style.
		@param fontSize The font size.
		*/
		public void setValue(String fontFamily, int fontStyle, int fontSize)
		{
			family=fontFamily!=null ? fontFamily.intern() : null; //save a canonical pooled version of the family string for quick comparison
		  style=fontStyle;  //save the font style
		  size=fontSize;  //save the font size
		}

		/**@return A hashcode value for the font key.*/
		public int hashCode()
		{
		  return family.hashCode()^style^size;  //construct a hash code and return it
		}

		/**Compares this font key to the specifed object.
		  The result is <code>true</code> if and only if the argument is not
		  <code>null</code> and is a <code>FontKey</code> object with the same
		  name, style, and point size as this font key.
		@param  object The object with which to compare this font key.
		@return <code>true</code> if the objects are equal; <code>false</code> otherwise.
		*/
		public boolean equals(Object object)
		{
		  if(object instanceof FontKey)  //if this is another font key
			{
				final FontKey fontKey=(FontKey)object;  //cast the object to a font key
				return fontKey.size==size&& fontKey.style==style && fontKey.family==family; //make sure the values are equal, comparing the family by reference because we used String.intern()
		  }
		  return false;
		}
	}

}