/*
 * Copyright © 1996-2011 GlobalMentor, Inc. <http://www.globalmentor.com/>
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

import java.net.*;
import java.util.*;
import static java.util.Collections.*;

import com.globalmentor.model.LocaledText;
import com.globalmentor.text.directory.*;

/**
 * An object representing a vCard <code>text/directory</code> profile as defined in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
 * "vCard MIME Directory Profile".
 * @author Garret Wilson
 */
public class VCard extends Directory
{

	/** The name of the vCard profile. */
	public final static String VCARD_PROFILE_NAME = "VCARD";

	/** The delimiter separating components of a vCard structured type (';'). */
	public final static char STRUCTURED_TEXT_VALUE_DELIMITER = 59;

	//defined types

	//identification types

	/**
	 * The required type to specify the formatted text corresponding to the name of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String FN_TYPE = "FN";

	/**
	 * The structured text type to specify the components of the name of the object the vCard represents.
	 */
	public final static String N_TYPE = "N";

	/**
	 * The type to specify the text corresponding to the nickname of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String NICKNAME_TYPE = "NICKNAME";

	/**
	 * The type to specify an image or photograph information that annotates some aspect of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#B_ENCODING_TYPE
	 * @see #BINARY_VALUE_TYPE
	 * @see com.globalmentor.text.directory.Directory#URI_VALUE_TYPE
	 */
	public final static String PHOTO_TYPE = "PHOTO";

	/**
	 * The type to specify the birth date of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#DATE_VALUE_TYPE
	 * @see com.globalmentor.text.directory.Directory#DATE_TIME_VALUE_TYPE
	 */
	public final static String BDAY_TYPE = "BDAY";

	//delivery addressing types

	/**
	 * The structured text type to specify the components of the delivery address for the vCard object.
	 */
	public final static String ADR_TYPE = "ADR";

	/** A domestic delivery address. */
	public final static String ADR_DOM_PARAM_VALUE = "dom";
	/** An international delivery address. */
	public final static String ADR_INTL_PARAM_VALUE = "intl";
	/** A postal delivery address. */
	public final static String ADR_POSTAL_PARAM_VALUE = "postal";
	/** A parcel delivery address. */
	public final static String ADR_PARCEL_PARAM_VALUE = "parcel";
	/** A delivery address for a residence. */
	public final static String ADR_HOME_PARAM_VALUE = "home";
	/** A delivery address for a place of work. */
	public final static String ADR_WORK_PARAM_VALUE = "work";
	/** The preferred delivery address. */
	public final static String ADR_PREF_PARAM_VALUE = "pref";

	/**
	 * The type to specify the formatted text corresponding to delivery address of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String LABEL_TYPE = "LABEL";

	//telecommunications addressing types

	/**
	 * The type to specify the telephone number for telephony communication with the object the vCard represents.
	 * @see #PHONE_NUMBER_VALUE_TYPE
	 */
	public final static String TEL_TYPE = "TEL";

	/** A telephone number associated with a residence. */
	public final static String TEL_HOME_PARAM_VALUE = "home";
	/** A telephone number that has voice messaging support. */
	public final static String TEL_MSG_PARAM_VALUE = "msg";
	/** A telephone number associated with a place of work. */
	public final static String TEL_WORK_PARAM_VALUE = "work";
	/** A preferred-use telephone number. */
	public final static String TEL_PREF_PARAM_VALUE = "pref";
	/** A voice telephone number. */
	public final static String TEL_VOICE_PARAM_VALUE = "voice";
	/** A facsimile telephone number. */
	public final static String TEL_FAX_PARAM_VALUE = "fax";
	/** A cellular telephone number. */
	public final static String TEL_CELL_PARAM_VALUE = "cell";
	/** A video conferencing telephone number. */
	public final static String TEL_VIDEO_PARAM_VALUE = "video";
	/** A paging device telephone number. */
	public final static String TEL_PAGER_PARAM_VALUE = "pager";
	/** A bulletin board system telephone number. */
	public final static String TEL_BBS_PARAM_VALUE = "bbs";
	/** A modem-connected telephone number. */
	public final static String TEL_MODEM_PARAM_VALUE = "modem";
	/** A car-phone telephone number. */
	public final static String TEL_CAR_PARAM_VALUE = "car";
	/** An ISDN service telephone number. */
	public final static String TEL_ISDN_PARAM_VALUE = "isdn";
	/** A personal communication services telephone number. */
	public final static String TEL_PCS_PARAM_VALUE = "pcs";

	/**
	 * The type to specify the electronic mail address for communication with the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String EMAIL_TYPE = "EMAIL";

	/** An Internet email address. */
	public final static String EMAIL_INTERNET_PARAM_VALUE = "internet";
	/** An X.400 addressing type. */
	public final static String EMAIL_X400_PARAM_VALUE = "x400";
	/** A preferred-use email address. */
	public final static String EMAIL_PREF_PARAM_VALUE = "pref";

	/**
	 * The type to specify the type of electronic mail software that is used by the individual associated with the vCard.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String MAILER_TYPE = "MAILER";

	//geographical types

	/**
	 * The type to specify information related to the time zone of the object the vCard represents.
	 * @see #UTC_OFFSET_VALUE_TYPE
	 */
	public final static String TZ_TYPE = "TZ";

	/**
	 * The structured type to specify information related to the global positioning of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#FLOAT_VALUE_TYPE
	 */
	public final static String GEO_TYPE = "GEO";

	//organizational types

	/**
	 * The type to specify the job title, functional position or function of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String TITLE_TYPE = "TITLE";

	/**
	 * The type to specify information concerning the role, occupation, or business category of the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String ROLE_TYPE = "ROLE";

	/**
	 * The type to specify a graphic image of a logo associated with the object the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#B_ENCODING_TYPE
	 * @see #BINARY_VALUE_TYPE
	 * @see com.globalmentor.text.directory.Directory#URI_VALUE_TYPE
	 */
	public final static String LOGO_TYPE = "LOGO";

	/**
	 * The type to specify information about another person who will act on behalf of the individual or resource associated with the vCard.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 * @see com.globalmentor.text.directory.Directory#URI_VALUE_TYPE
	 * @see #VCARD_VALUE_TYPE
	 */
	public final static String AGENT_TYPE = "AGENT";

	/**
	 * The structured text type to specify the organizational name and units associated with the vCard.
	 */
	public final static String ORG_TYPE = "ORG";

	//explanatory types

	/**
	 * The type to specify application category information about the vCard.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String CATEGORIES_TYPE = "CATEGORIES";

	/**
	 * The type to specify supplemental information or a comment that is associated with the vCard.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String NOTE_TYPE = "NOTE";

	/**
	 * The type to specify the identifier for the product that created the vCard object.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String PRODID_TYPE = "PRODID";

	/**
	 * The type to specify revision information about the current vCard.
	 * @see com.globalmentor.text.directory.Directory#DATE_TIME_VALUE_TYPE
	 * @see com.globalmentor.text.directory.Directory#DATE_VALUE_TYPE
	 */
	public final static String REV_TYPE = "REF";

	/**
	 * The type to specify the family name or given name text to be used for national-language-specific sorting of the FN and N types.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String SORT_STRING_TYPE = "SORT-STRING";

	/**
	 * The type To specify a digital sound content information that annotates some aspect of the vCard. By default this type is used to specify the proper
	 * pronunciation of the name type value of the vCard.
	 * @see com.globalmentor.text.directory.Directory#B_ENCODING_TYPE
	 * @see #BINARY_VALUE_TYPE
	 * @see com.globalmentor.text.directory.Directory#URI_VALUE_TYPE
	 */
	public final static String SOUND_TYPE = "SOUND";

	/**
	 * The type to specify a value that represents a globally unique identifier corresponding to the individual or resource associated with the vCard.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String UID_TYPE = "UID";

	/**
	 * The type to specify a uniform resource locator associated with the object that the vCard refers to.
	 * @see com.globalmentor.text.directory.Directory#URI_VALUE_TYPE
	 */
	public final static String URL_TYPE = "URL";

	/**
	 * The type to specify the version of the vCard specification used to format this vCard ("3.0").
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String VERSION_TYPE = "VERSION";

	/** The version of the vCard directory profile used in this implementation: "3.0". */
	public final static String VCARD_VERSION_VALUE = "3.0";

	//security types

	/**
	 * The type to specify the access classification for a vCard object.
	 * @see com.globalmentor.text.directory.Directory#TEXT_VALUE_TYPE
	 */
	public final static String CLASS_TYPE = "CLASS";

	/**
	 * The type to specify a public key or authentication certificate associated with the object that the vCard represents.
	 * @see com.globalmentor.text.directory.Directory#B_ENCODING_TYPE
	 * @see #BINARY_VALUE_TYPE
	 */
	public final static String KEY_TYPE = "KEY";

	//value types

	/**
	 * An inline, encoded binary value, encoded in the "B" encoding format.
	 * @see com.globalmentor.text.directory.Directory#B_ENCODING_TYPE
	 */
	public final static String BINARY_VALUE_TYPE = "BINARY";

	/** The type specifying another vCard. */
	public final static String VCARD_VALUE_TYPE = "vcard";

	/** The telephone number value type. */
	public final static String PHONE_NUMBER_VALUE_TYPE = "phone-number";

	/** The type specifying a signed offset from UTC. */
	public final static String UTC_OFFSET_VALUE_TYPE = "utc-offset";

	//parameters

	/** The type parameter. */
	public final static String TYPE_PARAM_NAME = "type";

	/** The list of content lines that represent unrecognized and/or unprocessed information. */
	private final List<ContentLine> contentLineList = new ArrayList<ContentLine>();

	/** @return The list of content lines that represent unrecognized and/or unprocessed information. */
	public List<ContentLine> getContentLineList()
	{
		return contentLineList;
	}

	//identification types

	/** Formatted text corresponding to the name of the object the vCard represents. */
	private LocaledText formattedName = null;

	/** @return Formatted text corresponding to the name of the object the vCard represents. */
	public LocaledText getFormattedName()
	{
		return formattedName;
	}

	/**
	 * Sets the ormatted text corresponding to the name of the object the vCard represents.
	 * @param fn The formatted name.
	 */
	public void setFormattedName(final LocaledText fn)
	{
		formattedName = fn;
	}

	/** The components of the name of the object the vCard represents. */
	private Name name = null;

	/** @return The components of the name of the object the vCard represents. */
	public Name getName()
	{
		return name;
	}

	/**
	 * Sets the components of the name of the object the vCard represents.
	 * @param n The name.
	 */
	public void setName(final Name n)
	{
		name = n;
	}

	/** The list of text corresponding to the nickname of the object the vCard represents. */
	private final List<LocaledText> nicknameList = new ArrayList<LocaledText>();

	/**
	 * @return The list of text corresponding to the nickname of the object the vCard represents.
	 */
	public List<LocaledText> getNicknameList()
	{
		return nicknameList;
	}

	/**
	 * Sets the text corresponding to the nickname of the object the vCard represents.
	 * @param nicknames The nicknames.
	 */
	public void setNicknames(final LocaledText[] nicknames)
	{
		nicknameList.clear(); //clear the list
		addAll(nicknameList, nicknames); //add the new values
	}

	//TODO add birthday

	//delivery addressing types

	/** The list of componentized delivery addresses for the vCard object. */
	private final List<Address> addressList = new ArrayList<Address>();

	/**
	 * @return The list of componentized delivery addresses for the vCard object.
	 * @see Address
	 */
	public List<Address> getAddressList()
	{
		return addressList;
	}

	/** @return An array of the delivery addresses for the vCard object. */
	public Address[] getAddresses()
	{
		return getAddressList().toArray(new Address[getAddressList().size()]); //return an array version of the address list
	}

	/**
	 * Sets the components of the delivery addresss for the vCard object.
	 * @param addresses The array of address.
	 */
	public void setAddresses(final Address[] addresses)
	{
		addressList.clear(); //clear the list
		addAll(addressList, addresses); //add the new values
	}

	/** The list of formatted text corresponding to delivery addresses of the object the vCard represents. */
	private final List<Label> labelList = new ArrayList<Label>();

	/**
	 * @return The list of formatted text corresponding to delivery addresses of the object the vCard represents.
	 */
	public List<Label> getLabelList()
	{
		return labelList;
	}

	/** @return An array of the delivery address labels for the vCard object. */
	public Label[] getLabels()
	{
		return getLabelList().toArray(new Label[getLabelList().size()]); //return an array version of the label list
	}

	/**
	 * Sets the formatted text corresponding to delivery addresses of the object the vCard represents.
	 * @param labels The array of delivery address labels.
	 */
	public void setLabels(final Label[] labels)
	{
		labelList.clear(); //clear the list
		addAll(labelList, labels); //add the new values
	}

	//telecommunications addressing types

	/** The list of telephone numbers for telephony communication with the object the vCard represents. */
	private final List<Telephone> telephoneList = new ArrayList<Telephone>();

	/**
	 * @return The list of telephone numbers for telephony communication with the object the vCard represents.
	 * @see Telephone
	 */
	public List<Telephone> getTelephoneList()
	{
		return telephoneList;
	}

	/**
	 * Sets the telephone numbers for telephony communication with the object the vCard represents.
	 * @param telephones The array of telephones.
	 */
	public void setTelephones(final Telephone[] telephones)
	{
		telephoneList.clear(); //clear the list
		addAll(telephoneList, telephones); //add the new values
	}

	/** The list of electronic mail addresses for communication with the object the vCard represents. */
	private final List<Email> emailList = new ArrayList<Email>();

	/**
	 * @return The list of electronic mail addresses for communication with the object the vCard represents.
	 * @see Email
	 */
	public List<Email> getEmailList()
	{
		return emailList;
	}

	/**
	 * Sets the electronic mail addresses for communication with the object the vCard represents.
	 * @param emails The array of email addresses.
	 */
	public void setEmails(final Email[] emails)
	{
		emailList.clear(); //clear the list
		addAll(emailList, emails); //add the new values
	}

	//TODO add geographical types

	//organizational types

	/** The organization name. */
	private LocaledText organizationName = null;

	/** @return The organization name, or <code>null</code> for no name. */
	public LocaledText getOrganizationName()
	{
		return organizationName;
	}

	/**
	 * Sets the organization name.
	 * @param org The name of the organization, or <code>null</code> for no name.
	 */
	public void setOrganizationName(final LocaledText org)
	{
		organizationName = org;
	}

	/** The organizational units. */
	private LocaledText[] organizationUnits = new LocaledText[] {};

	/** @return The organizational units. */
	public LocaledText[] getOrganizationUnits()
	{
		return organizationUnits;
	}

	/** @return The first organizational unit, or <code>null</code> if there are no organizational units. */
	public LocaledText getOrganizationUnit()
	{
		return organizationUnits.length > 0 ? organizationUnits[0] : null;
	}

	/**
	 * Sets the organizational units.
	 * @param units The organizational units.
	 */
	public void setOrganizationUnits(final LocaledText[] units)
	{
		organizationUnits = units;
	}

	/**
	 * Sets the organizational unit.
	 * @param unit The organizational unit, or <code>null</code> for no organizational unit.
	 */
	public void setOrganizationUnit(final LocaledText unit)
	{
		setOrganizationUnits(unit != null ? new LocaledText[] { unit } : null);
	}

	/** The job title, functional position or function at the organization. */
	private LocaledText title = null;

	/**
	 * @return The job title, functional position or function at the, organization or <code>null</code> for no title.
	 */
	public LocaledText getTitle()
	{
		return title;
	}

	/**
	 * Sets the job title.
	 * @param title The job title, functional position or function at the organization, or <code>null</code> for no title.
	 */
	public void setTitle(final LocaledText title)
	{
		this.title = title;
	}

	/** The role, occupation, or business category at the organization. */
	private LocaledText role = null;

	/**
	 * @return The role, occupation, or business category at the organization, or <code>null</code> for no role.
	 */
	public LocaledText getRole()
	{
		return role;
	}

	/**
	 * Sets the role.
	 * @param role The role, occupation, or business category at the organization, or <code>null</code> for no role.
	 */
	public void setRole(final LocaledText role)
	{
		this.role = role;
	}

	//explanatory types

	/** The list of application category information about the vCard. */
	private final List<LocaledText> categoryList = new ArrayList<LocaledText>();

	/** @return The list of application category information about the vCard. */
	public List<LocaledText> getCategoryList()
	{
		return categoryList;
	}

	/**
	 * Adds the application category information about the vCard.
	 * @param categories The array of categories to add.
	 */
	public void addCategories(final LocaledText[] categories)
	{
		addAll(categoryList, categories); //add the new values
	}

	/**
	 * Sets the application category information about the vCard.
	 * @param emails The array of categories.
	 */
	public void setCategories(final LocaledText[] categories)
	{
		categoryList.clear(); //clear the list
		addCategories(categories); //add the new values
	}

	/** The supplemental information or a comment that is associated with the vCard. */
	private LocaledText note = null;

	/**
	 * @return The supplemental information or a comment that is associated with the vCard, or <code>null</code> if there is no note.
	 */
	public LocaledText getNote()
	{
		return note;
	}

	/**
	 * Sets the supplemental information or a comment that is associated with the vCard.
	 * @param note The supplemental information or a comment, or <code>null</code> if there is no note.
	 */
	public void setNote(final LocaledText note)
	{
		this.note = note;
	}

	/** The national-language-specific sorting string. */
	private LocaledText sortString = null;

	/**
	 * @return The national-language-specific sorting string, or <code>null</code> if there is no sorting string specified.
	 */
	public LocaledText getSortString()
	{
		return sortString;
	}

	/**
	 * Sets the national-language-specific sorting string associated with the vCard.
	 * @param sortingString The sorting string, or <code>null</code> if there should be no sorting string.
	 */
	public void setSortString(final LocaledText sortString)
	{
		this.sortString = sortString;
	}

	//TODO add other explanatory types

	/** The URL associated with the vCard */
	private URI url = null;

	/**
	 * @return The URL that is associated with the vCard, or <code>null</code> if there is no URL.
	 */
	public URI getURL()
	{
		return url;
	}

	/**
	 * Sets the URL that is associated with the vCard.
	 * @param url The URL, or <code>null</code> if there is no URL.
	 */
	public void setURL(final URI url)
	{
		this.url = url;
	}

	/** The the version of the vCard specification used to format this vCard. */
	private String version = VCARD_VERSION_VALUE;

	/**
	 * @return The the version of the vCard specification used to format this vCard, which defaults to "3.0".
	 */
	public String getVersion()
	{
		return version;
	}

	/**
	 * Sets the the version of the vCard specification used to format this vCard.
	 * @param version The version of the vCard specification used to format this vCard.
	 */
	public void setVersion(final String version)
	{
		this.version = version;
	}

}
