package com.garretwilson.text.directory.vcard;

import java.util.*;
import com.garretwilson.util.*;

/**An object representing a vCard <code>text/directory</code> profile as defined
	in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class VCard
{
	
	/**The list of content lines that represent unrecognized and/or unprocessed information.*/
	private final List contentLineList=new ArrayList();

		/**@return The list of content lines that represent unrecognized and/or unprocessed information.*/
		public List getContentLineList() {return contentLineList;}

			//identification types

	/**Formatted text corresponding to the name of the object the vCard represents.*/
	private LocaleText formattedName=null;

		/**@return Formatted text corresponding to the name of the object the vCard represents.*/
		public LocaleText getFormattedName() {return formattedName;}

		/**Sets  the ormatted text corresponding to the name of the object the vCard represents.
		@param fn The formatted name.
		*/
		public void setFormattedName(final LocaleText fn) {formattedName=fn;}

	/**The components of the name of the object the vCard represents.*/
	private Name name=null;

		/**@return The components of the name of the object the vCard represents.*/
		public Name getName() {return name;}

		/**Sets the components of the name of the object the vCard represents.
		@param n The name.
		*/
		public void setName(final Name n) {name=n;}

	/**The list of text corresponding to the nickname of the object the vCard represents.*/
	private final List nicknameList=new ArrayList();

		/**@return The list of text corresponding to the nickname of the object the
			vCard represents.
		*/
		public List getNicknameList() {return nicknameList;}

		/**Sets the text corresponding to the nickname of the object the vCard represents.
		@param nicknames The nicknames.
		*/
		public void setNicknames(final LocaleText[] nicknames)
		{
			nicknameList.clear();	//clear the list
			CollectionUtilities.addAll(nicknameList, nicknames);	//add the new values
		}
		
//TODO add birthday

			//delivery addressing types

	/**The list of componentized delivery addresses for the vCard object.*/
	private final List addressList=new ArrayList();

		/**@return The list of componentized delivery addresses for the vCard object.
		@see Address
		*/
		public List getAddressList() {return addressList;}

		/**Sets the components of the delivery addresss for the vCard object.
		@param addresses The array of address.
		*/
		public void setAddresses(final Address[] addresses)
		{
			addressList.clear();	//clear the list
			CollectionUtilities.addAll(addressList, addresses);	//add the new values
		}

	/**The list of formatted text corresponding to delivery addresses of the object the vCard represents.*/
	private final List labelList=new ArrayList();

		/**@return The list of formatted text corresponding to delivery addresses of the
			object the vCard represents.
		*/
		public List getLabelList() {return labelList;}

		/**Sets the formatted text corresponding to delivery addresses of the
			object the vCard represents.
		@param labels The array of delivery address labels.
		*/
		public void setLabels(final LocaleText[] labels)
		{
			labelList.clear();	//clear the list
			CollectionUtilities.addAll(labelList, labels);	//add the new values
		}

			//telecommunications addressing types

	/**The list of telephone numbers for telephony communication with the object the vCard represents.*/
	private final List telephoneList=new ArrayList();

		/**@return The list of telephone numbers for telephony communication with the
			object the vCard represents.
		@see Telephone
		*/
		public List getTelephoneList() {return telephoneList;}

		/**Sets the telephone numbers for telephony communication with the
			object the vCard represents.
		@param telephones The array of telephones.
		*/
		public void setTelephones(final Telephone[] telephones)
		{
			telephoneList.clear();	//clear the list
			CollectionUtilities.addAll(telephoneList, telephones);	//add the new values
		}

	/**The list of electronic mail addresses for communication with the object the vCard represents.*/
	private final List emailList=new ArrayList();

		/**@return The list of electronic mail addresses for communication with the
			object the vCard represents.
		@see Email
		*/
		public List getEmailList() {return emailList;}

		/**Sets the electronic mail addresses for communication with the object
			the vCard represents.
		@param emails The array of email addresses.
		*/
		public void setEmails(final Email[] emails)
		{
			emailList.clear();	//clear the list
			CollectionUtilities.addAll(emailList, emails);	//add the new values
		}

//TODO add geographical types

			//organizational types

	/**The organization name.*/
	private LocaleText organizationName=null;

		/**@return The organization name, or <code>null</code> for no name.*/
		public LocaleText getOrganizationName() {return organizationName;}

		/**Sets the organization name.
		@param org The name of the organization, or <code>null</code> for no name.
		*/
		public void setOrganizationName(final LocaleText org) {organizationName=org;}

	/**The organizational units.*/
	private LocaleText[] organizationUnits=new LocaleText[]{};

		/**@return The organizational units.*/
		public LocaleText[] getOrganizationUnits() {return organizationUnits;}

		/**@return The first organizational unit, or <code>null</code> if there are no organizational units.*/
		public LocaleText getOrganizationUnit() {return organizationUnits.length>0 ? organizationUnits[0] : null;}
		
		/**Sets the organizational units.
		@param units The organizational units.
		*/
		public void setOrganizationUnits(final LocaleText[] units) {organizationUnits=units;}
		
		/**Sets the organizational unit.
		@param unit The organizational unit, or <code>null</code> for no organizational unit.
		*/
		public void setOrganizationUnit(final LocaleText unit) {setOrganizationUnits(unit!=null ? new LocaleText[]{unit} : null);}

	/**The job title, functional position or function at the organization.*/
	private LocaleText title=null;

		/**@return The job title, functional position or function at the,
			organization or <code>null</code> for no title.
		*/
		public LocaleText getTitle() {return title;}

		/**Sets the job title.
		@param title The job title, functional position or function at the
			organization, or <code>null</code> for no title.
		*/
		public void setTitle(final LocaleText title) {this.title=title;}

	/**The role, occupation, or business category at the organization.*/
	private LocaleText role=null;

		/**@return The role, occupation, or business category at the
			organization, or <code>null</code> for no role.
		*/
		public LocaleText getRole() {return role;}

		/**Sets the role.
		@param role The role, occupation, or business category at the organization,
			or <code>null</code> for no role.
		*/
		public void setRole(final LocaleText role) {this.role=role;}

			//explanatory types

	/**The list of application category information about the vCard.*/
	private final List categoryList=new ArrayList();

		/**@return The list of application category information about the vCard.*/
		public List getCategoryList() {return categoryList;}

		/**Adds the application category information about the vCard.
		@param categories The array of categories to add.
		*/
		public void addCategories(final LocaleText[] categories)
		{
			CollectionUtilities.addAll(categoryList, categories);	//add the new values
		}

		/**Sets the application category information about the vCard.
		@param emails The array of categories.
		*/
		public void setCategories(final LocaleText[] categories)
		{
			categoryList.clear();	//clear the list
			addCategories(categories);	//add the new values
		}

	/**The supplemental information or a comment that is associated with the vCard*/
	private LocaleText note=null;

		/**@return The supplemental information or a comment that is associated
			with the vCard, or <code>null</code> if there is no note.
		*/
		public LocaleText getNote() {return note;}

		/**Sets the supplemental information or a comment that is associated
			with the vCard.
		@param note The supplemental information or a comment, or <code>null</code>
			if there is no note.
		*/
		public void setNote(final LocaleText note) {this.note=note;}

//TODO add URL

}