package com.globalmentor.text.directory.vcard;

import java.net.*;
import java.util.*;
import static java.util.Collections.*;
import static com.globalmentor.text.directory.vcard.VCardConstants.*;

import com.globalmentor.text.directory.*;
import com.globalmentor.util.*;

/**An object representing a vCard <code>text/directory</code> profile as defined
	in <a href="http://www.ietf.org/rfc/rfc2426.txt">RFC 2426</a>,
	"vCard MIME Directory Profile".
@author Garret Wilson
*/
public class VCard extends Directory
{
	
	/**The list of content lines that represent unrecognized and/or unprocessed information.*/
	private final List<ContentLine> contentLineList=new ArrayList<ContentLine>();

		/**@return The list of content lines that represent unrecognized and/or unprocessed information.*/
		public List<ContentLine> getContentLineList() {return contentLineList;}

			//identification types

	/**Formatted text corresponding to the name of the object the vCard represents.*/
	private LocaledText formattedName=null;

		/**@return Formatted text corresponding to the name of the object the vCard represents.*/
		public LocaledText getFormattedName() {return formattedName;}

		/**Sets  the ormatted text corresponding to the name of the object the vCard represents.
		@param fn The formatted name.
		*/
		public void setFormattedName(final LocaledText fn) {formattedName=fn;}

	/**The components of the name of the object the vCard represents.*/
	private Name name=null;

		/**@return The components of the name of the object the vCard represents.*/
		public Name getName() {return name;}

		/**Sets the components of the name of the object the vCard represents.
		@param n The name.
		*/
		public void setName(final Name n) {name=n;}

	/**The list of text corresponding to the nickname of the object the vCard represents.*/
	private final List<LocaledText> nicknameList=new ArrayList<LocaledText>();

		/**@return The list of text corresponding to the nickname of the object the
			vCard represents.
		*/
		public List<LocaledText> getNicknameList() {return nicknameList;}

		/**Sets the text corresponding to the nickname of the object the vCard represents.
		@param nicknames The nicknames.
		*/
		public void setNicknames(final LocaledText[] nicknames)
		{
			nicknameList.clear();	//clear the list
			addAll(nicknameList, nicknames);	//add the new values
		}
		
//TODO add birthday

			//delivery addressing types

	/**The list of componentized delivery addresses for the vCard object.*/
	private final List<Address> addressList=new ArrayList<Address>();

		/**@return The list of componentized delivery addresses for the vCard object.
		@see Address
		*/
		public List<Address> getAddressList() {return addressList;}

		/**@return An array of the delivery addresses for the vCard object.*/
		public Address[] getAddresses()
		{
			return getAddressList().toArray(new Address[getAddressList().size()]);	//return an array version of the address list
		}

		/**Sets the components of the delivery addresss for the vCard object.
		@param addresses The array of address.
		*/
		public void setAddresses(final Address[] addresses)
		{
			addressList.clear();	//clear the list
			addAll(addressList, addresses);	//add the new values
		}

	/**The list of formatted text corresponding to delivery addresses of the object the vCard represents.*/
	private final List<Label> labelList=new ArrayList<Label>();

		/**@return The list of formatted text corresponding to delivery addresses of the
			object the vCard represents.
		*/
		public List<Label> getLabelList() {return labelList;}

		/**@return An array of the delivery address labels for the vCard object.*/
		public Label[] getLabels()
		{
			return getLabelList().toArray(new Label[getLabelList().size()]);	//return an array version of the label list
		}

		/**Sets the formatted text corresponding to delivery addresses of the
			object the vCard represents.
		@param labels The array of delivery address labels.
		*/
		public void setLabels(final Label[] labels)
		{
			labelList.clear();	//clear the list
			addAll(labelList, labels);	//add the new values
		}
		
			//telecommunications addressing types

	/**The list of telephone numbers for telephony communication with the object the vCard represents.*/
	private final List<Telephone> telephoneList=new ArrayList<Telephone>();

		/**@return The list of telephone numbers for telephony communication with the
			object the vCard represents.
		@see Telephone
		*/
		public List<Telephone> getTelephoneList() {return telephoneList;}

		/**Sets the telephone numbers for telephony communication with the
			object the vCard represents.
		@param telephones The array of telephones.
		*/
		public void setTelephones(final Telephone[] telephones)
		{
			telephoneList.clear();	//clear the list
			addAll(telephoneList, telephones);	//add the new values
		}

	/**The list of electronic mail addresses for communication with the object the vCard represents.*/
	private final List<Email> emailList=new ArrayList<Email>();

		/**@return The list of electronic mail addresses for communication with the
			object the vCard represents.
		@see Email
		*/
		public List<Email> getEmailList() {return emailList;}

		/**Sets the electronic mail addresses for communication with the object
			the vCard represents.
		@param emails The array of email addresses.
		*/
		public void setEmails(final Email[] emails)
		{
			emailList.clear();	//clear the list
			addAll(emailList, emails);	//add the new values
		}

//TODO add geographical types

			//organizational types

	/**The organization name.*/
	private LocaledText organizationName=null;

		/**@return The organization name, or <code>null</code> for no name.*/
		public LocaledText getOrganizationName() {return organizationName;}

		/**Sets the organization name.
		@param org The name of the organization, or <code>null</code> for no name.
		*/
		public void setOrganizationName(final LocaledText org) {organizationName=org;}

	/**The organizational units.*/
	private LocaledText[] organizationUnits=new LocaledText[]{};

		/**@return The organizational units.*/
		public LocaledText[] getOrganizationUnits() {return organizationUnits;}

		/**@return The first organizational unit, or <code>null</code> if there are no organizational units.*/
		public LocaledText getOrganizationUnit() {return organizationUnits.length>0 ? organizationUnits[0] : null;}
		
		/**Sets the organizational units.
		@param units The organizational units.
		*/
		public void setOrganizationUnits(final LocaledText[] units) {organizationUnits=units;}
		
		/**Sets the organizational unit.
		@param unit The organizational unit, or <code>null</code> for no organizational unit.
		*/
		public void setOrganizationUnit(final LocaledText unit) {setOrganizationUnits(unit!=null ? new LocaledText[]{unit} : null);}

	/**The job title, functional position or function at the organization.*/
	private LocaledText title=null;

		/**@return The job title, functional position or function at the,
			organization or <code>null</code> for no title.
		*/
		public LocaledText getTitle() {return title;}

		/**Sets the job title.
		@param title The job title, functional position or function at the
			organization, or <code>null</code> for no title.
		*/
		public void setTitle(final LocaledText title) {this.title=title;}

	/**The role, occupation, or business category at the organization.*/
	private LocaledText role=null;

		/**@return The role, occupation, or business category at the
			organization, or <code>null</code> for no role.
		*/
		public LocaledText getRole() {return role;}

		/**Sets the role.
		@param role The role, occupation, or business category at the organization,
			or <code>null</code> for no role.
		*/
		public void setRole(final LocaledText role) {this.role=role;}

			//explanatory types

	/**The list of application category information about the vCard.*/
	private final List<LocaledText> categoryList=new ArrayList<LocaledText>();

		/**@return The list of application category information about the vCard.*/
		public List<LocaledText> getCategoryList() {return categoryList;}

		/**Adds the application category information about the vCard.
		@param categories The array of categories to add.
		*/
		public void addCategories(final LocaledText[] categories)
		{
			addAll(categoryList, categories);	//add the new values
		}

		/**Sets the application category information about the vCard.
		@param emails The array of categories.
		*/
		public void setCategories(final LocaledText[] categories)
		{
			categoryList.clear();	//clear the list
			addCategories(categories);	//add the new values
		}

	/**The supplemental information or a comment that is associated with the vCard.*/
	private LocaledText note=null;

		/**@return The supplemental information or a comment that is associated
			with the vCard, or <code>null</code> if there is no note.
		*/
		public LocaledText getNote() {return note;}

		/**Sets the supplemental information or a comment that is associated
			with the vCard.
		@param note The supplemental information or a comment, or <code>null</code>
			if there is no note.
		*/
		public void setNote(final LocaledText note) {this.note=note;}

	/**The national-language-specific sorting string.*/
	private LocaledText sortString=null;

		/**@return The national-language-specific sorting string, or
			<code>null</code> if there is no sorting string specified.
		*/
		public LocaledText getSortString() {return sortString;}

		/**Sets the national-language-specific sorting string associated
			with the vCard.
		@param sortingString The sorting string, or <code>null</code>
			if there should be no sorting string.
		*/
		public void setSortString(final LocaledText sortString) {this.sortString=sortString;}

//TODO add other explanatory types

	/**The URL associated with the vCard*/
	private URI url=null;

		/**@return The URL that is associated with the vCard, or <code>null</code>
			if there is no URL.
		*/
		public URI getURL() {return url;}

		/**Sets the URL that is associated with the vCard.
		@param url The URL, or <code>null</code> if there is no URL.
		*/
		public void setURL(final URI url) {this.url=url;}

	/**The the version of the vCard specification used to format this vCard.*/
	private String version=VCARD_VERSION_VALUE;

		/**@return The the version of the vCard specification used to format this
			vCard, which defaults to "3.0".
		*/
		public String getVersion() {return version;}

		/**Sets the the version of the vCard specification used to format this vCard.
		@param version The version of the vCard specification used to format this vCard.
		*/
		public void setVersion(final String version) {this.version=version;}

}
