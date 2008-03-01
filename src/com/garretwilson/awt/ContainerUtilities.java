package com.garretwilson.awt;

import java.awt.*;

import com.globalmentor.util.*;

/**Utility methods for working with AWT containers.
@author Garret Wilson
*/
public class ContainerUtilities
{

	/**Sets the modified property of all components that are children of the
		given container and that implement <code>Modifiable</code>. Any
		children that do not implement <code>Modifiable</code> will recursively
		be searched for children that do implement <code>Modifiable</code>.
	<p>This method is useful for a container that has had its content saved and
		wishes to express that all its content, including that of its children,
		are now unmodified. Note that this method is not useful for the converse
		situation of setting modified to <code>true</code>&mdash;just because one
		component has been modified does not mean that each of its children has had
		their content modified.</p>
	@param container The container the children of which to set the modified status.
	@param newModified The new modification status.
	@see Modifiable#setModified(boolean)
	*/
	public static void setModifiableDescendants(final Container container, final boolean modified)
	{
		final Component[] components=container.getComponents();	//get all child components
		for(int i=components.length-1; i>=0; --i)	//look at each child component
		{
			final Component component=components[i];	//get a reference to this child component
			if(component instanceof Modifiable)	//if this child component is modifiable
			{
				((Modifiable)component).setModified(modified);	//change the modification status of the child component
			}
			else if(component instanceof Container)	//if this child isn't modifiable, but it is a container (a tab set component, for example)
			{
				setModifiableDescendants((Container)component, modified);	//see if any of the descendants of the child component are modifiable
			}
		}
	}

	/**Verifies each child component that implements <code>Verifiable</code>.
		If any child does not implement <code>Verifiable</code>, its children
		will be searched for components that implements <code>Verifiable</code>.
	@param container The container the children of which to verify.
	@return <code>true</code> if all the child component contents are
		valid&mdash;if no descendant that implements <code>Verifiable</code>
		returns <code>false</code>.
	@see Verifiable#verify()
	*/
	public static boolean verifyDescendants(final Container container)
	{
		final Component[] components=container.getComponents();	//get all child components
		for(int i=components.length-1; i>=0; --i)	//look at each child component
		{
			final Component component=components[i];	//get a reference to this component
			if(component instanceof Verifiable)	//if this component is verifiable
			{
				if(((Verifiable)component).verify()==false)	//if this component doesn't verify
				{
					return false;	//show that a child component didn't verify
				}
			}
			else if(component instanceof Container)	//if this child isn't verifiable, but it is a container (a tab set component, for example)
			{
				if(!verifyDescendants((Container)component))	//try to verify the descendants of this container---if one of them doesn't verify
				{
					return false;	//show that a child component didn't verify					
				}
			}
		}
		return true;  //if we couldn't find any problems, verification succeeded
	}

}
