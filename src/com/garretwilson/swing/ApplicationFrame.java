package com.garretwilson.swing;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import com.garretwilson.rdf.*;
import com.garretwilson.rdf.dublincore.DCUtilities;
import com.garretwilson.resources.icon.IconResources;

/**Main frame parent class for an application.
<p>If an <code>Application</code> is set for the frame, the close
	operation changes to <code>EXIT_ON_CLOSE</code>. (This is essential to work
	around a JDK bug that under certain instances runs a daemon thread that
	prevents the JVM from exiting, even if the main program thread has finished
	and the sole frame has closed.) If there is no <code>Application</code> set,
	the close operation remains the default, <code>DISPOSE_ON_CLOSE</code>.</p>
This class maintains the default close operation of
	<code>DO_NOTHING_ON_CLOSE</code> . This allows the class listen to window
	events and perform consistent <code>canClose()</code> checks for all methods
	of closing. This is all handled transparently&mdash;once closing should occur,
	the local default close operation setting is honored as normal.</p>
@author Garret Wilson
@see Application
@see SwingApplication
*/
public class ApplicationFrame extends BasicFrame
{

	/**The application this frame represents, or <code>null</code> if there is no
		application information.
	*/
	private final SwingApplication application;

		/**@return The application this frame represents, or <code>null</code> if
			there is no application information.
		*/
		public final SwingApplication getApplication() {return application;}

	/**The action for file|exit; defaults to <code>getExitAction()</code>.*/
	private Action fileExitAction;

		/**@return The action for exiting, either <code>getCloseAction()</code> or
			<code>getExitAction()</code>. The default is <code>getExitAction()</code>.
		@see #getCloseAction
		@see #getExitAction
		*/
//G***del		public Action getFileExitAction() {return fileExitAction;}
		
		
		/**Sets the action to use for file|exit.
		The default is <code>getExitAction()</code>.
		@param action The action to use for file|exit.
		@see #getCloseAction
		@see #getExitAction
		*/
//G***del		public void setFileExitAction(final Action action) {fileExitAction=action;}

	/**The action for showing help contents.*/
//G***del	private final Action helpContentsAction;

		/**@return The action for showing help contents.*/
//G***del		public Action getHelpContentsAction() {return helpContentsAction;}

	/**The action for showing an about box.*/
	private final Action aboutAction;

		/**@return The action for showing an about box.*/
		public Action getAboutAction() {return aboutAction;}

	/**The proxy action for closing the frame or exiting the application.
	<p>Defaults to <code>getExitAction()</code> if there is an application, or
		<code>getCloseAction()</code> if there is no application.</p>
	*/
	private final ProxyAction closeProxyAction;

		/**@return The proxy action for closing the frame and optionally exiting
			the application, depending on whether an application is present.
		@see #getApplication()
		*/
		public Action getCloseProxyAction() {return closeProxyAction;}

	/**The action for closing the application frame.*/
	private final Action closeAction;

		/**@return The action for closing the application frame.*/
		public Action getCloseAction() {return closeAction;}

	/**The action for exiting the application.*/
	private final Action exitAction;

		/**@return The action for exiting the application.*/
		public Action getExitAction() {return exitAction;}

	/**Constructs a string appropriate for showing on the title of the frame.
	<p>This version returns the application name.</p>
	@return A title to display on the frame.
	*/
	protected String constructTitle()
	{
		final String title;
		if(getApplication()!=null)	//if we have an application
		{
			final RDFObject titleObject=DCUtilities.getTitle(getApplication());	//get the application's title object
			title=titleObject!=null ? titleObject.toString() : null;	//use the title
		}
		else	//if we have no application
		{
			title=super.constructTitle();  //use the default title
		}
		return title;	//return the title we discovered
	}
	
	/**Default constructor.*/
	public ApplicationFrame()
	{
		this((SwingApplication)null);	//construct the frame with no application	
	}

	/**Application constructor.
	@param application The application this frame represents, or
		<code>null</code> if there is no application information available or this
		frame doesn't represent an application.
	*/
	public ApplicationFrame(final SwingApplication application)
	{
		this(application, true); //create an application frame with a default application panel and initialize
	}

	/**Constructor with a default panel and optional initialization.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ApplicationFrame(final boolean initialize)
	{
		this((SwingApplication)null, initialize);	//construct the frame with no application	
	}

	/**Application constructor with a content pane and optional initialization.
	@param application The application this frame represents, or
		<code>null</code> if there is no application information available or this
		frame doesn't represent an application.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ApplicationFrame(final SwingApplication application, final boolean initialize)
	{
		this(application, null, initialize); //create an application frame with the default content pane
	}

	/**Content pane constructor.
	@param contentPane The container to be used as the content pane, or
		<code>null</code> if the default content pane should be used.
	*/
	public ApplicationFrame(final Container contentPane)
	{
		this(null, contentPane);	//construct the frame with no application	
	}

	/**Content pane and application constructor.
	@param application The application this frame represents, or
		<code>null</code> if there is no application information available or this
		frame doesn't represent an application.
	@param contentPane The container to be used as the content pane, or
		<code>null</code> if the default content pane should be used.
	*/
	public ApplicationFrame(final SwingApplication application, final Container contentPane)
	{
		this(application, contentPane, true);  //construct and initialize the frame
	}

	/**Content pane constructor with optional initialization.
	@param contentPane The container to be used as the content pane, or
		<code>null</code> if the default content pane should be used.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ApplicationFrame(final Container contentPane, final boolean initialize)
	{
		this(null, contentPane, initialize);	//construct the frame with no application	
	}

	/**Content pane and application constructor with optional initialization.
	@param application The application this frame represents, or
		<code>null</code> if there is no application information available or this
		frame doesn't represent an application.
	@param contentPane The container to be used as the content pane, or
		<code>null</code> if the default content pane should be used.
	@param initialize <code>true</code> if the panel should initialize itself by
		calling the initialization methods.
	*/
	public ApplicationFrame(final SwingApplication application, final Container contentPane, final boolean initialize)
	{
		super(contentPane, false);	//construct the parent class with the given content pane, but don't initialize it
		this.application=application;	//store the application
		if(application!=null)	//if this frame represents an application
			setDefaultCloseOperation(EXIT_ON_CLOSE);	//exit when the frame closes
		closeAction=new CloseAction();  //create the close action
		exitAction=new ExitAction();  //create the exit action
		closeProxyAction=new ProxyAction(application!=null ? exitAction : closeAction);	//create the close proxy action, proxying the exit action if we have an application
		aboutAction=new AboutAction();
		if(initialize)  //if we should initialize
		  initialize(); //initialize the frame
	}

	/**Initializes the user interface.
		Any derived class that overrides this method should call this version.
	*/
  protected void initializeUI()
  {
		final ActionManager actionManager=getActionManager();	//get our action manager and create menus
		final SwingApplication application=getApplication();	//get our application, if there is one
		final Action fileMenuAction=ActionManager.getFileMenuAction();
		actionManager.addMenuAction(fileMenuAction);	//file
		actionManager.addMenuAction(fileMenuAction, getCloseProxyAction());	//file|close/exit
		if(application!=null)	//if we have an application
		{
			final Action helpMenuAction=ActionManager.getHelpMenuAction();
			actionManager.addMenuAction(helpMenuAction);	//help
			actionManager.addMenuAction(helpMenuAction, getAboutAction());	//help|about
		}
  	super.initializeUI();	//do the default UI initialization
  }

	/**Updates the states of the actions, including enabled/disabled status,
		proxied actions, etc.
	*/
/*G***fix if needed
	protected void updateStatus()
	{
		super.updateStatus();	//update the status normally
//G***fix this; this isn't good, because if a child class uses another save action, such as a proxied action, this will screw things up
//G***fix		final RDFResourceState description=getDocumentDescription();	//see what document is being described
//G***fix		getFileSaveAction().setEnabled(description!=null && description.isModified());	//only enable saving when there is a document that's modified
	}
*/

	/**Shows information about the application.*/
	public void about()
	{
		final AboutPanel aboutPanel=new AboutPanel(getApplication());	//create a new about panel for the application
			//determine a title for the dialog, based upon the application title
		final String dialogTitle="About"+(aboutPanel.getTitle()!=null ? " "+aboutPanel.getTitle() : "");	//G***i18n
		//have an option pane create and show a new dialog using our about panel
		OptionPane.showMessageDialog(this, aboutPanel, dialogTitle, JOptionPane.INFORMATION_MESSAGE);	//G***check and see why we originally had a more complex version
	}

	/**Determines whether the frame and application can close.
	@return <code>true</code> if the application frame and application can close.
	@see ToolStatusPanel#canClose
	@see #getDocumentDescription
	@see #canClose(RDFResourceState)
	*/
/*G***fix
	public boolean canClose()
	{
		return canCloseFile();	//return whether or not the current file can be closed
	}
*/

	/**@return <code>true</code> if the currently open file can be closed, else
		<code>false</code> if closing should be cancelled.
	If the content pane is an <code>ToolStatusPanel</code>, its
		<code>canClose()</code> method is called.
	If there is a document description, the frame's <code>canClose()</code> method
		is called for the description. 
	*/
/*G***fix
	public boolean canCloseFile()
	{
		boolean canClose=true;	//default to being able to be closed
		if(getContentPane() instanceof CanClosable)	//if the content pane knows how to ask about closing
		{
			canClose=((CanClosable)getContentPane()).canClose();	//ask if the content pane can be closed
		}
		if(canClose)	//if everything looks like we can close so far
		{
			final RDFResourceState description=getDocumentDescription();	//get the current document description
			if(description!=null && description!=getContentPane())	//if we have a document description, and the description isn't the content pane (otherwise we would call canClose() twice)
				canClose=canClose(description);	//see if we can close the description
		}
		return canClose;	//return whether we can close the frame
	}
*/
	

	/**Action for closing the frame.*/
	class CloseAction extends AbstractAction
	{
		/**Default constructor.*/
		public CloseAction()
		{
			super("Close");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Close the window");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Close the window.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_O));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.EXIT_ICON_FILENAME)); //load the correct icon
			putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F4, Event.ALT_MASK)); //add the accelerator
			putValue(ActionManager.MENU_ORDER_PROPERTY, new Integer(ActionManager.FILE_EXIT_MENU_ACTION_ORDER));	//set the order
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
			close(); //close the frame
		}
	}

	/**Action for exiting the application.*/
	class ExitAction extends AbstractAction
	{
		/**Default constructor.*/
		public ExitAction()
		{
			super("Exit");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "Exit the application");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Exit the application.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_X));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.EXIT_ICON_FILENAME)); //load the correct icon
			putValue(ActionManager.MENU_ORDER_PROPERTY, new Integer(ActionManager.FILE_EXIT_MENU_ACTION_ORDER));	//set the order
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
//G***del when works			exit(); //exit the application
			close(); //close the frame; this assumes that setDefaultCloseOperation has been set to DISPOSE_ON_CLOSE or EXIT_ON_CLOSE
		}
	}

	/**Action for showing the help about dialog.*/
	class AboutAction extends AbstractAction
	{
		/**Default constructor.*/
		public AboutAction()
		{
			super("About...");	//create the base class G***i18n
			putValue(SHORT_DESCRIPTION, "About the application");	//set the short description G***i18n
			putValue(LONG_DESCRIPTION, "Show more information about the application.");	//set the long description G***i18n
			putValue(MNEMONIC_KEY, new Integer(KeyEvent.VK_A));  //set the mnemonic key G***i18n
			putValue(SMALL_ICON, IconResources.getIcon(IconResources.INFO_ICON_FILENAME)); //load the correct icon
			putValue(ActionManager.MENU_ORDER_PROPERTY, new Integer(ActionManager.HELP_ABOUT_MENU_ACTION_ORDER));	//set the order
		}

		/**Called when the action should be performed.
		@param actionEvent The event causing the action.
		*/
		public void actionPerformed(final ActionEvent actionEvent)
		{
		  about();  //show the help about
		}
	}

}
