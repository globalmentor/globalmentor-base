package com.garretwilson.swing;

import java.awt.*;
import javax.swing.*;
import com.garretwilson.awt.BasicGridBagLayout;

/**A toolbar showing status, such as text labels and progress bars.
<p>By default progress is not visible until some change in progress.</p>
@author Garret Wilson
*/
public class StatusBar extends BasicToolBar
{

	/**Insets to use for the major status components.*/
	protected final Insets STATUS_INSETS=new Insets(2, 2, 2, 2);

	/**The zero-based relative size of the progress bar font.*/
	protected final int PROGRESS_BAR_RELATIVE_FONT_SIZE=-1;

	/**The progress bar.*/
	private final JProgressBar progressBar;

		/**@return The progress bar.*/
		protected JProgressBar getProgressBar() {return progressBar;}

	/**The panel on which custom status components can be placed.*/
	private final BasicPanel statusComponentPanel;

		/**@return The panel on which custom status components can be placed.*/
		protected BasicPanel getStatusComponentPanel() {return statusComponentPanel;}

	/**Default constructor with no name and horizontal orientation.*/
	public StatusBar()
	{
		this(true);	//construct a toolbar and initialize it	
	}

	/**Constructor with horizontal orientation and optional initialization.
	@param initialize <code>true</code> if the toolbar should initialize itself by
		calling the initialization methods.
	*/
	public StatusBar(final boolean initialize)
	{
		this(null, initialize);	//construct and initialize the toolbar
	}

	/**Name constructor with horizontal orientation.
	@param name The name of the toolbar, used as the title of an undocked toolbar.
	*/
	public StatusBar(final String name)
	{
		this(name, true);	//construct and initialize the toolbar
	}

	/**Orientation constructor with no name.
	@param orientation The orientation of the toolbar, either
		<code>HORIZONTAL</code> or <code>VERTICAL</code>.
	*/
	public StatusBar(final int orientation)
	{
		this(orientation, true);	//construct and initialize the toolbar with the given orientation
	}

	/**Name constructor with optional initialization.
	@param name The name of the toolbar, used as the title of an undocked toolbar.
	@param initialize <code>true</code> if the toolbar should initialize itself by
		calling the initialization methods.
	*/
	public StatusBar(final String name, final boolean initialize)
	{
		this(name, HORIZONTAL, initialize);	//construct the toolbar with horizontal orientation
	}

	/**Orientation constructor with optional initialization.
	@param orientation The orientation of the toolbar, either
		<code>HORIZONTAL</code> or <code>VERTICAL</code>.
	*/
	public StatusBar(final int orientation, final boolean initialize)
	{
		this(null, orientation, initialize);	//construct the toolbar with no name
	}

	/**Name and orientation constructor.
	@param name The name of the toolbar, used as the title of an undocked toolbar.
	@param orientation The orientation of the toolbar, either
		<code>HORIZONTAL</code> or <code>VERTICAL</code>.
	*/
	public StatusBar(final String name, final int orientation)
	{
		this(name, orientation, true);	//construct and initialize the toolbar with the given name and orientation
	}

	/**Name and orientation constructor with optional initialization.
	@param name The name of the toolbar, used as the title of an undocked toolbar.
	@param orientation The orientation of the toolbar, either
		<code>HORIZONTAL</code> or <code>VERTICAL</code>.
	@param initialize <code>true</code> if the toolbar should initialize itself by
		calling the initialization methods.
	*/
	public StatusBar(final String name, final int orientation, final boolean initialize)
	{
		super(name, orientation, false);	//construct the parent class without initializing it
		setLayout(new BasicGridBagLayout());	//switch to a basic grid layout
		statusComponentPanel=new BasicPanel(new BasicGridBagLayout());	//create the panel on which custom status components can be placed
		progressBar=new JProgressBar();	//create the progress bar
		if(initialize)  //if we should initialize
			initialize();   //initialize the panel
	}

	/**Initializes the user interface.*/
	public void initializeUI()
	{
		super.initializeUI();	//do the default user interface initialization
//G**del if not needed		setBorder(BorderFactory.createEtchedBorder());	//set the status border
		statusComponentPanel.setOpaque(false);	//the status component panel is only for layout
		setProgressVisible(false);	//hide progress until there is some change in progress
//G***fix		statusStatusLabel.setFont(statusBar.getFont().deriveFont((float)statusBar.getFont().getSize()-1));	//G***testing
//G**fix		statusBar.add(statusStatusLabel, new GridBagConstraints(0, 0, 1, 1, 0.5, 0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, new Insets(0, 0, 0, 0), 0, 0));	//add the status label to the status bar
				//TODO i18n add these status components correctly west or east based upon component orientation, and make sure the orientation of the status component panel matches 
			//add the status component panel
		add(statusComponentPanel, new GridBagConstraints(0, 0, 1, 1, 0, 0, GridBagConstraints.WEST, GridBagConstraints.HORIZONTAL, STATUS_INSETS, 0, 0));
			//add glue to separate the status components and the progress bar
		add(Box.createGlue(), new GridBagConstraints(1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.WEST, GridBagConstraints.BOTH, STATUS_INSETS, 0, 0));
		progressBar.setStringPainted(true);	//set the progresss bar to display text
			//change the font size of the progress bar
		progressBar.setFont(progressBar.getFont().deriveFont((float)progressBar.getFont().getSize()+PROGRESS_BAR_RELATIVE_FONT_SIZE));
		add(progressBar, new GridBagConstraints(2, 0, 1, 1, 0.5, 0, GridBagConstraints.EAST, GridBagConstraints.HORIZONTAL, STATUS_INSETS, 0, 0));
	}

	/**Adds a status component to the status panel.
	@param component The component to add to the status panel.
	*/
	public void addStatusComponent(final Component component)
	{
			//add the status component to the status component panel in the next position horizontally
		getStatusComponentPanel().add(component, ((BasicGridBagLayout)getStatusComponentPanel().getLayout()).createNextBoxConstraints(BasicGridBagLayout.X_AXIS));
	}

	/**Sets whether progress is visible.
	@param visible <code>true</code> if progress should be visible, else
		<code>false</code> if it should be hidden.
	*/
	public void setProgressVisible(final boolean visible)
	{
		getProgressBar().setVisible(visible);	//show or hide the progress bar		
	}

	/**Sets the range of values for the progress bar.
	@param minimum The minumum progress value.
	@param maximum The maximum progress value.
	*/
	public void setProgressRange(final int minimum, final int maximum)
	{
		getProgressBar().setMinimum(minimum);	//set the progress bar minimum
		getProgressBar().setMaximum(maximum);	//set the progress bar maximum
	}

	/**Sets the text to be displayed on the progress bar.
	<p>If text is not <code>null</code>, progress visibility is turned on if it
		isn't already.</p>
	@param text The text to be displayed on the progress bar, or <code>null</code>
		if no text should be displayed.
	*/
	public void setProgress(final String text)
	{
		getProgressBar().setString(text);	//set the text of the progress bar
		if(text!=null)	//if there is text
		{
			setProgressVisible(true);	//show progress
		}
	}

	/**Sets the text to be displayed on the progress bar.
	<p>Progress visibility is turned on if it isn't already.</p>
	@param text The text to be displayed on the progress bar, or <code>null</code>
		if no text should be displayed.
	@param value The amount of progress.
	*/
	public void setProgress(final String text, final int value)
	{
		setProgress(text);	//sets the text of the progress
		getProgressBar().setValue(value);	//set the text of the progress bar
		setProgressVisible(true);	//show progress
	}

	/**@return A new custom label appropriate for placing on the status panel.*/
	public static JLabel createStatusLabel()
	{
		final JLabel label=new JLabel();	//create a label
		label.setBorder(BorderFactory.createLoweredBevelBorder());	//set the label border
		return label;	//return the label we created
	}

}