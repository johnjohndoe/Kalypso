package org.kalypso.kalypsosimulationmodel.wizard.shapeImport;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.IDialogPage;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;

/**
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 *
 */
public class PageMain extends WizardPage implements Listener
{

	IWorkbench workbench;
	IStructuredSelection selection;

	// widgets on this page 
	Combo cmb_ShapeProperty;
	Combo cmb_CoordinateSystem;
	Text txt_InputFile;
	Text txt_OutputFile;
	Button btn_inputFileBrowse;
	Button btn_outputFileBrowse;

	// status variable for the possible errors on this page
	IStatus msg_StatusLine;

	/**
	 * Constructor for main page
	 */
	public PageMain(IWorkbench workbench, IStructuredSelection selection) {
		super(Messages.getString("PageMain.0")); //$NON-NLS-1$
		setTitle(Messages.getString("PageMain.1")); //$NON-NLS-1$
		setDescription(Messages.getString("PageMain.2")); //$NON-NLS-1$
		this.workbench = workbench;
		this.selection = selection;
		msg_StatusLine = new Status(IStatus.OK, "not_used", 0, "", null); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * @see IDialogPage#createControl(Composite)
	 */
	public void createControl(Composite parent) {

		// create the composite to hold the widgets
		GridData gd;
		Composite composite =  new Composite(parent, SWT.NULL);

		// create the desired layout for this wizard page
		GridLayout gl = new GridLayout();
		int ncol = 3;
		gl.numColumns = ncol;
		composite.setLayout(gl);

		// create the widgets. If the appearance of the widget is different from the default, 
		// create a GridData for it to set the alignment and define how much space it will occupy		

		// Input shape file
		new Label (composite, SWT.NONE).setText(Messages.getString("PageMain.5"));				 //$NON-NLS-1$
		txt_InputFile = new Text(composite, SWT.BORDER);
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.grabExcessHorizontalSpace = true;
		//gd.horizontalSpan = ncol - 1;
		txt_InputFile.setLayoutData(gd);
		btn_inputFileBrowse = new Button(composite, SWT.PUSH);
		btn_inputFileBrowse.setText(Messages.getString("PageMain.6")); //$NON-NLS-1$
		gd = new GridData(GridData.END);
		//gd.horizontalSpan = ncol;
		btn_inputFileBrowse.setLayoutData(gd);
		btn_inputFileBrowse.setSelection(true);
		//createLine(composite, ncol);

		// Roughness parameter combo box
		new Label (composite, SWT.NONE).setText(Messages.getString("PageMain.7"));						 //$NON-NLS-1$
		cmb_ShapeProperty = new Combo(composite, SWT.BORDER | SWT.READ_ONLY);
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.widthHint = 75;
		cmb_ShapeProperty.setEnabled(false);
		cmb_ShapeProperty.setLayoutData(gd);
		
		createLine(composite, ncol);

		// Coordinate system combo box
		new Label (composite, SWT.NONE).setText(Messages.getString("PageMain.8"));						 //$NON-NLS-1$
		cmb_CoordinateSystem = new Combo(composite, SWT.BORDER | SWT.READ_ONLY);
		cmb_CoordinateSystem.setItems(( new ConvenienceCSFactoryFull() ).getKnownCS());
		final int index_GausKrueger = cmb_CoordinateSystem.indexOf(KalypsoModelSimulationBaseConsts.STRING_GAUSS_KRUEGER);
		cmb_CoordinateSystem.select(index_GausKrueger > -1 ? index_GausKrueger : 0);
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		gd.widthHint = 75;
		cmb_CoordinateSystem.setEnabled(true);
		cmb_CoordinateSystem.setLayoutData(gd);
		
		createLine(composite, ncol);

		// Output gml file
		new Label (composite, SWT.NONE).setText(Messages.getString("PageMain.9"));				 //$NON-NLS-1$
		txt_OutputFile = new Text(composite, SWT.BORDER);
		gd = new GridData();
		gd.horizontalAlignment = GridData.FILL;
		//gd.horizontalSpan = ncol - 1;
		txt_OutputFile.setLayoutData(gd);
		btn_outputFileBrowse = new Button(composite, SWT.PUSH);
		btn_outputFileBrowse.setText(Messages.getString("PageMain.10")); //$NON-NLS-1$
		gd = new GridData(GridData.END);
		//gd.horizontalSpan = ncol;
		btn_outputFileBrowse.setLayoutData(gd);
		createLine(composite, ncol);


		// set the composite as the control for this page
		setControl(composite);		
		addListeners();
	}

	private void addListeners()
	{
		btn_inputFileBrowse.addListener(SWT.MouseDown, this);
		btn_inputFileBrowse.addListener(SWT.KeyDown, this);
		btn_outputFileBrowse.addListener(SWT.MouseDown, this);
		btn_outputFileBrowse.addListener(SWT.KeyDown, this);
		txt_InputFile.addListener(SWT.Modify, this);
		txt_OutputFile.addListener(SWT.Modify, this);
		cmb_ShapeProperty.addListener(SWT.Selection, this);
	}


	/**
	 * @see Listener#handleEvent(Event)
	 */
	public void handleEvent(Event event) {
		// Initialize a variable with the no error status
		Status status = new Status(IStatus.OK, "not_used", 0, "", null); //$NON-NLS-1$ //$NON-NLS-2$
		msg_StatusLine = status;
		if(event.widget == btn_inputFileBrowse)
		{
			txt_InputFile.setText(getFilenameFromDialog(null, new String[] { "*.shp" }, null )); //$NON-NLS-1$
		}
		if(event.widget == btn_outputFileBrowse)
		{
			txt_OutputFile.setText(getFilenameFromDialog(null, null, null )); //$NON-NLS-1$
		}
		if(event.widget == txt_InputFile)
		{
			try
			{
				if(isTextNonEmpty(txt_InputFile))
				{
					File file = new File(txt_InputFile.getText());
					if (file.exists() && file.isFile() && file.canRead())
					{
						ShapeFile shape = new ShapeFile( FileUtilities.nameWithoutExtension(txt_InputFile.getText()) );
						String[] propertyNames = shape.getProperties();
						shape.close();
						cmb_ShapeProperty.setItems( propertyNames );
						cmb_ShapeProperty.select(0);
						cmb_ShapeProperty.setEnabled(true);
						if(!isTextNonEmpty(txt_OutputFile))
						{
							txt_OutputFile.setText(FileUtilities.nameWithoutExtension(txt_InputFile.getText()) + ".gml"); //$NON-NLS-1$
						}
					}
					else
					{
						cmb_ShapeProperty.setEnabled(false);
					}
				}
				else
				{
					cmb_ShapeProperty.setEnabled(false);
				}
			}
			catch(Exception e){
				StringBuffer sb = new StringBuffer(Messages.getString("PageMain.15")); //$NON-NLS-1$
				sb.append(txt_InputFile.getText().substring(txt_InputFile.getText().lastIndexOf(File.separator)+1));
				sb.append("\n").append(Messages.getString("PageMain.17")); //$NON-NLS-1$ //$NON-NLS-2$
				status = new Status(IStatus.ERROR, "not_used", 0, sb.toString(), null);         //$NON-NLS-1$
				msg_StatusLine = status;
				e.printStackTrace();
			}
		}
		// If the event is triggered by the destination or departure fields
		// set the corresponding status variable to the right value
		if ((event.widget == txt_InputFile) || (event.widget == txt_OutputFile)) {
			if (txt_InputFile.getText().equals(txt_OutputFile.getText()) && !"".equals(txt_InputFile.getText())) //$NON-NLS-1$
				status = new Status(IStatus.ERROR, "not_used", 0,  //$NON-NLS-1$
						Messages.getString("PageMain.21"), null);         //$NON-NLS-1$
			msg_StatusLine = status;
		}

		// Show the most serious error
		applyToStatusLine(msg_StatusLine);
		getWizard().getContainer().updateButtons();
	}
	
	  String getFilenameFromDialog( File selectedFile, String[] filterExtensions, String path )
	  {
	    FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
	    dialog.setFilterExtensions( filterExtensions );
	    if(path != null)
	    	dialog.setFilterPath(path);
	    if( selectedFile != null )
	    {
	      dialog.setFileName( selectedFile.getName() );
	      dialog.setFilterPath( selectedFile.getParent() );
	    }
	    dialog.open();
	    String fileName = dialog.getFileName();
	    String filterPath = dialog.getFilterPath();
	    String filePath = null;
	    if( fileName != null && fileName != "" && filterPath != null ) //$NON-NLS-1$
	    {
	      filePath = filterPath + File.separator + fileName; //$NON-NLS-1$
	    }
	    return filePath;
	  }

	/**
	 * Applies the status to the status line of a dialog page.
	 */
	private void applyToStatusLine(IStatus status) {
		String message= status.getMessage();
		if (message.length() == 0) message= null;
		switch (status.getSeverity()) {
		case IStatus.OK:
			setErrorMessage(null);
			setMessage(message);
			break;
		case IStatus.WARNING:
			setErrorMessage(null);
			setMessage(message, WizardPage.WARNING);
			break;				
		case IStatus.INFO:
			setErrorMessage(null);
			setMessage(message, WizardPage.INFORMATION);
			break;			
		default:
			setErrorMessage(message);
		setMessage(null);
		break;		
		}
	}	

	private static boolean isTextNonEmpty(Text t)
	{
		String s = t.getText();
		if ((s!=null) && (s.trim().length() >0)) return true;
		return false;
	}	

	private void createLine(Composite parent, int ncol) 
	{
		Label line = new Label(parent, SWT.SEPARATOR|SWT.HORIZONTAL|SWT.BOLD);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.horizontalSpan = ncol;
		line.setLayoutData(gridData);
	}

	@Override
	public boolean canFlipToNextPage() {
		boolean canFlip = cmb_ShapeProperty.isEnabled() && isTextNonEmpty(txt_OutputFile);
		if(canFlip)
		{
			ImportWizard wizard = (ImportWizard)getWizard();
			wizard.model.inputFile = txt_InputFile.getText();
			wizard.model.outputFile = txt_OutputFile.getText();
			wizard.model.shapeProperty = cmb_ShapeProperty.getText();
			wizard.model.coordinateSystem = cmb_CoordinateSystem.getText();
		}
		return canFlip;
	}	
}

