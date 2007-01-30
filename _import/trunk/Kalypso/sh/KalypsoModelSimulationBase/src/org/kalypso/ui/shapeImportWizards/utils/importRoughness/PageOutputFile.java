package org.kalypso.ui.shapeImportWizards.utils.importRoughness;

import java.io.File;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.WizardResourceImportPage;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class PageOutputFile extends WizardResourceImportPage implements Listener
{
  IStructuredSelection m_Selection;

  // widgets on this page
  Text txt_OutputFile;

//  Text txt_Description;

  Button btn_CheckBox_CreateGMT;

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  /**
   * Constructor for main page
   */
  public PageOutputFile( final IStructuredSelection selection )
  {
	    this( Messages.getString( "PageOutputFile.0" ), selection );
  }

  protected PageOutputFile( final String name, final IStructuredSelection selection )
  {
    super( name, selection );
    setTitle( Messages.getString( "PageOutputFile.1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "PageOutputFile.2" ) ); //$NON-NLS-1$
    this.m_Selection = selection;
    msg_StatusLine = new Status( IStatus.OK, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
  }
  
  @Override
  public void createOptionsGroupButtons( Group optionsGroup )
  {

    // create the composite to hold the widgets
    GridData gd;
    Composite composite = new Composite( optionsGroup, SWT.NULL );

    GridLayout gl = new GridLayout();
    int ncol = 2;
    gl.numColumns = ncol;
    composite.setLayout( gl );
    composite.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL ) );

    // Output file name
    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageOutputFile.3" ) ); //$NON-NLS-1$
    txt_OutputFile = new Text( composite, SWT.BORDER );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    txt_OutputFile.setLayoutData( gd );
    
    txt_OutputFile.setFocus();

    // Description
//    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageOutputFile.4" ) ); //$NON-NLS-1$
//    txt_Description = new Text( composite, SWT.BORDER );
//    gd = new GridData();
//    gd.horizontalAlignment = GridData.FILL;
//    gd.grabExcessHorizontalSpace = true;
//    // gd.horizontalSpan = ncol - 1;
//    txt_Description.setLayoutData( gd );

    createLine( composite, ncol );

    // Create gmt (Karte) file
    btn_CheckBox_CreateGMT = new Button( composite, SWT.CHECK );
    btn_CheckBox_CreateGMT.setText( Messages.getString( "PageOutputFile.5" ) ); //$NON-NLS-1$
    gd = new GridData( GridData.BEGINNING );
    // gd.horizontalSpan = ncol;
    btn_CheckBox_CreateGMT.setLayoutData( gd );
    btn_CheckBox_CreateGMT.setSelection( true );

    setControl( composite );
	addListeners();
  }

	private void addListeners()
	{
		txt_OutputFile.addListener(SWT.Modify, this);
	}

	/**
	 * @see Listener#handleEvent(Event)
	 */
	public void handleEvent(Event event)
	{
		getWizard().getContainer().updateButtons();
	}

  protected boolean isTextNonEmpty( Text t )
  {
    String s = t.getText();
    if( (s != null) && (s.trim().length() > 0) )
      return true;
    return false;
  }

  private void createLine( Composite parent, int ncol )
  {
    Label line = new Label( parent, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD );
    GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalSpan = ncol;
    line.setLayoutData( gridData );
  }

  @Override
  public boolean canFlipToNextPage( )
  {
    return false;
  }
  
  @Override
  public boolean isPageComplete() {
	  boolean isComplete = isTextNonEmpty(txt_OutputFile);
	  if(isComplete){
		  saveDataToModel( );
	  }
	  return isComplete;
  }

  private void saveDataToModel( )
  {
    DataContainer m_data = ((ImportWizard) getWizard()).m_data;
    m_data.setOutputFile( getResourceAbsolutePath() + File.separator + txt_OutputFile.getText() );
//    m_data.setDescription( txt_Description.getText() );
    m_data.setProject( getTargetContainer().getProject() );
    m_data.setCreateMap( btn_CheckBox_CreateGMT.getSelection() );
    m_data.setOutputDirectory( getResourceRelativePath() );
  }

  public IContainer getTargetContainer( )
  {
    final IPath path = super.getResourcePath();

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    if( path.segmentCount() == 1 )
      return root.getProject( path.segment( 0 ) );

    return root.getFolder( path );
  }

  public String getResourceAbsolutePath( )
  {
    return ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + super.getResourcePath().toOSString();
  }

  public String getResourceRelativePath( )
  {
    return super.getResourcePath().removeFirstSegments( 1 ).addTrailingSeparator().toOSString();
  }

  @Override
  protected ITreeContentProvider getFileProvider( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  protected ITreeContentProvider getFolderProvider( )
  {
    // TODO Auto-generated method stub
    return null;
  }

@Override
protected void createSourceGroup(Composite parent)
{
	// TODO Auto-generated method stub
	
}
}
