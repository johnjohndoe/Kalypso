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
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.WizardResourceImportPage;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class PageMain extends WizardResourceImportPage implements Listener
{
  IStructuredSelection m_Selection;

  // widgets on this page
  Combo cmb_ShapeProperty;

  Combo cmb_CoordinateSystem;

  Text txt_InputFile;

  Button btn_inputFileBrowse;

  Text txt_OutputFile;

  Text txt_Description;

  Button btn_CheckBox_CreateGMT;

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  // just to be sure that this ObjectFactory is loaded, otherwise some TypeHandlers will be null
  private static final org.kalypso.template.gismapview.ObjectFactory mapTemplateOF = new org.kalypso.template.gismapview.ObjectFactory();

  // private static final org.kalypso.template.types.ObjectFactory typeOF = new
  // org.kalypso.template.types.ObjectFactory();
  // private static final JAXBContext JC = JaxbUtilities.createQuiet(
  // org.kalypso.template.gismapview.ObjectFactory.class, org.kalypso.template.types.ObjectFactory.class );

  private static final String aaa = Messages.getString( "PageMain.0" );

  /**
   * Constructor for main page
   */
  public PageMain( final IStructuredSelection selection )
  {
    this( aaa, selection );//$NON-NLS-1$
  }

  protected PageMain( final String name, final IStructuredSelection selection )
  {
    super( name, selection ); //$NON-NLS-1$
    setTitle( Messages.getString( "PageMain.1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "PageMain.2" ) ); //$NON-NLS-1$
    this.m_Selection = selection;
    msg_StatusLine = new Status( IStatus.OK, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void createSourceGroup( Composite parent )
  {
    // create the composite to hold the widgets
    GridData gd;
    Composite composite = new Composite( parent, SWT.NULL );

    // create the desired layout for this wizard page
    GridLayout gl = new GridLayout();
    int ncol = 3;
    gl.numColumns = ncol;
    composite.setLayout( gl );
    composite.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_FILL | GridData.GRAB_HORIZONTAL ) );

    // Input shape file
    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageMain.5" ) ); //$NON-NLS-1$
    txt_InputFile = new Text( composite, SWT.BORDER );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    txt_InputFile.setLayoutData( gd );
    btn_inputFileBrowse = new Button( composite, SWT.PUSH );
    btn_inputFileBrowse.setText( Messages.getString( "PageMain.6" ) ); //$NON-NLS-1$
    gd = new GridData( GridData.END );
    // gd.horizontalSpan = ncol;
    btn_inputFileBrowse.setLayoutData( gd );
    // createLine(composite, ncol);

    // Roughness parameter combo box
    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageMain.7" ) ); //$NON-NLS-1$
    cmb_ShapeProperty = new Combo( composite, SWT.BORDER | SWT.READ_ONLY );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.widthHint = 75;
    cmb_ShapeProperty.setEnabled( false );
    cmb_ShapeProperty.setLayoutData( gd );

    createLine( composite, ncol );

    // Coordinate system combo box
    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageMain.8" ) ); //$NON-NLS-1$
    cmb_CoordinateSystem = new Combo( composite, SWT.BORDER | SWT.READ_ONLY );
    cmb_CoordinateSystem.setItems( (new ConvenienceCSFactoryFull()).getKnownCS() );
    final int index_GausKrueger = cmb_CoordinateSystem.indexOf( DataContainer.GAUS_KRUEGER );
    cmb_CoordinateSystem.select( index_GausKrueger > -1 ? index_GausKrueger : 0 );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.widthHint = 75;
    cmb_CoordinateSystem.setEnabled( true );
    cmb_CoordinateSystem.setLayoutData( gd );

    createLine( composite, ncol );

    // set the composite as the control for this page
    setControl( composite );
    addListeners();
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
    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageMain.19" ) ); //$NON-NLS-1$
    txt_OutputFile = new Text( composite, SWT.BORDER );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    txt_OutputFile.setLayoutData( gd );

    // Description
    new Label( composite, SWT.NONE ).setText( Messages.getString( "PageMain.18" ) ); //$NON-NLS-1$
    txt_Description = new Text( composite, SWT.BORDER );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    txt_Description.setLayoutData( gd );

    createLine( composite, ncol );

    // Create gmt (Karte) file
    btn_CheckBox_CreateGMT = new Button( composite, SWT.CHECK );
    btn_CheckBox_CreateGMT.setText( Messages.getString( "PageMain.20" ) ); //$NON-NLS-1$
    gd = new GridData( GridData.BEGINNING );
    // gd.horizontalSpan = ncol;
    btn_CheckBox_CreateGMT.setLayoutData( gd );
    btn_CheckBox_CreateGMT.setSelection( true );

    setControl( composite );
  }

  private void addListeners( )
  {
    btn_inputFileBrowse.addListener( SWT.MouseDown, this );
    btn_inputFileBrowse.addListener( SWT.KeyDown, this );
    txt_InputFile.addListener( SWT.Modify, this );
    cmb_ShapeProperty.addListener( SWT.Selection, this );
  }

  /**
   * @see Listener#handleEvent(Event)
   */
  @Override
  public void handleEvent( Event event )
  {
    // Initialize a variable with the no error status
    Status status = new Status( IStatus.OK, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
    msg_StatusLine = status;
    if( event.widget == btn_inputFileBrowse )
    {
      txt_InputFile.setText( getFilenameFromDialog( null, new String[] { "*.shp" }, null ) ); //$NON-NLS-1$
    }
    else if( event.widget == txt_InputFile )
    {
      try
      {
        if( isTextNonEmpty( txt_InputFile ) )
        {
          File file = new File( txt_InputFile.getText() );
          if( file.exists() && file.isFile() && file.canRead() )
          {
            ShapeFile shape = new ShapeFile( FileUtilities.nameWithoutExtension( txt_InputFile.getText() ) );
            String[] propertyNames = shape.getProperties();
            shape.close();
            cmb_ShapeProperty.setItems( propertyNames );
            cmb_ShapeProperty.select( 0 );
            cmb_ShapeProperty.setEnabled( true );
            if( !isTextNonEmpty( txt_OutputFile ) )
              txt_OutputFile.setText( FileUtilities.nameWithoutExtension( FileUtilities.nameFromPath( txt_InputFile.getText() ) ) + ".gml" ); //$NON-NLS-1$
          }
          else
          {
            cmb_ShapeProperty.setEnabled( false );
          }
        }
        else
        {
          cmb_ShapeProperty.setEnabled( false );
        }
      }
      catch( Exception e )
      {
        StringBuffer sb = new StringBuffer( Messages.getString( "PageMain.15" ) ); //$NON-NLS-1$
        sb.append( txt_InputFile.getText().substring( txt_InputFile.getText().lastIndexOf( File.separator ) + 1 ) );
        sb.append( "\n" ).append( Messages.getString( "PageMain.17" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        status = new Status( IStatus.ERROR, "not_used", 0, sb.toString(), null ); //$NON-NLS-1$
        msg_StatusLine = status;
        e.printStackTrace();
      }
    }
    else
      super.handleEvent( event );

    // Show the most serious error
    applyToStatusLine( msg_StatusLine );
    getWizard().getContainer().updateButtons();
  }

  String getFilenameFromDialog( File selectedFile, String[] filterExtensions, String path )
  {
    FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    dialog.setFilterExtensions( filterExtensions );
    if( path != null )
      dialog.setFilterPath( path );
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
  private void applyToStatusLine( IStatus status )
  {
    String message = status.getMessage();
    if( message.length() == 0 )
      message = null;
    switch( status.getSeverity() )
    {
      case IStatus.OK:
        setErrorMessage( null );
        setMessage( message );
        break;
      case IStatus.WARNING:
        setErrorMessage( null );
        setMessage( message, WizardPage.WARNING );
        break;
      case IStatus.INFO:
        setErrorMessage( null );
        setMessage( message, WizardPage.INFORMATION );
        break;
      default:
        setErrorMessage( message );
        setMessage( null );
        break;
    }
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
    // return cmb_ShapeProperty.isEnabled() && isTextNonEmpty(txt_OutputFile) && btn_CheckBox_CreateGMT.getSelection();
    return false;
  }

  // @Override
  // public IWizardPage getNextPage() {
  // saveDataToModel();
  // PageStyleSelection nextPage = ((ImportWizard)getWizard()).pageStyleSelection;
  // nextPage.createColorControls();
  // return nextPage;
  // }

  protected void saveDataToModel( )
  {
    DataContainer m_data = ((ImportWizard) getWizard()).m_data;
    m_data.setInputFile( txt_InputFile.getText() );
    m_data.setOutputFile( getResourceAbsolutePath() + File.separator + txt_OutputFile.getText() );
    m_data.setShapeProperty( cmb_ShapeProperty.getText() );
    m_data.setCoordinateSystem( cmb_CoordinateSystem.getText() );
    m_data.setDescription( txt_Description.getText() );
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
}
