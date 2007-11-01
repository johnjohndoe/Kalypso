package org.kalypso.risk.model.actions.dataImport.landuse;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
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
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportLanduseShpPage extends WizardPage implements Listener
{
  private Text txt_InputFile;

  private Combo cmb_ShapeProperty;

  private Combo cmb_CoordinateSystem;

  private IPath initialSourcePath;

  private IPath m_sourcePath = null;

  private List<String> fileExtensions = new LinkedList<String>();

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  private Button btn_inputFileBrowse;

  public ImportLanduseShpPage( )
  {
    super( Messages.getString( "ImportLanduseShpPage.0" ), "", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "ImportLanduseShpPage.2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportLanduseShpPage.3" ) ); //$NON-NLS-1$
  }

  /**
   * Creates the top level control for this dialog page under the given parent composite, then calls
   * <code>setControl</code> so that the created control can be accessed via <code>getControl</code>
   * 
   * @param parent
   *            the parent composite
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    final Label label_1 = new Label( container, SWT.NONE );
    final GridData gridData_1 = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    label_1.setLayoutData( gridData_1 );
    label_1.setText( Messages.getString( "ImportLanduseShpPage.4" ) ); //$NON-NLS-1$

    txt_InputFile = new Text( container, SWT.BORDER );
    txt_InputFile.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    btn_inputFileBrowse = new Button( container, SWT.NONE );
    btn_inputFileBrowse.setText( Messages.getString( "ImportLanduseShpPage.11" ) ); //$NON-NLS-1$

    createLine( container, 3 );

    // Landuse property combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "ImportLanduseShpPage.12" ) ); //$NON-NLS-1$
    cmb_ShapeProperty = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd0 = new GridData();
    gd0.horizontalAlignment = GridData.FILL;
    gd0.widthHint = 75;
    cmb_ShapeProperty.setEnabled( false );
    cmb_ShapeProperty.setLayoutData( gd0 );

    createLine( container, 3 );

    // Coordinate system combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "ImportLanduseShpPage.13" ) ); //$NON-NLS-1$
    cmb_CoordinateSystem = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    cmb_CoordinateSystem.setItems( (new ConvenienceCSFactoryFull()).getKnownCS() );
    final int indexOfDefaultCRS = cmb_CoordinateSystem.indexOf( IKalypsoCorePreferences.DEFAULT_CRS );
    cmb_CoordinateSystem.select( indexOfDefaultCRS > -1 ? indexOfDefaultCRS : 0 );
    final GridData gd2 = new GridData();
    gd2.horizontalAlignment = GridData.FILL;
    gd2.widthHint = 75;
    cmb_CoordinateSystem.setEnabled( true );
    cmb_CoordinateSystem.setLayoutData( gd2 );

    btn_inputFileBrowse.setFocus();
    initContents();
    addListeners();
    setControl( container );
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
  public void handleEvent( Event event )
  {
    // Initialize a variable with the no error status
    Status status = new Status( IStatus.OK, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
    msg_StatusLine = status;
    if( event.widget == btn_inputFileBrowse )
    {
      final String filenameFromDialog = getFilenameFromDialog( new String[] { "*.shp" }, null ); //$NON-NLS-1$
      if( filenameFromDialog != null )
        txt_InputFile.setText( filenameFromDialog );
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
        status = new Status( IStatus.ERROR, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
        msg_StatusLine = status;
        e.printStackTrace();
      }
    }
    // Show the most serious error
    applyToStatusLine( msg_StatusLine );
    getWizard().getContainer().updateButtons();
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * 
   * @param selection
   *            the selection or <code>null</code> if none
   */
  public void init( ISelection selection )
  {
    if( !(selection instanceof IStructuredSelection) )
      return;

    fileExtensions.add( new String( "shp" ) ); //$NON-NLS-1$

    final Iterator iter = ((IStructuredSelection) selection).iterator();
    while( iter.hasNext() )
    {
      Object item = iter.next();
      if( item instanceof IFile )
      {
        IFile file = (IFile) item;
        if( fileExtensions.contains( file.getFileExtension() ) )
        {
          initialSourcePath = file.getLocation();
          break;
        }
        item = file.getProject();
      }
    }
  }

  /**
   * Called by <code>createControl</code> to initialize the receiver's content based upon the cached selection
   * provided by the wizard.
   */
  private void initContents( )
  {
    if( initialSourcePath == null )
    {
      setPageComplete( false );
      return;
    }
    IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    IPath path = initialSourcePath;
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    txt_InputFile.setText( path.toString() );
    updatePageComplete();
    setMessage( null );
    setErrorMessage( null );
  }

  /**
   * Update the current page complete state based on the field content.
   */
  protected void updatePageComplete( )
  {
    setPageComplete( false );

    IPath sourceLoc = getSourceLocation();
    // if( sourceLoc == null || !(fileExtensions.contains( sourceLoc.getFileExtension() )) )
    if( sourceLoc == null || !sourceLoc.toFile().isFile() )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "ImportLanduseShpPage.6" ) ); //$NON-NLS-1$
      return;
    }
    else if( sourceLoc.getFileExtension().equalsIgnoreCase( "shp" ) && !sourceLoc.removeFileExtension().addFileExtension( "dbf" ).toFile().isFile() //$NON-NLS-1$ //$NON-NLS-2$
        && !sourceLoc.removeFileExtension().addFileExtension( "shx" ).toFile().isFile() ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "ImportLanduseShpPage.10" ) ); //$NON-NLS-1$
      return;
    }
    setMessage( null );
    setErrorMessage( null );
    setPageComplete( true );
  }

  /**
   * Answer the source file location or <code>null</code> if unspecified
   */
  public IPath getSourceLocation( )
  {
    // if(m_sourcePath != null)
    // return m_sourcePath;
    final String text = txt_InputFile.getText().trim();
    if( text.length() == 0 )
      return null;
    m_sourcePath = new Path( text );
    if( !m_sourcePath.isAbsolute() )
      m_sourcePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().append( m_sourcePath );
    return m_sourcePath;
  }

  public CS_CoordinateSystem getCoordinateSystem( )
  {
    return ConvenienceCSFactory.getInstance().getOGCCSByName( cmb_CoordinateSystem.getText() );
  }

  public String getLanduseProperty( )
  {
    return cmb_ShapeProperty.getText();
  }

  private String getFilenameFromDialog( final String[] filterExtensions, final String path )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.SINGLE );
    dialog.setFilterExtensions( filterExtensions );
    if( path != null )
      dialog.setFilterPath( path );
    dialog.open();
    final String fileName = dialog.getFileName();
    final String filterPath = dialog.getFilterPath();
    String filePath = null;
    if( fileName != null && fileName != "" && filterPath != null ) //$NON-NLS-1$
      filePath = filterPath + File.separator + fileName; //$NON-NLS-1$
    return filePath;
  }

  /**
   * Applies the status to the status line of a dialog page.
   */
  private void applyToStatusLine( final IStatus status )
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

  protected boolean isTextNonEmpty( final Text t )
  {
    final String s = t.getText();
    return (s != null) && (s.trim().length() > 0);
  }

  private void createLine( final Composite parent, final int ncol )
  {
    final Label line = new Label( parent, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD );
    final GridData gridData = new GridData( GridData.FILL_HORIZONTAL );
    gridData.horizontalSpan = ncol;
    line.setLayoutData( gridData );
  }

  @Override
  public boolean isPageComplete( )
  {
    return cmb_ShapeProperty.isEnabled();
  }

}
