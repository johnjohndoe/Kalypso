package org.kalypso.ui.rrm.wizards.importGeologyData;

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
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportGeologyWizardPage extends WizardPage implements SelectionListener, ModifyListener
{
  private Text txt_InputFile;

  private Combo cmbMaxPerculationsRateProperty;

  private Combo cmbGWFactorProperty;

  private IPath initialSourcePath;

  private IPath m_sourcePath = null;

  private final List<String> m_fileExtensions;

  private Button btn_inputShpFileBrowse;

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  public ImportGeologyWizardPage( )
  {
    super( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.0" ), "", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.3" ) ); //$NON-NLS-1$

    m_fileExtensions = new LinkedList<String>();
  }

  /**
   * Creates the top level control for this dialog page under the given parent composite, then calls
   * <code>setControl</code> so that the created control can be accessed via <code>getControl</code>
   * 
   * @param parent
   *          the parent composite
   */
  @Override
  public void createControl( final Composite parent )
  {
    final Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.4" ) ); //$NON-NLS-1$
    txt_InputFile = new Text( container, SWT.BORDER );
    txt_InputFile.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    btn_inputShpFileBrowse = new Button( container, SWT.NONE );
    btn_inputShpFileBrowse.setText( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.11" ) ); //$NON-NLS-1$

    // Landuse property combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.12" ) ); //$NON-NLS-1$
    cmbMaxPerculationsRateProperty = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd0 = new GridData();
    gd0.horizontalAlignment = GridData.FILL;
    gd0.widthHint = 75;
    gd0.horizontalSpan = 2;
    cmbMaxPerculationsRateProperty.setEnabled( false );
    cmbMaxPerculationsRateProperty.setLayoutData( gd0 );

    // Sealing factor property combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.14" ) ); //$NON-NLS-1$
    cmbGWFactorProperty = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd1 = new GridData();
    gd1.horizontalAlignment = GridData.FILL;
    gd1.widthHint = 75;
    gd1.horizontalSpan = 2;
    cmbGWFactorProperty.setEnabled( false );
    cmbGWFactorProperty.setLayoutData( gd1 );

    new Label( container, SWT.NONE ).setText( " " ); //$NON-NLS-1$

    /* Coordinate system combo */
    final Composite crsContainer = new Composite( container, SWT.NULL );
    final GridLayout crsGridLayout = new GridLayout();
    crsGridLayout.numColumns = 1;
    crsContainer.setLayout( crsGridLayout );

    final GridData crsGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    crsGridData.horizontalSpan = 3;
    crsContainer.setLayoutData( crsGridData );

    initContents();
    addListeners();
    setControl( container );
    btn_inputShpFileBrowse.setFocus();
  }

  private void addListeners( )
  {
    btn_inputShpFileBrowse.addSelectionListener( this );
    cmbMaxPerculationsRateProperty.addSelectionListener( this );
    cmbGWFactorProperty.addSelectionListener( this );
    txt_InputFile.addModifyListener( this );
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * 
   * @param selection
   *          the selection or <code>null</code> if none
   * @param assetValueClassesList
   * @param damageFunctionNamesList
   */
  public void init( final ISelection selection )
  {
    if( !(selection instanceof IStructuredSelection) )
      return;

    m_fileExtensions.add( new String( "shp" ) ); //$NON-NLS-1$

    final Iterator< ? > iter = ((IStructuredSelection) selection).iterator();
    while( iter.hasNext() )
    {
      Object item = iter.next();
      if( item instanceof IFile )
      {
        final IFile file = (IFile) item;
        if( m_fileExtensions.contains( file.getFileExtension() ) )
        {
          initialSourcePath = file.getLocation();
          break;
        }
        item = file.getProject();
      }
    }
  }

  /**
   * Called by <code>createControl</code> to initialize the receiver's content based upon the cached selection provided
   * by the wizard.
   */
  private void initContents( )
  {
    if( initialSourcePath == null )
    {
      setPageComplete( false );
      return;
    }
    final IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
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

    final IPath sourceLoc = getSourceLocation();
    // if( sourceLoc == null || !(fileExtensions.contains( sourceLoc.getFileExtension() )) )
    if( sourceLoc == null || !sourceLoc.toFile().isFile() )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.6" ) ); //$NON-NLS-1$
      return;
    }
    else if( sourceLoc.getFileExtension().equalsIgnoreCase( "shp" ) && !sourceLoc.removeFileExtension().addFileExtension( "dbf" ).toFile().isFile() //$NON-NLS-1$ //$NON-NLS-2$
        && !sourceLoc.removeFileExtension().addFileExtension( "shx" ).toFile().isFile() ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.rrm.wizards.importGeologyData.ImportGeologyWizardPage.10" ) ); //$NON-NLS-1$
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
    final String text = txt_InputFile.getText().trim();
    if( text.length() == 0 )
      return null;
    m_sourcePath = new Path( text );
    if( !m_sourcePath.isAbsolute() )
      m_sourcePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().append( m_sourcePath );
    return m_sourcePath;
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

  @Override
  public boolean isPageComplete( )
  {
    return cmbMaxPerculationsRateProperty.isEnabled();
  }

  @Override
  public void widgetDefaultSelected( final SelectionEvent event )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void widgetSelected( final SelectionEvent event )
  {
    if( event.widget == btn_inputShpFileBrowse )
    {
      final String filenameFromDialog = getFilenameFromDialog( new String[] { "*.shp" }, null ); //$NON-NLS-1$
      if( filenameFromDialog != null )
        txt_InputFile.setText( filenameFromDialog );
    }
    getWizard().getContainer().updateButtons();
  }

  @Override
  public void modifyText( final ModifyEvent event )
  {
    // Initialize a variable with the no error status
    Status status = new Status( IStatus.OK, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
    msg_StatusLine = status;
    if( event.widget == txt_InputFile )
    {
      try
      {
        if( isTextNonEmpty( txt_InputFile ) )
        {
          final File file = new File( txt_InputFile.getText() );
          if( file.exists() && file.isFile() && file.canRead() )
          {
            final ShapeFile shape = new ShapeFile( FileUtilities.nameWithoutExtension( txt_InputFile.getText() ) );
            final String[] propertyNames = shape.getProperties();
            shape.close();
            cmbMaxPerculationsRateProperty.setItems( propertyNames );
            cmbMaxPerculationsRateProperty.select( 0 );
            cmbMaxPerculationsRateProperty.setEnabled( true );
            cmbGWFactorProperty.setItems( propertyNames );
            cmbGWFactorProperty.select( 0 );
            cmbGWFactorProperty.setEnabled( true );
          }
          else
          {
            cmbMaxPerculationsRateProperty.setEnabled( false );
            cmbGWFactorProperty.setEnabled( false );
          }
        }
        else
        {
          cmbMaxPerculationsRateProperty.setEnabled( false );
          cmbGWFactorProperty.setEnabled( false );
        }
      }
      catch( final Exception e )
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

  public String getMaxPerculationsRateProperty( )
  {
    return cmbMaxPerculationsRateProperty.getText();
  }

  public String getGWFactorProperty( )
  {
    return cmbGWFactorProperty.getText();
  }

}