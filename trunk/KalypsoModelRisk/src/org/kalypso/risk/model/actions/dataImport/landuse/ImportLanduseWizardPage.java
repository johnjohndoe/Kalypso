package org.kalypso.risk.model.actions.dataImport.landuse;

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
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
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.dialogs.ListDialog;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportLanduseWizardPage extends WizardPage implements SelectionListener, ModifyListener
{
  private Text txt_InputFile;

  protected String m_crs;

  private Combo cmb_ShapeProperty;

  private IPath initialSourcePath;

  private IPath m_sourcePath = null;

  private final List<String> m_fileExtensions;

  private Button btn_inputLanduseFileBrowse;

  private Text m_txtExternalProjectPath;

  private Combo m_cmbDamageFunctionsSources;

  private Combo m_cmbAssetValuesSources;

  private CRSSelectionPanel m_crsPanel;

  private Button btn_existingDatabaseBrowse;

  private Button[] m_radioButtons;

  private Label m_lblRadioSelection_1;

  private Label m_lblRadioSelection_21;

  private Label m_lblRadioSelection_22;

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  private List<String> m_damageFunctionNamesList;

  private List<String> m_assetValueClassesList;

  public ImportLanduseWizardPage( )
  {
    super( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.0" ), "", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.3" ) ); //$NON-NLS-1$

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

    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.4" ) ); //$NON-NLS-1$
    txt_InputFile = new Text( container, SWT.BORDER );
    txt_InputFile.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    btn_inputLanduseFileBrowse = new Button( container, SWT.NONE );
    btn_inputLanduseFileBrowse.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.11" ) ); //$NON-NLS-1$

    // Landuse property combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.12" ) ); //$NON-NLS-1$
    cmb_ShapeProperty = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd0 = new GridData();
    gd0.horizontalAlignment = GridData.FILL;
    gd0.widthHint = 75;
    cmb_ShapeProperty.setEnabled( false );
    cmb_ShapeProperty.setLayoutData( gd0 );

    new Label( container, SWT.NONE ).setText( " " ); //$NON-NLS-1$

    /* Coordinate system combo */
    final Composite crsContainer = new Composite( container, SWT.NULL );
    final GridLayout crsGridLayout = new GridLayout();
    crsGridLayout.numColumns = 1;
    crsContainer.setLayout( crsGridLayout );

    final GridData crsGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    crsGridData.horizontalSpan = 3;
    crsContainer.setLayoutData( crsGridData );

    m_crsPanel = new CRSSelectionPanel( crsContainer, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_crsPanel.setToolTipText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.21" ) ); //$NON-NLS-1$

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_crsPanel.setSelectedCRS( m_crs );
    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      @Override
      protected void selectionChanged( final String selectedCRS )
      {
        m_crs = selectedCRS;
        updatePageComplete();
      }
    } );

    final Group dbGroup = new Group( crsContainer, SWT.NONE );
    final GridLayout groupGridLayout = new GridLayout();
    groupGridLayout.numColumns = 3;
    final GridData groupGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    groupGridData.horizontalSpan = 3;
    dbGroup.setLayout( groupGridLayout );
    dbGroup.setLayoutData( groupGridData );
    dbGroup.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.14" ) ); //$NON-NLS-1$

    final GridData radioLayoutData = new GridData( SWT.FILL, SWT.FILL, true, true );
    radioLayoutData.horizontalSpan = 3;
    final GridData comboLayoutData = new GridData( SWT.FILL, SWT.FILL, true, true );
    comboLayoutData.horizontalSpan = 2;

    m_radioButtons = new Button[3];
    m_radioButtons[0] = new Button( dbGroup, SWT.RADIO );
    m_radioButtons[0].setSelection( true );
    m_radioButtons[0].setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.15" ) ); //$NON-NLS-1$
    m_radioButtons[0].setLayoutData( radioLayoutData );

    m_radioButtons[1] = new Button( dbGroup, SWT.RADIO );
    m_radioButtons[1].setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.16" ) ); //$NON-NLS-1$
    m_radioButtons[1].setLayoutData( radioLayoutData );
    m_lblRadioSelection_1 = new Label( dbGroup, SWT.NONE );
    m_lblRadioSelection_1.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.17" ) ); //$NON-NLS-1$
    m_lblRadioSelection_1.setEnabled( false );
    m_txtExternalProjectPath = new Text( dbGroup, SWT.BORDER );
    m_txtExternalProjectPath.setEnabled( false );
    m_txtExternalProjectPath.setEditable( false );
    m_txtExternalProjectPath.setLayoutData( new GridData( GridData.FILL_HORIZONTAL | GridData.GRAB_HORIZONTAL ) );
    btn_existingDatabaseBrowse = new Button( dbGroup, SWT.NONE );
    btn_existingDatabaseBrowse.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.11" ) ); //$NON-NLS-1$
    btn_existingDatabaseBrowse.setEnabled( false );

    m_radioButtons[2] = new Button( dbGroup, SWT.RADIO );
    m_radioButtons[2].setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.18" ) ); //$NON-NLS-1$
    m_radioButtons[2].setLayoutData( radioLayoutData );
    m_lblRadioSelection_21 = new Label( dbGroup, SWT.NONE );
    m_lblRadioSelection_21.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.19" ) ); //$NON-NLS-1$
    m_lblRadioSelection_21.setEnabled( false );
    m_cmbDamageFunctionsSources = new Combo( dbGroup, SWT.BORDER | SWT.READ_ONLY );
    m_cmbDamageFunctionsSources.setEnabled( false );
    m_cmbDamageFunctionsSources.setLayoutData( comboLayoutData );

    m_lblRadioSelection_22 = new Label( dbGroup, SWT.NONE );
    m_lblRadioSelection_22.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.20" ) ); //$NON-NLS-1$
    m_lblRadioSelection_22.setEnabled( false );
    m_cmbAssetValuesSources = new Combo( dbGroup, SWT.BORDER | SWT.READ_ONLY );
    m_cmbAssetValuesSources.setEnabled( false );
    m_cmbAssetValuesSources.setLayoutData( comboLayoutData );

    initContents();
    addListeners();
    setControl( container );
    btn_inputLanduseFileBrowse.setFocus();
  }

  private void addListeners( )
  {
    btn_inputLanduseFileBrowse.addSelectionListener( this );
    btn_existingDatabaseBrowse.addSelectionListener( this );
    cmb_ShapeProperty.addSelectionListener( this );
    m_radioButtons[0].addSelectionListener( this );
    m_radioButtons[1].addSelectionListener( this );
    m_radioButtons[2].addSelectionListener( this );
    txt_InputFile.addModifyListener( this );
    // m_txtExternalProjectPath.addModifyListener( this );
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * 
   * @param selection
   *          the selection or <code>null</code> if none
   * @param assetValueClassesList
   * @param damageFunctionNamesList
   */
  public void init( final ISelection selection, final List<String> damageFunctionNamesList, final List<String> assetValueClassesList )
  {
    m_damageFunctionNamesList = damageFunctionNamesList;
    m_assetValueClassesList = assetValueClassesList;
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
    m_cmbDamageFunctionsSources.setItems( m_damageFunctionNamesList.toArray( new String[0] ) );
    m_cmbAssetValuesSources.setItems( m_assetValueClassesList.toArray( new String[0] ) );
    m_cmbDamageFunctionsSources.select( 0 );
    m_cmbAssetValuesSources.select( 0 );

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
      setErrorMessage( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.6" ) ); //$NON-NLS-1$
      return;
    }
    else if( sourceLoc.getFileExtension().equalsIgnoreCase( "shp" ) && !sourceLoc.removeFileExtension().addFileExtension( "dbf" ).toFile().isFile() //$NON-NLS-1$ //$NON-NLS-2$
        && !sourceLoc.removeFileExtension().addFileExtension( "shx" ).toFile().isFile() ) //$NON-NLS-1$
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.risk.model.actions.dataImport.landuse.ImportLanduseWizardPage.10" ) ); //$NON-NLS-1$
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

  private String getExistingProjectDialog( )
  {
    final ListDialog dialog = new ListDialog( getShell() );
    dialog.setLabelProvider( new WorkbenchLabelProvider() );
    dialog.setContentProvider( new ArrayContentProvider() );
    dialog.setInput( ResourcesPlugin.getWorkspace().getRoot().getProjects() );
    if( dialog.open() != Window.OK )
      return ""; //$NON-NLS-1$
    final Object[] result = dialog.getResult();
    if( result.length > 0 )
      return ((IProject) result[0]).getName();
    return ""; //$NON-NLS-1$
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
    if( m_radioButtons[1].getSelection() )
      return cmb_ShapeProperty.isEnabled() && isTextNonEmpty( m_txtExternalProjectPath );
    else
      return cmb_ShapeProperty.isEnabled();
  }

  @Override
  public void widgetDefaultSelected( final SelectionEvent event )
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void widgetSelected( final SelectionEvent event )
  {
    if( event.widget == btn_inputLanduseFileBrowse )
    {
      final String filenameFromDialog = getFilenameFromDialog( new String[] { "*.shp" }, null ); //$NON-NLS-1$
      if( filenameFromDialog != null )
        txt_InputFile.setText( filenameFromDialog );
    }
    else if( event.widget == btn_existingDatabaseBrowse )
    {
      m_txtExternalProjectPath.setText( getExistingProjectDialog() );
    }
    else
    {
      m_lblRadioSelection_1.setEnabled( m_radioButtons[1].getSelection() );
      m_txtExternalProjectPath.setEnabled( m_radioButtons[1].getSelection() );
      btn_existingDatabaseBrowse.setEnabled( m_radioButtons[1].getSelection() );
      m_lblRadioSelection_21.setEnabled( m_radioButtons[2].getSelection() );
      m_lblRadioSelection_22.setEnabled( m_radioButtons[2].getSelection() );
      m_cmbDamageFunctionsSources.setEnabled( m_radioButtons[2].getSelection() );
      m_cmbAssetValuesSources.setEnabled( m_radioButtons[2].getSelection() );
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

  public String getCoordinateSystem( )
  {
    return m_crs;
  }

  public String getLanduseProperty( )
  {
    return cmb_ShapeProperty.getText();
  }

  public String getExternalProjectName( )
  {
    return m_txtExternalProjectPath.getText();
  }

  public int getSelectedDatabaseOption( )
  {
    if( m_radioButtons[0].getSelection() )
      return ImportLanduseWizard.DB_CREATE_NEW;
    if( m_radioButtons[1].getSelection() )
      return ImportLanduseWizard.DB_IMPORT;
    if( m_radioButtons[2].getSelection() )
      return ImportLanduseWizard.DB_USE_PREDEFINED;
    return ImportLanduseWizard.DB_UNDEFINED_SELECTION;
  }

  public String getDamageFunctionsCollectionName( )
  {
    return m_cmbDamageFunctionsSources.getText();
  }

  public String getAssetValuesCollectionName( )
  {
    return m_cmbAssetValuesSources.getText();
  }
}