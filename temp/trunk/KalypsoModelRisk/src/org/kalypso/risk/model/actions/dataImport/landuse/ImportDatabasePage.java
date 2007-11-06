package org.kalypso.risk.model.actions.dataImport.landuse;

import java.io.File;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.UrlResolver;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.risk.model.schema.KalypsoRiskSchemaCatalog;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.opengis.cs.CS_CoordinateSystem;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportDatabasePage extends WizardPage
{
  private Text m_txtExternalProjectPath;

  private Combo m_cmbDamageFunctionsSources;

  private Combo m_cmbAssetValuesSources;

  private IPath m_sourcePath = null;

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  private Button btn_inputFileBrowse;

  private IFolder m_scenarioFolder;

  public ImportDatabasePage( )
  {
    super( Messages.getString( "ImportLanduseShpPage.0" ), "", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "ImportLanduseShpPage.2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportLanduseShpPage.3" ) ); //$NON-NLS-1$

    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    m_scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
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

    m_txtExternalProjectPath = new Text( container, SWT.BORDER );
    m_txtExternalProjectPath.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    btn_inputFileBrowse = new Button( container, SWT.NONE );
    btn_inputFileBrowse.setText( Messages.getString( "ImportLanduseShpPage.11" ) ); //$NON-NLS-1$

    createLine( container, 3 );

    // Landuse property combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "ImportLanduseShpPage.12" ) ); //$NON-NLS-1$
    m_cmbDamageFunctionsSources = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd0 = new GridData();
    gd0.horizontalAlignment = GridData.FILL;
    gd0.widthHint = 75;
    m_cmbDamageFunctionsSources.setEnabled( true );
    m_cmbDamageFunctionsSources.setLayoutData( gd0 );
    try
    {
      final URL url = m_scenarioFolder.getFile( "models/PredefinedDataset.gml" ).getLocationURI().toURL();
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, new UrlResolver(), null );
      final List<Feature> property = (FeatureList) workspace.getRootFeature().getProperty( new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "damageFunctionsCollection" ) );
      for( final Feature feature : property )
      {
        final List<String> names = (List<String>) feature.getProperty( new QName( NS.GML3, "name" ) );
        if( names != null && names.size() > 0 )
          m_cmbDamageFunctionsSources.add( names.get( 0 ) );
      }
      m_cmbDamageFunctionsSources.select( 0 );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    createLine( container, 3 );

    // Coordinate system combo box
    new Label( container, SWT.NONE ).setText( Messages.getString( "ImportLanduseShpPage.13" ) ); //$NON-NLS-1$
    m_cmbAssetValuesSources = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    final GridData gd2 = new GridData();
    gd2.horizontalAlignment = GridData.FILL;
    gd2.widthHint = 75;
    m_cmbAssetValuesSources.setEnabled( true );
    m_cmbAssetValuesSources.setLayoutData( gd2 );
    try
    {
      final URL url = m_scenarioFolder.getFile( "models/PredefinedDataset.gml" ).getLocationURI().toURL();
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( url, new UrlResolver(), null );
      final List<Feature> property = (FeatureList) workspace.getRootFeature().getProperty( new QName( KalypsoRiskSchemaCatalog.NS_PREDEFINED_DATASET, "assetValueClassesCollection" ) );
      for( final Feature feature : property )
      {
        final List<String> names = (List<String>) feature.getProperty( new QName( NS.GML3, "name" ) );
        if( names != null && names.size() > 0 )
          m_cmbAssetValuesSources.add( names.get( 0 ) );
      }
      m_cmbAssetValuesSources.select( 0 );
    }
    catch( Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    btn_inputFileBrowse.setFocus();
    setControl( container );
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * 
   * @param selection
   *            the selection or <code>null</code> if none
   */
  public void init( ISelection selection )
  {
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
    final String text = m_txtExternalProjectPath.getText().trim();
    if( text.length() == 0 )
      return null;
    m_sourcePath = new Path( text );
    if( !m_sourcePath.isAbsolute() )
      m_sourcePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().append( m_sourcePath );
    return m_sourcePath;
  }

  public CS_CoordinateSystem getCoordinateSystem( )
  {
    return ConvenienceCSFactory.getInstance().getOGCCSByName( m_cmbAssetValuesSources.getText() );
  }

  public String getLanduseProperty( )
  {
    return m_cmbDamageFunctionsSources.getText();
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
    return m_cmbDamageFunctionsSources.isEnabled();
  }

}
