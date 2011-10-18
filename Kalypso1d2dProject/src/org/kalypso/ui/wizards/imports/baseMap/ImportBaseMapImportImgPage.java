package org.kalypso.ui.wizards.imports.baseMap;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.contribs.eclipse.swt.widgets.FileDialogUtils;
import org.kalypso.contribs.java.io.filter.IgnoreCaseFilenameFilter;
import org.kalypso.grid.WorldFileFormat;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * FIXME: Totally copy/paste and double code.... Combine this one with the general image import page!
 * 
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportBaseMapImportImgPage extends WizardPage
{
  private Text m_sourceFileField;

  private IPath m_initialSourcePath;

  private IPath m_sourcePath = null;

  protected String m_crs;

  private CRSSelectionPanel m_crsPanel;

  public ImportBaseMapImportImgPage( )
  {
    super( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.0" ), "", ImageProvider.IMAGE_UTIL_BERICHT_WIZ ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.0" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.description" ) ); //$NON-NLS-1$
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

    final Label label_1 = new Label( container, SWT.NONE );
    final GridData gridData_1 = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    label_1.setLayoutData( gridData_1 );
    label_1.setText( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.4" ) ); //$NON-NLS-1$

    m_sourceFileField = new Text( container, SWT.BORDER );
    m_sourceFileField.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_sourceFileField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Button button = new Button( container, SWT.NONE );
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        browseForSourceFile();
      }
    } );
    button.setText( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.BrowseButton" ) ); //$NON-NLS-1$

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

    m_crsPanel.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.5" ) ); //$NON-NLS-1$

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

    button.setFocus();
    initContents();
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * 
   * @param selection
   *          the selection or <code>null</code> if none
   */
  public void init( final ISelection selection )
  {
    if( !(selection instanceof IStructuredSelection) )
      return;

    final List<String> fileExtensions = new LinkedList<String>();

    final WorldFileFormat[] availableFormats = WorldFileFormat.getAvailableFormats();
    for( final WorldFileFormat worldFileFormat : availableFormats )
      fileExtensions.add( worldFileFormat.getImgFileExtension() );

    final Iterator< ? > iter = ((IStructuredSelection) selection).iterator();
    while( iter.hasNext() )
    {
      final Object item = iter.next();
      if( item instanceof IFile )
      {
        final IFile file = (IFile) item;
        if( fileExtensions.contains( file.getFileExtension() ) )
        {
          m_initialSourcePath = file.getLocation();
          break;
        }
      }
    }
  }

  /**
   * Called by <code>createControl</code> to initialize the receiver's content based upon the cached selection provided
   * by the wizard.
   */
  private void initContents( )
  {
    if( m_initialSourcePath == null )
    {
      setPageComplete( false );
      return;
    }
    final IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    IPath path = m_initialSourcePath;
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    m_sourceFileField.setText( path.toString() );
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

    setMessage( null );
    setErrorMessage( null );

    final IPath sourceLoc = getSourceLocation();
    if( sourceLoc == null || sourceLoc.isEmpty() )
    {
      // no error message, but page is not complete
      return;
    }

    final File file = sourceLoc.toFile();
    if( !file.isFile() )
    {
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.6" ) ); //$NON-NLS-1$
      return;
    }

    final String fileExtension = sourceLoc.getFileExtension();
    final WorldFileFormat[] availableFormats = WorldFileFormat.getAvailableFormats();
    for( final WorldFileFormat worldFileFormat : availableFormats )
    {
      if( worldFileFormat.getImgFileExtension().equalsIgnoreCase( fileExtension ) )
      {
        final IPath worldFilePath = sourceLoc.removeFileExtension().addFileExtension( worldFileFormat.getWorldFileExtension() );
        final String worldFileName = worldFilePath.lastSegment();
        final FilenameFilter filter = new IgnoreCaseFilenameFilter( worldFileName );
        if( file.getParentFile().listFiles( filter ).length == 0 )
        {
          final String msg = Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.1", worldFileName ); //$NON-NLS-1$
          setErrorMessage( msg );
          return;
        }
      }
    }

    setPageComplete( true );
  }

  /**
   * Open a file browser dialog to locate a source file
   */
  protected void browseForSourceFile( )
  {
    IPath path = browse( getSourceLocation() );
    if( path == null )
      return;
    final IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    m_sourceFileField.setText( path.toString() );
  }

  /**
   * Open a file dialog for selecting a file
   * 
   * @param path
   *          the initially selected file
   * @param mustExist
   *          <code>true</code> if the selected file must already exist, else <code>false</code>
   * @return the newly selected file or <code>null</code>
   */
  private IPath browse( final IPath path )
  {
    // TODO: replace with file chooser stuff. Lots of small features are missing....
    final FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );

    String[] filterNames = new String[0];
    String[] filterExtensions = new String[0];

    filterNames = (String[]) ArrayUtils.add( filterNames, WorldFileFormat.getAllSuportedFilterName() );
    filterExtensions = (String[]) ArrayUtils.add( filterExtensions, WorldFileFormat.getAllSupportedFilters() );

    final WorldFileFormat[] availableFormats = WorldFileFormat.getAvailableFormats();
    for( final WorldFileFormat worldFileFormat : availableFormats )
    {
      filterNames = (String[]) ArrayUtils.add( filterNames, worldFileFormat.getFilterName() );
      filterExtensions = (String[]) ArrayUtils.add( filterExtensions, worldFileFormat.getFilterExtension() );
    }

    filterNames = (String[]) ArrayUtils.add( filterNames, FileDialogUtils.FILTERNAME_ALL_FILES );
    filterExtensions = (String[]) ArrayUtils.add( filterExtensions, FileDialogUtils.FILTER_ALL_FILES );

    dialog.setFilterNames( filterNames );
    dialog.setFilterExtensions( filterExtensions );
    dialog.setFilterIndex( 0 );
    if( path != null )
    {
      if( path.segmentCount() > 1 )
        dialog.setFilterPath( path.removeLastSegments( 1 ).toOSString() );
      if( path.segmentCount() > 0 )
        dialog.setFileName( path.lastSegment() );
    }
    final String result = dialog.open();
    if( result == null )
      return null;
    return new Path( result );
  }

  /**
   * Answer the source file location or <code>null</code> if unspecified
   */
  public IPath getSourceLocation( )
  {
    // if(m_sourcePath != null)
    // return m_sourcePath;
    final String text = m_sourceFileField.getText().trim();
    if( text.length() == 0 )
      return null;
    m_sourcePath = new Path( text );
    if( !m_sourcePath.isAbsolute() )
      m_sourcePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().append( m_sourcePath );
    return m_sourcePath;
  }

  public String getCoordinateSystem( )
  {
    return m_crs;
  }

  /**
   * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
   */
  @Override
  public IWizardPage getNextPage( )
  {
    return null;
  }

}
