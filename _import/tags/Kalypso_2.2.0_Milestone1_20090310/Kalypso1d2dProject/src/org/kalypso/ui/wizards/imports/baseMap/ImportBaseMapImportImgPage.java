package org.kalypso.ui.wizards.imports.baseMap;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

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
import org.kalypso.transformation.ui.CRSSelectionListener;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportBaseMapImportImgPage extends WizardPage
{
  private Text sourceFileField;

  private IPath initialSourcePath;

  private IPath m_sourcePath = null;

  List<String> fileExtensions = new LinkedList<String>();

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
    label_1.setText( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.4" ) ); //$NON-NLS-1$

    sourceFileField = new Text( container, SWT.BORDER );
    sourceFileField.addModifyListener( new ModifyListener()
    {
      public void modifyText( ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    sourceFileField.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Button button = new Button( container, SWT.NONE );
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( SelectionEvent e )
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

    m_crsPanel.setToolTipText( Messages.getString("org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.5") ); //$NON-NLS-1$

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_crsPanel.setSelectedCRS( m_crs );
    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      @Override
      protected void selectionChanged( String selectedCRS )
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
   *            the selection or <code>null</code> if none
   */
  @SuppressWarnings("unchecked")
  public void init( ISelection selection )
  {
    if( !(selection instanceof IStructuredSelection) )
      return;

    fileExtensions.add( new String( "tif" ) ); //$NON-NLS-1$
    fileExtensions.add( new String( "jpg" ) ); //$NON-NLS-1$
    // fileExtensions.add( new String( "gml" ) );

    // Find the first plugin.xml file.
    Iterator iter = ((IStructuredSelection) selection).iterator();
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
    sourceFileField.setText( path.toString() );
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
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.6" ) ); //$NON-NLS-1$
      return;
    }
    else if( sourceLoc.getFileExtension().equalsIgnoreCase( "tif" ) && !sourceLoc.removeFileExtension().addFileExtension( "tfw" ).toFile().isFile() ) //$NON-NLS-1$ //$NON-NLS-2$
    {
      setMessage( null );
      setErrorMessage( Messages.getFormatString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.5", new Object[] { sourceLoc.lastSegment(), //$NON-NLS-1$
          sourceLoc.removeFileExtension().addFileExtension( "tfw" ).lastSegment() } ) ); //$NON-NLS-1$
      return;
    }
    else if( sourceLoc.getFileExtension().equalsIgnoreCase( "jpg" ) && !sourceLoc.removeFileExtension().addFileExtension( "jgw" ).toFile().isFile() ) //$NON-NLS-1$ //$NON-NLS-2$
    {
      setMessage( null );
      setErrorMessage( Messages.getFormatString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapImportImgPage.5", new Object[] { sourceLoc.lastSegment(), //$NON-NLS-1$
          sourceLoc.removeFileExtension().addFileExtension( "jgw" ).lastSegment() } ) ); //$NON-NLS-1$
      return;
    }
    setMessage( null );
    setErrorMessage( null );
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
    IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    sourceFileField.setText( path.toString() );
  }

  /**
   * Open a file dialog for selecting a file
   * 
   * @param path
   *            the initially selected file
   * @param mustExist
   *            <code>true</code> if the selected file must already exist, else <code>false</code>
   * @return the newly selected file or <code>null</code>
   */
  private IPath browse( IPath path )
  {
    FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    dialog.setFilterExtensions( new String[] { "*.tif; *.jpg" } ); //$NON-NLS-1$
    // dialog.setFilterExtensions( (String[]) fileExtensions.toArray() );
    if( path != null )
    {
      if( path.segmentCount() > 1 )
        dialog.setFilterPath( path.removeLastSegments( 1 ).toOSString() );
      if( path.segmentCount() > 0 )
        dialog.setFileName( path.lastSegment() );
    }
    String result = dialog.open();
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
    String text = sourceFileField.getText().trim();
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
