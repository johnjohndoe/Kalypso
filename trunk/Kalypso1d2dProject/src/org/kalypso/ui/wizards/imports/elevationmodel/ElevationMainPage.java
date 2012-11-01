package org.kalypso.ui.wizards.imports.elevationmodel;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
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
import org.kalypso.kalypso1d2d.internal.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Madanagopal
 */
public class ElevationMainPage extends WizardPage
{
  private Text m_filename;

  private IPath m_initialSourcePath;

  private final List<String> m_fileExtensions = new ArrayList<>();

  public Text m_tileTitle;

  public Text m_tileDescription;

  private CRSSelectionPanel m_crsPanel;

  protected String m_crs;

  public ElevationMainPage( )
  {
    super( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ) ); //$NON-NLS-1$
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.4" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.1" ) ); //$NON-NLS-1$
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
    label_1.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.2" ) ); //$NON-NLS-1$
    m_filename = new Text( container, SWT.BORDER );
    m_filename.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    m_filename.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Button button = new Button( container, SWT.NONE );
    button.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        browseForSourceFile();
      }
    } );
    button.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.Browse" ) ); //$NON-NLS-1$

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

    m_crsPanel.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationmodel.ElevationMainPage.5" ) ); //$NON-NLS-1$

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

    final Label label_3 = new Label( container, SWT.NONE );
    label_3.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
    label_3.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.7" ) ); //$NON-NLS-1$
    m_tileTitle = new Text( container, SWT.BORDER );
    m_tileTitle.addModifyListener( new ModifyListener()
    {
      @Override
      public void modifyText( final ModifyEvent e )
      {
        updatePageComplete();
      }
    } );
    final GridData gridData_3 = new GridData( GridData.FILL_HORIZONTAL );
    gridData_3.horizontalSpan = 2;
    m_tileTitle.setLayoutData( gridData_3 );

    final Label label_4 = new Label( container, SWT.NONE );
    label_4.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING | GridData.VERTICAL_ALIGN_BEGINNING ) );
    label_4.setText( "" ); //$NON-NLS-1$
    m_tileDescription = new Text( container, SWT.BORDER | SWT.MULTI );
    final GridData gridData_4 = new GridData( GridData.FILL_HORIZONTAL );
    gridData_4.horizontalSpan = 2;
    gridData_4.heightHint = 100;
    m_tileDescription.setLayoutData( gridData_4 );
    m_tileDescription.setText( "" ); //$NON-NLS-1$

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

    m_fileExtensions.add( "hmo" ); //$NON-NLS-1$
    m_fileExtensions.add( "asc" ); //$NON-NLS-1$
    m_fileExtensions.add( "asg" ); //$NON-NLS-1$
    // Find the first plugin.xml file.
    final Iterator< ? > iter = ((IStructuredSelection) selection).iterator();
    while( iter.hasNext() )
    {
      Object item = iter.next();
      if( item instanceof IFile )
      {
        final IFile file = (IFile) item;
        if( m_fileExtensions.contains( file.getFileExtension() ) )
        {
          m_initialSourcePath = file.getLocation();
          break;
        }
        item = file.getProject();
      }
    }
    setPageComplete( false );
  }

  /**
   * Called by <code>createControl</code> to initialize the receiver's content based upon the cached selection provided
   * by the wizard.
   */
  private void initContents( )
  {
    if( m_initialSourcePath == null )
      return;
    final IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    IPath path = m_initialSourcePath;
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    m_filename.setText( path.toString() );
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

    // source file not empty
    // tile title not empty

    final IPath sourceLoc = getSourceLocation();
    if( sourceLoc == null || !(m_fileExtensions.contains( sourceLoc.getFileExtension() )) )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.3" ) ); //$NON-NLS-1$
      return;
    }
    if( m_tileTitle.getText().trim().length() == 0 )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.3" ) ); //$NON-NLS-1$
      return;
    }
    setPageComplete( true );
    setMessage( null );
    setErrorMessage( null );
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
    m_filename.setText( path.toString() );

    // Multiple Elevation Model Select.. Still under development
    /*
     * IPath[] ar = null; IPath[] path = browse( getSourceLocation(), false ); if( path == null ) return; IPath rootLoc
     * = ResourcesPlugin.getWorkspace().getRoot().getLocation(); for (int i = 0; i<path.length;i++){ if(
     * rootLoc.isPrefixOf( path[i] ) ) ar[i] = path[i].setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
     * sourceFileField.setText(ar.toString()); }
     */

  }

  /**
   * Multiple Elevation Model Select.. Still under development Open a file dialog for selecting a file
   *
   * @param path
   *          the initially selected file
   * @param mustExist
   *          <code>true</code> if the selected file must already exist, else <code>false</code>
   * @return the newly selected file or <code>null</code>
   */

  // private IPath[] browse( IPath path, boolean mustExist )
  // {
  // String fileFilterPath;
  // String[] selectedFiles;
  // Path[] arrPaths = null;
  // FileDialog dialog = new FileDialog( getShell(), SWT.OPEN|SWT.MULTI );
  // dialog.setFilterExtensions( new String[] { "*.asc", "*.hmo" } );
  // if( path != null )
  // {
  // if( path.segmentCount() > 1 )
  // dialog.setFilterPath( path.removeLastSegments( 1 ).toOSString() );
  // if( path.segmentCount() > 0 )
  // dialog.setFileName( path.lastSegment() );
  // }
  // String result = dialog.open();
  // if( result == null )
  // return null;
  // else {
  // fileFilterPath = dialog.getFilterPath();
  // selectedFiles = dialog.getFileNames();
  // for(int i=0; i<selectedFiles.length; i++) {
  // selectedFiles[i]= dialog.getFilterPath()+"\\"+selectedFiles[i];
  // arrPaths[i]= new Path(selectedFiles[i]);
  // }
  // return arrPaths;
  // }
  //
  // }
  private IPath browse( final IPath path )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    // TODO: what about m_fileExtensions ???
    dialog.setFilterExtensions( new String[] { "*.hmo; *.asc; *.asg" } ); //$NON-NLS-1$
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
    final String text = m_filename.getText().trim();
    if( text.length() == 0 )
      return null;
    final IPath path = new Path( text );
    if( !path.isAbsolute() )
      return ResourcesPlugin.getWorkspace().getRoot().getLocation().append( path );
    else
      return path;
  }

  public String getDescriptionForFileArea( )
  {
    return m_tileDescription.getText();
  }

  public String getNameForFile( )
  {
    return m_tileTitle.getText();
  }

  public String getCoordinateSystem( )
  {
    return m_crs;
  }
}
