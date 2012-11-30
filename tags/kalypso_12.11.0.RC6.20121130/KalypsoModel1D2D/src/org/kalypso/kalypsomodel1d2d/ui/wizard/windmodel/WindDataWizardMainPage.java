package org.kalypso.kalypsomodel1d2d.ui.wizard.windmodel;

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
import org.kalypso.kalypsomodel1d2d.conv.wind.WindDataConverterFactory.EWindFileTypes;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * Wizard main page for wind data import wizard.
 *
 * selection of source file, filtered by extensions, selection of coordinate system.
 */
public class WindDataWizardMainPage extends WizardPage
{
  private Text m_filename;

  private IPath m_initialSourcePath;

  private List<String> m_fileExtensions = new ArrayList<>();

  public Text m_tileTitle;

  //removed - not usable in case of actually needed data(wind field)
//  public Text m_height;

  public Text m_tileDescription;

  private CRSSelectionPanel m_crsPanel;

  protected String m_crs;

  public WindDataWizardMainPage( )
  {
    super( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.0" ) ); //$NON-NLS-1$
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.4" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.1" ) ); //$NON-NLS-1$
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
    label_1.setText( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.2" ) ); //$NON-NLS-1$
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
    button.setText( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.Browse" ) ); //$NON-NLS-1$

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

    m_crsPanel.setToolTipText( Messages.getString( "org.kalypso.ui.wizards.imports.windmodel.WindMainPage.5" ) ); //$NON-NLS-1$

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
    label_3.setText( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.7" ) ); //$NON-NLS-1$
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

//    final Label label_hight = new Label( container, SWT.NONE );
//    label_hight.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING ) );
//    label_hight.setText( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.8" ) ); //$NON-NLS-1$
//    m_height = new Text( container, SWT.BORDER );
//    m_height.addModifyListener( new ModifyListener()
//    {
//      public void modifyText( final ModifyEvent e )
//      {
//        updatePageComplete();
//      }
//    } );
//    final GridData gridData_hight = new GridData( GridData.FILL_HORIZONTAL );
//    gridData_hight.horizontalSpan = 2;
//    m_height.setLayoutData( gridData_hight );

    final Label label_4 = new Label( container, SWT.NONE );
    label_4.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING | GridData.VERTICAL_ALIGN_BEGINNING ) );
    label_4.setText( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.9" ) ); //$NON-NLS-1$
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

    m_fileExtensions = EWindFileTypes.getExtentionsList();
    //    m_fileExtensions.add( "dat" ); //$NON-NLS-1$
    //    m_fileExtensions.add( "gz" ); //$NON-NLS-1$
    //    m_fileExtensions.add( "asc" ); //$NON-NLS-1$
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

    if( sourceLoc == null || !(isValidExtention( sourceLoc.getFileExtension() )) )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.3" ) ); //$NON-NLS-1$
      return;
    }
    if( m_tileTitle.getText().trim().length() == 0 )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.3" ) ); //$NON-NLS-1$
      return;
    }
//    if( m_height.getText().trim().length() != 0 )
//    {
//      try{
//        Double.parseDouble( m_height.getText().trim() );
//      }
//      catch (Exception e) {
//        setMessage( null );
//        setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.windModel.Wind.5" ) ); //$NON-NLS-1$
//        return;
//      }
//    }
    setPageComplete( true );
    setMessage( null );
    setErrorMessage( null );
  }

  private boolean isValidExtention( final String fileExtension )
  {
    if( m_fileExtensions.contains( fileExtension ) )
      return true;
    for( final String lStrActExt : m_fileExtensions )
    {
      if( lStrActExt.toLowerCase().endsWith( fileExtension.toLowerCase() ) )
        return true;
    }
    return false;
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

  }

  private IPath browse( final IPath path )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    // TODO: what about m_fileExtensions ???
    dialog.setFilterExtensions( getFileExts() ); //$NON-NLS-1$
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

  private String[] getFileExts( )
  {
    // new String[] { "*.dat; *.asc; *.dat.gz" }
    // String[] lArrExts = new String[ 1 ];
    String lStrExtensions = "";  //$NON-NLS-1$
    // int lIntCount = 0;
    for( final String lStrFileExt : m_fileExtensions )
    {
      // lArrExts[ lIntCount++ ]
      lStrExtensions += "*." + lStrFileExt + "; "; //$NON-NLS-1$ //$NON-NLS-2$
    }
    return new String[] { lStrExtensions };
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

  public String getModelName( )
  {
    return m_tileTitle.getText();
  }

  public String getCoordinateSystem( )
  {
    return m_crs;
  }
}
