package org.kalypso.ui.wizards.imports.elevationmodel;

import java.util.Iterator;
import java.util.LinkedList;
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
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ui.wizards.imports.Messages;
import org.kalypso.ui.wizards.imports.roughness.DataContainer;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;

/**
 * @author Madanagopal
 */
public class ElevationMainPage extends WizardPage
{
  private Text sourceFileField;

  private IPath initialSourcePath;

  List<String> fileExtensions = new LinkedList<String>();

  private Combo coordinateSystem_Combo;

  public Text nameForFileText;

  public Text descriptionForFileArea;

  private Label statusText;
//  static private IPreferenceStore preferenceStore =  
    //KalypsoModelSimulationBase.getDefault().getPreferenceStore();

  //private IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();

  public ElevationMainPage( )
  {
    super( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.0" ) );
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.4" ) );
    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.1" ) );
  }
 

  /**
   * Creates the top level control for this dialog page under the given parent composite, then calls
   * <code>setControl</code> so that the created control can be accessed via <code>getControl</code>
   * 
   * @param parent
   *          the parent composite
   */
  public void createControl( Composite parent )
  {
    Composite container = new Composite( parent, SWT.NULL );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.numColumns = 3;
    container.setLayout( gridLayout );
    setControl( container );

    // final Label label = new Label( container, SWT.NONE );
    // final GridData gridData = new GridData();
    // gridData.horizontalSpan = 3;
    // label.setLayoutData( gridData );
    // label.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.1" ) );

    final Label label_1 = new Label( container, SWT.NONE );
    final GridData gridData_1 = new GridData( GridData.HORIZONTAL_ALIGN_END );
    label_1.setLayoutData( gridData_1 );
    label_1.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.2" ) );

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
      public void widgetSelected( SelectionEvent e )
      {
        browseForSourceFile();
      }
    } );
    button.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.Browse" ) );

    // Coordinate system combo box
    Label coordinateLabel = new Label( container, SWT.NONE );
    coordinateLabel.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.12" ) ); //$NON-NLS-1$
    coordinateLabel.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_END ) );

    coordinateSystem_Combo = new Combo( container, SWT.BORDER | SWT.READ_ONLY );
    coordinateSystem_Combo.setItems( (new ConvenienceCSFactoryFull()).getKnownCS() );
    final int index_GausKrueger = coordinateSystem_Combo.indexOf( DataContainer.GAUS_KRUEGER );
    coordinateSystem_Combo.select( index_GausKrueger > -1 ? index_GausKrueger : 0 );

    GridData gd = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gd.horizontalSpan = 2;
    coordinateSystem_Combo.setEnabled( true );
    coordinateSystem_Combo.setLayoutData( gd );

    final Label nameForFile = new Label( container, SWT.NONE );
    nameForFile.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_END ) );
    nameForFile.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.7" ) );
    nameForFileText = new Text( container, SWT.BORDER );  

   
    GridData gridData = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING);
    gridData.horizontalSpan = 2;
    nameForFileText.setLayoutData( gridData );
    
//    statusText = new Label( container, SWT.NONE );
//    statusText.setText("Enter a Name for Elevation Model");
//    gridData = new GridData(GridData.HORIZONTAL_ALIGN_BEGINNING);
//    statusText.setLayoutData( gridData );
    

    final Label descriptionForFile = new Label( container, SWT.NONE );
    descriptionForFile.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_END | GridData.VERTICAL_ALIGN_BEGINNING ) );
    descriptionForFile.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.8" ) ); //$NON-NLS-1$
    
    descriptionForFileArea = new Text( container, SWT.BORDER | SWT.MULTI );
    GridData gridData2 = new GridData( GridData.HORIZONTAL_ALIGN_BEGINNING );
    gridData2.horizontalSpan = 2;
    gridData2.heightHint = 100;
    descriptionForFileArea.setLayoutData( gridData2 );
    descriptionForFileArea.setText( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.9" ) );
    initContents();    
  }

  /**
   * Called by the wizard to initialize the receiver's cached selection.
   * 
   * @param selection
   *          the selection or <code>null</code> if none
   */
  public void init( ISelection selection )
  {
    if( !(selection instanceof IStructuredSelection) )
      return;

    fileExtensions.add( "hmo" );
    fileExtensions.add( "asc" );
    // Find the first plugin.xml file.
    Iterator iter = ((IStructuredSelection) selection).iterator();
    while( iter.hasNext() )
    {
      Object item = (Object) iter.next();
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
      return;
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
  private void updatePageComplete( )
  {
    setPageComplete( false );

    IPath sourceLoc = getSourceLocation();
    if( sourceLoc == null || !(fileExtensions.contains( sourceLoc.getFileExtension() )) )
    {
      setMessage( null );
      setErrorMessage( Messages.getString( "org.kalypso.ui.wizards.imports.elevationModel.Elevation.3" ) );
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
    IPath path = browse( getSourceLocation(), false );
    if( path == null )
      return;
    IPath rootLoc = ResourcesPlugin.getWorkspace().getRoot().getLocation();
    if( rootLoc.isPrefixOf( path ) )
      path = path.setDevice( null ).removeFirstSegments( rootLoc.segmentCount() );
    sourceFileField.setText( path.toString() );

    // Multiple Elevation Model Select.. Still under development
    /*
     * IPath[] ar = null; IPath[] path = browse( getSourceLocation(), false ); if( path == null ) return; IPath rootLoc =
     * ResourcesPlugin.getWorkspace().getRoot().getLocation(); for (int i = 0; i<path.length;i++){ if(
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
  private IPath browse( IPath path, boolean mustExist )
  {
    FileDialog dialog = new FileDialog( getShell(), SWT.OPEN );
    dialog.setFilterExtensions( new String[] { "*.hmo", "*.asc" } );
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
    String text = sourceFileField.getText().trim();
    if( text.length() == 0 )
      return null;
    IPath path = new Path( text );
    if( !path.isAbsolute() )
      path = ResourcesPlugin.getWorkspace().getRoot().getLocation().append( path );
    return path;
  }

  public String getDescriptionForFileArea( )
  {
    return descriptionForFileArea.getText();
  }

  public String getNameForFile( )
  {
    return nameForFileText.getText();
  }
  
  public String getCoordinateSystem(){
    return coordinateSystem_Combo.getSelection().toString();
  }
}
