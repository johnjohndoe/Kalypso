package org.kalypso.ui.wizards.imports.roughness;

import java.io.File;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.wizard.IWizardPage;
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
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;
import org.kalypso.ui.wizards.i18n.Messages;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree_impl.io.shpapi.ShapeFile;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class PageMain extends WizardPage implements Listener
{
  private final DataContainer m_data;

  // widgets on this page
  Combo cmb_ShapeProperty;

  Text txt_InputFile;

  Button btn_inputFileBrowse;

  // Text m_layerName;

  Button m_layerTypeCorrection;

  Button m_layerTypeBasic;

  // status variable for the possible errors on this page
  IStatus msg_StatusLine;

  private CRSSelectionPanel m_crsPanel;

  protected String m_crs;

  /**
   * Constructor for main page
   */
  public PageMain( DataContainer data )
  {
    this( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.0" ), data );//$NON-NLS-1$
  }

  protected PageMain( final String name, DataContainer data )
  {
    super( name );
    setTitle( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.1" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.2" ) ); //$NON-NLS-1$
    msg_StatusLine = new Status( IStatus.OK, "not_used", 0, "", null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_data = data;
  }

  @Override
  public void createControl( Composite parent )
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
    new Label( composite, SWT.NONE ).setText( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.5" ) ); //$NON-NLS-1$
    txt_InputFile = new Text( composite, SWT.BORDER );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    txt_InputFile.setLayoutData( gd );
    btn_inputFileBrowse = new Button( composite, SWT.PUSH );
    btn_inputFileBrowse.setText( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.6" ) ); //$NON-NLS-1$
    gd = new GridData( GridData.END );
    // gd.horizontalSpan = ncol;
    btn_inputFileBrowse.setLayoutData( gd );
    // createLine(composite, ncol);

    // Roughness parameter combo box
    new Label( composite, SWT.NONE ).setText( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.7" ) ); //$NON-NLS-1$
    cmb_ShapeProperty = new Combo( composite, SWT.BORDER | SWT.READ_ONLY );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.widthHint = 75;
    cmb_ShapeProperty.setEnabled( false );
    cmb_ShapeProperty.setLayoutData( gd );

    createLine( composite, ncol );

    /* Coordinate system combo */
    final Composite crsContainer = new Composite( composite, SWT.NULL );
    final GridLayout crsGridLayout = new GridLayout();
    crsGridLayout.numColumns = 1;
    crsContainer.setLayout( crsGridLayout );

    final GridData crsGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    crsGridData.horizontalSpan = 3;
    crsContainer.setLayoutData( crsGridData );

    m_crsPanel = new CRSSelectionPanel( crsContainer, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    m_crsPanel.setToolTipText( Messages.getString("org.kalypso.ui.wizards.imports.roughness.PageMain.15") ); //$NON-NLS-1$

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_crsPanel.setSelectedCRS( m_crs );
    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      @Override
      protected void selectionChanged( String selectedCRS )
      {
        m_crs = selectedCRS;
        // updatePageComplete();
      }

    } );

    createLine( composite, ncol );

    new Label( composite, SWT.NONE ).setText( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.12" ) ); //$NON-NLS-1$
    m_layerTypeCorrection = new Button( composite, SWT.RADIO );
    m_layerTypeCorrection.setText( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.14" ) ); //$NON-NLS-1$
    m_layerTypeCorrection.setSelection( false );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    m_layerTypeCorrection.setLayoutData( gd );
    new Label( composite, SWT.NONE ).setText( "" ); //$NON-NLS-1$

    new Label( composite, SWT.NONE ).setText( "" ); //$NON-NLS-1$
    m_layerTypeBasic = new Button( composite, SWT.RADIO );
    m_layerTypeBasic.setText( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.13" ) ); //$NON-NLS-1$
    m_layerTypeBasic.setSelection( true );
    gd = new GridData();
    gd.horizontalAlignment = GridData.FILL;
    gd.grabExcessHorizontalSpace = true;
    // gd.horizontalSpan = ncol - 1;
    m_layerTypeBasic.setLayoutData( gd );

    // set the composite as the control for this page

    btn_inputFileBrowse.setFocus();
    setControl( composite );
    addListeners();
  }

  private void addListeners( )
  {
    btn_inputFileBrowse.addListener( SWT.MouseDown, this );
    btn_inputFileBrowse.addListener( SWT.KeyDown, this );
    txt_InputFile.addListener( SWT.Modify, this );
    cmb_ShapeProperty.addListener( SWT.Selection, this );
    // m_layerName.addListener( SWT.Modify, this );
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
        StringBuffer sb = new StringBuffer( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.9" ) ); //$NON-NLS-1$
        sb.append( txt_InputFile.getText().substring( txt_InputFile.getText().lastIndexOf( File.separator ) + 1 ) );
        sb.append( "\n" ).append( Messages.getString( "org.kalypso.ui.wizards.imports.roughness.PageMain.10" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        status = new Status( IStatus.ERROR, "not_used", 0, sb.toString(), null ); //$NON-NLS-1$
        msg_StatusLine = status;
        e.printStackTrace();
      }
    }
    // Show the most serious error
    applyToStatusLine( msg_StatusLine );
    getWizard().getContainer().updateButtons();
  }

  private String getFilenameFromDialog( File selectedFile, String[] filterExtensions, String path )
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
    return cmb_ShapeProperty.isEnabled();// && isTextNonEmpty( m_layerName );
  }

  @Override
  public IWizardPage getNextPage( )
  {
    saveDataToModel();
    return super.getNextPage();
  }

  protected void saveDataToModel( )
  {
    if( isCurrentPage() )
    {
      m_data.setInputFile( txt_InputFile.getText() );
      m_data.setShapeProperty( cmb_ShapeProperty.getText() );
      m_data.setCoordinateSystem( m_crs );
      // m_data.setLayerName( m_layerName.getText() );
      // m_data.setLayerEditable( m_layerType.getSelection() );
      m_data.setLayerAsBasic( m_layerTypeBasic.getSelection() );

      // m_data.getRoughnessPolygonCollection().clear();
      // m_data.getRoughnessShapeStaticRelationMap().clear();
      // m_data.getRoughnessStaticCollectionMap().clear();
      try
      {
        ((Transformer) ((ImportWizard) getWizard()).m_operation).unprepare();
        ((Transformer) ((ImportWizard) getWizard()).m_operation).prepare( true );
        ((ImportWizard) getWizard()).m_pageSecond.delayedCreateControl();
      }
      catch( Exception e )
      {
        e.printStackTrace();
        ((ImportWizard) getWizard()).performCancel();
      }
    }
  }

}
