package org.kalypso.risk.model.actions.dataImport.waterdepth;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.kalypso.risk.i18n.Messages;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.transformation.ui.CRSSelectionPanel;
import org.kalypso.transformation.ui.listener.CRSSelectionListener;
import org.kalypso.ui.ImageProvider;
import org.kalypsodeegree.KalypsoDeegreePlugin;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWaterdepthPage extends WizardPage
{
  protected Table m_tableViewer;

  protected String m_crs;

  protected final List<AsciiRasterInfo> m_rasterInfos = new ArrayList<>();

  private Button m_btnAddNew;

  private Button m_btnDeleteSelected;

  protected int m_selectedRasterIndex = -1;

  private Spinner m_fldReturnPeriod;

  private Text m_fldOffset;

  private Text m_fldRasterSize;

  private Text m_fldCellSize;

  private CRSSelectionPanel m_crsPanel;

  private final List<Image> m_imageList = new ArrayList<>();

  public ImportWaterdepthPage( )
  {
    super( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.0" ), "", ImageProvider.IMAGE_NEW_FILE ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.3" ) ); //$NON-NLS-1$

  }

  @Override
  public void createControl( final Composite parent )
  {
    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData)layoutData;
      pLayout.widthHint = 410;
      pLayout.heightHint = 400;
      parent.layout();
    }
    final Composite mainComposite = new Composite( parent, SWT.NONE );
    mainComposite.setSize( 400, 500 );
    final GridLayout mainCompositeLayout = new GridLayout();
    mainCompositeLayout.numColumns = 2;
    final GridLayout subCompositeLayout = new GridLayout();
    subCompositeLayout.numColumns = 1;
    mainComposite.setLayout( mainCompositeLayout );

    final Composite tableComposite = new Composite( mainComposite, SWT.NULL );
    final Composite buttonsComposite = new Composite( mainComposite, SWT.NULL );
    final Composite infoComposite = new Composite( mainComposite, SWT.NULL );

    final GridData tableCompositeGridData = new GridData( SWT.FILL, SWT.FILL, true, true );
    tableComposite.setLayout( subCompositeLayout );
    tableComposite.setLayoutData( tableCompositeGridData );

    buttonsComposite.setLayout( subCompositeLayout );
    buttonsComposite.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final GridData infoCompositeGridData = new GridData( SWT.FILL, SWT.FILL, true, false );
    infoCompositeGridData.horizontalSpan = 1;
    infoComposite.setLayout( subCompositeLayout );
    infoComposite.setLayoutData( infoCompositeGridData );

    createControlTablePart( tableComposite );
    createControlButtonsPart( buttonsComposite );

    final GridLayout groupLayout = new GridLayout();
    groupLayout.numColumns = 2;
    groupLayout.marginHeight = 2;
    groupLayout.marginWidth = 2;
    groupLayout.horizontalSpacing = 3;

    final Group group = new Group( infoComposite, SWT.NONE );
    group.setLayout( groupLayout );
    group.setLayoutData( infoCompositeGridData );
    group.layout();
    group.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.12" ) ); //$NON-NLS-1$

    createControlInfoPart( group );

    m_btnAddNew.setFocus();
    setControl( mainComposite );

    updateComposite();
  }

  private void createControlTablePart( final Composite parent )
  {
    m_tableViewer = new Table( parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION );
    m_tableViewer.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );

    final TableColumn fileColumn = new TableColumn( m_tableViewer, SWT.LEFT );
    fileColumn.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.13" ) ); //$NON-NLS-1$
    fileColumn.setWidth( 200 );

    final TableColumn annualityColumn = new TableColumn( m_tableViewer, SWT.LEFT );
    annualityColumn.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.14" ) ); //$NON-NLS-1$
    annualityColumn.setWidth( 50 );

    final TableColumn csColumn = new TableColumn( m_tableViewer, SWT.LEFT );
    csColumn.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.15" ) ); //$NON-NLS-1$
    csColumn.setWidth( 80 );

    m_tableViewer.setSize( 300, 250 );
    /*
     * TableColumn hqColumn = new TableColumn( waterlevelTable, SWT.CENTER ); hqColumn.setText( "HQ" );
     * hqColumn.setWidth( 100 );
     */
    m_tableViewer.setHeaderVisible( true );
    m_tableViewer.setLinesVisible( true );
    m_tableViewer.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final int[] selection = m_tableViewer.getSelectionIndices();
        m_selectedRasterIndex = new Integer( selection[0] ).intValue();
        updateComposite();
      }
    } );
  }

  private void createControlButtonsPart( final Composite parent )
  {
    final ImageDescriptor addBtnID = KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/raster_add.gif" ); //$NON-NLS-1$
    final ImageDescriptor removeBtnID = KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/raster_delete.gif" ); //$NON-NLS-1$

    m_btnAddNew = new Button( parent, SWT.PUSH );
    final Image addButtonImage = new Image( parent.getDisplay(), addBtnID.getImageData() );
    m_imageList.add( addButtonImage );

    m_btnAddNew.setImage( addButtonImage );
    m_btnAddNew.setToolTipText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.11" ) ); //$NON-NLS-1$
    m_btnAddNew.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        final List<String> rasterFiles = getFilenamesFromDialog();

        for( int i = 0; i < rasterFiles.size(); i++ )
        {
          try
          {
            final String rasterFile = rasterFiles.get( i );
            final AsciiRasterInfo rasterInfo = new AsciiRasterInfo( rasterFile );
            rasterInfo.setReturnPeriod( RiskModelHelper.guessReturnPeriodFromName( rasterFile ) );
            m_rasterInfos.add( rasterInfo );
            final TableItem tableItem = new TableItem( m_tableViewer, 0 );
            tableItem.setText( rasterInfo.getDisplayDetails() );
            m_selectedRasterIndex = 0;
            updateComposite();
            getWizard().getContainer().updateButtons();
          }
          catch( final Exception e1 )
          {
            // TODO Auto-generated catch block
            e1.printStackTrace();
          }
        }
      }
    } );
    m_btnDeleteSelected = new Button( parent, SWT.PUSH );

    final Image deleteButtonImage = new Image( parent.getDisplay(), removeBtnID.getImageData() );
    m_imageList.add( deleteButtonImage );

    m_btnDeleteSelected.setImage( deleteButtonImage );
    m_btnDeleteSelected.setToolTipText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.16" ) ); //$NON-NLS-1$
    m_btnDeleteSelected.setEnabled( false );
    m_btnDeleteSelected.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_rasterInfos.remove( m_selectedRasterIndex );
        m_tableViewer.remove( m_selectedRasterIndex );
        m_selectedRasterIndex--;
        updateComposite();
        getWizard().getContainer().updateButtons();
      }
    } );
  }

  private void createControlInfoPart( final Composite parent )
  {

    final Label lbl1 = new Label( parent, SWT.NONE );
    lbl1.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.17" ) ); //$NON-NLS-1$
    m_fldRasterSize = new Text( parent, SWT.BORDER );
    m_fldRasterSize.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    m_fldRasterSize.setEditable( false );
    m_fldRasterSize.setText( "" ); //$NON-NLS-1$

    final Label lbl2 = new Label( parent, SWT.NONE );
    lbl2.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.19" ) ); //$NON-NLS-1$
    m_fldOffset = new Text( parent, SWT.BORDER );
    m_fldOffset.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    m_fldOffset.setEditable( false );
    m_fldOffset.setText( "" ); //$NON-NLS-1$

    final Label lbl3 = new Label( parent, SWT.NONE );
    lbl3.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.21" ) ); //$NON-NLS-1$
    m_fldCellSize = new Text( parent, SWT.BORDER );
    m_fldCellSize.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    m_fldCellSize.setEditable( false );
    m_fldCellSize.setText( "" ); //$NON-NLS-1$

    final Label lbl5 = new Label( parent, SWT.NONE );
    lbl5.setText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.27" ) ); //$NON-NLS-1$
    m_fldReturnPeriod = new Spinner( parent, SWT.BORDER );
    m_fldReturnPeriod.setLayout( new GridLayout() );
    m_fldReturnPeriod.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    m_fldReturnPeriod.setValues( 1, 1, 1000, 0, 1, 50 );
    m_fldReturnPeriod.setEnabled( false );

    final Spinner fldReturnPeriod = m_fldReturnPeriod;
    m_fldReturnPeriod.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_rasterInfos.get( m_selectedRasterIndex ).setReturnPeriod( fldReturnPeriod.getSelection() );
        m_tableViewer.getItem( m_selectedRasterIndex ).setText( 1, Integer.toString( fldReturnPeriod.getSelection() ) );
        m_tableViewer.redraw();
        getWizard().getContainer().updateButtons();
      }
    } );

    /* Coordinate system combo */
    final Composite crsContainer = new Composite( parent, SWT.NULL );
    final GridLayout crsGridLayout = new GridLayout();
    crsGridLayout.numColumns = 1;
    crsContainer.setLayout( crsGridLayout );

    final GridData crsGridData = new GridData( SWT.FILL, SWT.FILL, true, false );
    crsGridData.horizontalSpan = 2;
    crsContainer.setLayoutData( crsGridData );

    m_crsPanel = new CRSSelectionPanel( crsContainer, SWT.NONE );
    m_crsPanel.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );

    m_crsPanel.setToolTipText( Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.30" ) ); //$NON-NLS-1$

    m_crs = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
    m_crsPanel.setSelectedCRS( m_crs );
    m_crsPanel.addSelectionChangedListener( new CRSSelectionListener()
    {
      @Override
      protected void selectionChanged( final String selectedCRS )
      {
        m_crs = selectedCRS;
        if( m_selectedRasterIndex >= 0 && m_selectedRasterIndex < m_rasterInfos.size() )
        {
          if( m_rasterInfos.get( m_selectedRasterIndex ).setCoordinateSystem( m_crs ) )
          {
            m_tableViewer.getItem( m_selectedRasterIndex ).setText( 2, m_crs );
            m_tableViewer.redraw();
          }
          else
          {
            MessageDialog.openError( parent.getShell(), Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.25" ), Messages.getString( "org.kalypso.risk.model.actions.dataImport.waterdepth.ImportWaterdepthPage.26" ) ); //$NON-NLS-1$ //$NON-NLS-2$
          }
        }
      }
    } );

  }

  protected void updateComposite( )
  {
    if( m_selectedRasterIndex < 0 || m_selectedRasterIndex >= m_rasterInfos.size() )
    {
      m_fldRasterSize.setText( "" ); //$NON-NLS-1$
      m_fldOffset.setText( "" ); //$NON-NLS-1$
      m_fldCellSize.setText( "" ); //$NON-NLS-1$
      m_crsPanel.setEnabled( false );
      m_fldReturnPeriod.setEnabled( false );
      m_btnDeleteSelected.setEnabled( false );
    }
    else
    {
      m_tableViewer.setSelection( m_selectedRasterIndex );
      final AsciiRasterInfo rasterInfo = m_rasterInfos.get( m_selectedRasterIndex );
      m_fldRasterSize.setText( rasterInfo.getRasterSizeX() + " x " + rasterInfo.getRasterSizeY() ); //$NON-NLS-1$
      m_fldOffset.setText( rasterInfo.getOffsetX() + ", " + rasterInfo.getOffsetY() ); //$NON-NLS-1$
      m_fldCellSize.setText( rasterInfo.getCellSize() + "" ); //$NON-NLS-1$

      m_crsPanel.setSelectedCRS( rasterInfo.getCoordinateSystem() );
      m_fldReturnPeriod.setSelection( rasterInfo.getReturnPeriod() );
      m_fldReturnPeriod.setEnabled( true );
      m_btnDeleteSelected.setEnabled( true );
      m_crsPanel.setEnabled( true );
    }
    m_tableViewer.redraw();
  }

  protected List<String> getFilenamesFromDialog( )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.MULTI );
    dialog.setFilterExtensions( new String[] { "*.asc" } ); //$NON-NLS-1$
    dialog.setFilterNames( new String[] { "ASCII grid (*.asc)" } ); //$NON-NLS-1$
    dialog.open();
    final String[] fileNames = dialog.getFileNames();
    final String filterPath = dialog.getFilterPath();

    final List<String> resultList = new ArrayList<>();
    for( final String fileName : fileNames )
    {
      if( fileName != null && fileName.length() != 0 )
        resultList.add( filterPath + File.separator + fileName );
    }

    return resultList;
  }

  @Override
  public boolean isPageComplete( )
  {
    if( m_rasterInfos == null || m_rasterInfos.size() == 0 )
      return false;

    final List<Integer> returnPeriodsList = new ArrayList<>();
    for( final AsciiRasterInfo rasterInfo : m_rasterInfos )
    {
      final Integer returnPeriod = rasterInfo.getReturnPeriod();
      if( returnPeriod > 0 )
      {
        if( returnPeriodsList.contains( returnPeriod ) )
          return false;
        else
          returnPeriodsList.add( returnPeriod );
      }
      else
        return false;
    }
    return true;
  }

  public List<AsciiRasterInfo> getRasterInfos( )
  {
    return m_rasterInfos;
  }

  @Override
  public void dispose( )
  {
    // dispose all images and colors
    for( final Image image : m_imageList )
      image.dispose();

    m_crsPanel.dispose();
  }
}