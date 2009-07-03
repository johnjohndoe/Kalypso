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
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.kalypso.core.preferences.IKalypsoCorePreferences;
import org.kalypso.risk.model.utils.RiskModelHelper;
import org.kalypso.risk.plugin.KalypsoRiskPlugin;
import org.kalypso.transformation.CRSHelper;
import org.kalypso.ui.ImageProvider;

/**
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 */
public class ImportWaterdepthPage extends WizardPage
{
  private Table m_tableViewer;

  private final List<AsciiRasterInfo> m_rasterInfos;

  private Button m_btnAddNew;

  private Button m_btnDeleteSelected;

  private int m_selectedRasterIndex = -1;

  private Combo m_cmbCoordinateSystem;

  private Spinner m_fldReturnPeriod;

  private Text m_fldOffset;

  private Text m_fldRasterSize;

  private Text m_fldCellSize;

  public ImportWaterdepthPage( )
  {
    super( Messages.getString( "ImportWaterdepthPage.0" ), "", ImageProvider.IMAGE_NEW_FILE ); //$NON-NLS-1$ //$NON-NLS-2$
    setTitle( Messages.getString( "ImportWaterdepthPage.2" ) ); //$NON-NLS-1$
    setDescription( Messages.getString( "ImportWaterdepthPage.3" ) ); //$NON-NLS-1$
    m_rasterInfos = new ArrayList<AsciiRasterInfo>();
  }

  public void createControl( final Composite parent )
  {
    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
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

    final GridData tableCompositeGridData = new GridData();
    tableCompositeGridData.horizontalAlignment = GridData.FILL;
    tableCompositeGridData.grabExcessHorizontalSpace = true;
    tableCompositeGridData.verticalAlignment = GridData.FILL;
    tableCompositeGridData.grabExcessVerticalSpace = true;
    tableComposite.setLayout( subCompositeLayout );
    tableComposite.setLayoutData( tableCompositeGridData );

    buttonsComposite.setLayout( subCompositeLayout );
    buttonsComposite.setLayoutData( new GridData( GridData.BEGINNING | GridData.VERTICAL_ALIGN_BEGINNING ) );

    final GridData infoCompositeGridData = new GridData();
    infoCompositeGridData.horizontalAlignment = GridData.FILL;
    infoCompositeGridData.grabExcessHorizontalSpace = true;
    infoCompositeGridData.verticalAlignment = GridData.BEGINNING;
    infoCompositeGridData.grabExcessVerticalSpace = false;
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
    group.setText( Messages.getString( "ImportWaterdepthPage.12" ) ); //$NON-NLS-1$

    createControlInfoPart( group );

    m_btnAddNew.setFocus();
    setControl( mainComposite );
  }

  private void createControlTablePart( final Composite parent )
  {
    m_tableViewer = new Table( parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION );
    final GridData gridData = new GridData( GridData.FILL_BOTH );
    gridData.grabExcessHorizontalSpace = true;
    gridData.grabExcessVerticalSpace = true;
    m_tableViewer.setLayoutData( gridData );

    final TableColumn fileColumn = new TableColumn( m_tableViewer, SWT.LEFT );
    fileColumn.setText( Messages.getString( "ImportWaterdepthPage.13" ) ); //$NON-NLS-1$
    fileColumn.setWidth( 200 );

    final TableColumn annualityColumn = new TableColumn( m_tableViewer, SWT.LEFT );
    annualityColumn.setText( Messages.getString( "ImportWaterdepthPage.14" ) ); //$NON-NLS-1$
    annualityColumn.setWidth( 50 );

    final TableColumn csColumn = new TableColumn( m_tableViewer, SWT.LEFT );
    csColumn.setText( Messages.getString( "ImportWaterdepthPage.15" ) ); //$NON-NLS-1$
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
      public void widgetSelected( SelectionEvent e )
      {
        int[] selection = m_tableViewer.getSelectionIndices();
        m_selectedRasterIndex = new Integer( selection[0] ).intValue();
        updateComposite();
      }
    } );
  }

  private void createControlButtonsPart( final Composite parent )
  {
    final ImageDescriptor addBtnID = KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/raster_add.gif" );
    final ImageDescriptor removeBtnID = KalypsoRiskPlugin.getImageProvider().getImageDescriptor( "icons/etool16/raster_delete.gif" );

    m_btnAddNew = new Button( parent, SWT.PUSH );
    m_btnAddNew.setImage( new Image( parent.getDisplay(), addBtnID.getImageData() ) );
    m_btnAddNew.setToolTipText( Messages.getString( "ImportWaterdepthPage.11" ) ); //$NON-NLS-1$
    m_btnAddNew.addSelectionListener( new SelectionListener()
    {
      public void widgetDefaultSelected( final SelectionEvent e )
      {
      }

      public void widgetSelected( final SelectionEvent e )
      {
        final List<String> rasterFiles = getFilenamesFromDialog();
        for( final String rasterFile : rasterFiles )
        {
          try
          {
            final AsciiRasterInfo rasterInfo = new AsciiRasterInfo( rasterFile );
            rasterInfo.setReturnPeriod( RiskModelHelper.guessReturnPeriodFromName( rasterFile ) );
            m_rasterInfos.add( rasterInfo );
            final TableItem tableItem = new TableItem( m_tableViewer, 0 );
            tableItem.setText( rasterInfo.getDisplayDetails() );
            m_selectedRasterIndex = 0;
            updateComposite();
            getWizard().getContainer().updateButtons();
          }
          catch( Exception e1 )
          {
            // TODO Auto-generated catch block
            e1.printStackTrace();
          }
        }
      }
    } );
    m_btnDeleteSelected = new Button( parent, SWT.PUSH );
    m_btnDeleteSelected.setImage( new Image( parent.getDisplay(), removeBtnID.getImageData() ) );
    m_btnDeleteSelected.setToolTipText( Messages.getString( "ImportWaterdepthPage.16" ) ); //$NON-NLS-1$
    m_btnDeleteSelected.setEnabled( false );
    m_btnDeleteSelected.addSelectionListener( new SelectionListener()
    {
      public void widgetDefaultSelected( final SelectionEvent e )
      {
      }

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
    lbl1.setText( Messages.getString( "ImportWaterdepthPage.17" ) ); //$NON-NLS-1$
    m_fldRasterSize = new Text( parent, SWT.BORDER );
    m_fldRasterSize.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fldRasterSize.setEditable( false );
    m_fldRasterSize.setText( "" ); //$NON-NLS-1$

    final Label lbl2 = new Label( parent, SWT.NONE );
    lbl2.setText( Messages.getString( "ImportWaterdepthPage.19" ) ); //$NON-NLS-1$
    m_fldOffset = new Text( parent, SWT.BORDER );
    m_fldOffset.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    m_fldOffset.setEditable( false );
    m_fldOffset.setText( "" ); //$NON-NLS-1$

    final Label lbl3 = new Label( parent, SWT.NONE );
    lbl3.setText( Messages.getString( "ImportWaterdepthPage.21" ) ); //$NON-NLS-1$
    m_fldCellSize = new Text( parent, SWT.BORDER );
    m_fldCellSize.setEditable( false );
    m_fldCellSize.setText( "" ); //$NON-NLS-1$

    final Label lbl4 = new Label( parent, SWT.NONE );
    lbl4.setText( Messages.getString( "ImportWaterdepthPage.23" ) ); //$NON-NLS-1$
    m_cmbCoordinateSystem = new Combo( parent, SWT.NONE );
    final String[] allCoordinateSystems = CRSHelper.getAllNames().toArray( new String[] {} );
    m_cmbCoordinateSystem.setItems( allCoordinateSystems );
    m_cmbCoordinateSystem.select( m_cmbCoordinateSystem.indexOf( IKalypsoCorePreferences.DEFAULT_CRS ) );
    m_cmbCoordinateSystem.setEnabled( false );
    m_cmbCoordinateSystem.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        if( m_rasterInfos.get( m_selectedRasterIndex ).setCoordinateSystem( m_cmbCoordinateSystem.getText() ) )
        {
          m_tableViewer.getItem( m_selectedRasterIndex ).setText( 2, m_cmbCoordinateSystem.getText() );
          m_tableViewer.redraw();
        }
        else
        {
          MessageDialog.openError( parent.getShell(), Messages.getString( "ImportWaterdepthPage.25" ), Messages.getString( "ImportWaterdepthPage.26" ) ); //$NON-NLS-1$ //$NON-NLS-2$
        }
      }
    } );

    final Label lbl5 = new Label( parent, SWT.NONE );
    lbl5.setText( Messages.getString( "ImportWaterdepthPage.27" ) ); //$NON-NLS-1$
    m_fldReturnPeriod = new Spinner( parent, SWT.BORDER );
    m_fldReturnPeriod.setValues( 1, 1, 1000, 0, 1, 50 );
    m_fldReturnPeriod.setEnabled( false );
    m_fldReturnPeriod.addSelectionListener( new SelectionAdapter()
    {
      @SuppressWarnings("synthetic-access")//$NON-NLS-1$
      @Override
      public void widgetSelected( SelectionEvent e )
      {
        m_rasterInfos.get( m_selectedRasterIndex ).setReturnPeriod( m_fldReturnPeriod.getSelection() );
        m_tableViewer.getItem( m_selectedRasterIndex ).setText( 1, Integer.toString( m_fldReturnPeriod.getSelection() ) );
        m_tableViewer.redraw();
        getWizard().getContainer().updateButtons();
      }
    } );
  }

  private void updateComposite( )
  {
    if( m_selectedRasterIndex < 0 || m_selectedRasterIndex >= m_rasterInfos.size() )
    {
      m_fldRasterSize.setText( "" ); //$NON-NLS-1$
      m_fldOffset.setText( "" ); //$NON-NLS-1$
      m_fldCellSize.setText( "" ); //$NON-NLS-1$
      m_cmbCoordinateSystem.setEnabled( false );
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
      m_cmbCoordinateSystem.select( m_cmbCoordinateSystem.indexOf( rasterInfo.getCoordinateSystem() ) );
      m_fldReturnPeriod.setSelection( rasterInfo.getReturnPeriod() );
      m_cmbCoordinateSystem.setEnabled( true );
      m_fldReturnPeriod.setEnabled( true );
      m_btnDeleteSelected.setEnabled( true );
    }
    m_tableViewer.redraw();
  }

  private List<String> getFilenamesFromDialog( )
  {
    final FileDialog dialog = new FileDialog( getShell(), SWT.MULTI );
    dialog.setFilterExtensions( new String[] { "*.asc" } ); //$NON-NLS-1$
    dialog.setFilterNames( new String[] { "ASCII grid (*.asc)" } ); //$NON-NLS-1$
    dialog.open();
    final String[] fileNames = dialog.getFileNames();
    final String filterPath = dialog.getFilterPath();
    final List<String> resultList = new ArrayList<String>();
    for( int i = 0; i < fileNames.length; i++ )
      if( fileNames[i] != null && fileNames[i].length() != 0 )
        resultList.add( filterPath + File.separator + fileNames[i] );
    return resultList;
  }

  @Override
  public boolean isPageComplete( )
  {
    if( m_rasterInfos == null || m_rasterInfos.size() == 0 )
      return false;
    final List<Integer> returnPeriodsList = new ArrayList<Integer>();
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
}
