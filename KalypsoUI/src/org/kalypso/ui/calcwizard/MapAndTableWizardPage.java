package org.kalypso.ui.calcwizard;

import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;

import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.data.time.TimeSeriesCollection;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.widgets.ToggleSelectWidget;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.services.calcjob.CalcJobDescription;
import org.kalypso.services.calcjob.CalcJobStatus;
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.nature.ModelNature;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public class MapAndTableWizardPage extends AbstractCalcWizardPage implements ModellEventListener
{
  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  public final static String PROP_MAPTEMPLATE = "mapTemplate";

  /** Der Titel der Seite */
  public static final String PROP_MAPTITLE = "mapTitle";

  /** Pfad auf Vorlage für die Gis-Tabell (.gtt Datei) */
  public final static String PROP_TABLETEMPLATE = "tableTemplate";

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  public final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  public final static String PROP_RIGHTSASH = "rightSash";

  /**
   * Property-Names zum Layer in der Tabelle: alle Zeitreihen dieser Spalten
   * werden im diagram angezeigt
   */
  public final static String PROP_TIMEPROPNAME = "timeseriesPropertyNames";

  private static final int SELECTION_ID = 0x10;

  private final TimeSeriesCollection m_tsCol = new TimeSeriesCollection();

  private LayerTableViewer m_viewer;

  private IMapModell m_mapModell;

  private GM_Envelope m_boundingBox;

  private MapPanel m_mapPanel;

  public MapAndTableWizardPage()
  {
    super( "<MapAndTableWizardPage>" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    m_mapModell.removeModellListener( this );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );
      createMapPanel( sashForm );
      createRightPanel( sashForm );

      setControl( sashForm );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void createRightPanel( final SashForm sashForm ) throws NumberFormatException
  {
    final Composite rightPanel = new Composite( sashForm, SWT.NONE );

    final GridLayout gridLayout = new GridLayout();
    rightPanel.setLayout( gridLayout );
    rightPanel.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final SashForm rightSash = new SashForm( rightPanel, SWT.VERTICAL );
    rightSash.setLayoutData( new GridData( GridData.FILL_BOTH ) );
    createTablePanel( rightSash );
    createDiagramPanel( rightSash );

    final Button button = new Button( rightPanel, SWT.NONE | SWT.PUSH );
    button.setText( "Berechnung durchführen" );

    final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
    final int rightWeight = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH, "50" ) );

    sashForm.setWeights( new int[]
    { mainWeight, 100 - mainWeight } );

    rightSash.setWeights( new int[]
    { rightWeight, 100 - rightWeight } );

    // die Karte soll immer maximiert sein
    rightSash.addControlListener( new ControlAdapter()
    {
      public void controlResized( ControlEvent e )
      {
        maximizeMap();
      }

    } );

    button.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        runCalculation();
      }
    } );
  }

  private void createDiagramPanel( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );

    final Frame vFrame = SWT_AWT.new_Frame( composite );

    final JFreeChart chart = ChartFactory.createTimeSeriesChart( "", "Datum", "Wert", m_tsCol,
        false, false, false );

    final ChartPanel chartPanel = new ChartPanel( chart );
    chartPanel.setMouseZoomable( true, false );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );
  }

  private void createTablePanel( final Composite parent )
  {
    try
    {
      final String templateFileName = getArguments().getProperty( PROP_TABLETEMPLATE );
      final IFile templateFile = (IFile)getProject().findMember( templateFileName );
      final Gistableview template = GisTemplateHelper.loadGisTableview( templateFile,
          getReplaceProperties() );

      m_viewer = new LayerTableViewer( parent, getProject(), KalypsoGisPlugin.getDefault()
          .createFeatureTypeCellEditorFactory(), SELECTION_ID );
      m_viewer.applyTableTemplate( template, getProject() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final Text text = new Text( parent, SWT.NONE );
      text.setText( "Fehler beim Laden des TableTemplate" );
    }
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final ViewForm mapView = new ViewForm( parent, SWT.FLAT );

    ///////////////
    // MapModell //
    ///////////////
    final String mapFileName = getArguments().getProperty( PROP_MAPTEMPLATE );
    final IFile mapFile = (IFile)getProject().findMember( mapFileName );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile, getReplaceProperties() );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    m_mapModell = new GisTemplateMapModell( gisview, getProject(), crs );
    m_mapModell.addModellListener( this );

    m_mapPanel = new MapPanel( this, crs, SELECTION_ID );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    m_mapPanel.setMapModell( m_mapModell );
    m_mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );
    m_boundingBox = GisTemplateHelper.getBoundingBox( gisview );

    // das kann erst passieren, wenn die Control fertig ist: beim ersten rezise
    // event
    //m_mapPanel.setBoundingBox( m_boundingBox );
    m_mapPanel.changeWidget( new ToggleSelectWidget() );

    /////////////
    // Legende //
    /////////////
    final GisMapOutlineViewer outlineViewer = new GisMapOutlineViewer( this, m_mapModell );
    outlineViewer.createControl( mapView );

    mapView.setContent( mapComposite );
    mapView.setTopLeft( outlineViewer.getControl() );
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    // TODO: error handling?
    m_viewer.saveData();

    return true;
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
  // TODO: Marc will do it
  //    final String propNames = getArguments().getProperty( PROP_TIMEPROPNAME, ""
  // );
  //    final String[] timeNames = propNames.split( "#" );
  //
  //    final IKalypsoLayer layer = m_mapModell.getActiveTheme().getLayer();
  //    if( !( layer instanceof KalypsoFeatureLayer ) )
  //      return;
  //    
  //    final KalypsoFeatureLayer kfl = (KalypsoFeatureLayer)layer;
  //    final KalypsoFeature[] allFeatures = kfl.getAllFeatures();
  //    for( int i = 0; i < allFeatures.length; i++ )
  //    {
  //      if( allFeatures[i].isSelected( SELECTION_ID ) )
  //      {
  //        // do something
  //      }
  //    }
  }

  public void maximizeMap()
  {
    m_mapPanel.setBoundingBox( m_boundingBox );
  }

  protected void runCalculation()
  {
    m_viewer.saveData();
    
    try
    {
      getWizard().getContainer().run( false, true, new IRunnableWithProgress()
      {
        public void run( final IProgressMonitor monitor ) throws InvocationTargetException,
            InterruptedException
        {
          try
          {
            
            monitor.beginTask( "Prognoserechnung durchführen",  2000 );
            
            final ModelNature nature = (ModelNature)getCalcFolder().getProject().getNature( ModelNature.ID );
            final String jobID = nature.startCalculation( getCalcFolder(), new SubProgressMonitor( monitor, 100 ) );
            
            while( !monitor.isCanceled() )
            {
              // check if job is ready!
              final CalcJobDescription description = nature.checkCalculation( jobID );
              switch( description.getState() )
              {
                case CalcJobStatus.ERROR:
                  setErrorMessage( description.getMessage() );
                  return;
                  
                  case CalcJobStatus.FINISHED:
                    return;
              }
              
              Thread.sleep( 100 );
              monitor.worked( 10 );
            }
            
            if( monitor.isCanceled() )
            {
              nature.stopCalculation(jobID);
              throw new InterruptedException( "Abbruch durch Benutzer" );
            }
            // auf rechnung warten!
          }
          catch( final CoreException e )
          {
            e.printStackTrace();
            throw new InvocationTargetException( e, "Fehler bei der Berechnung" );
          }
        }
      } );
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();
      setErrorMessage( e.getLocalizedMessage() );
    }
    catch( final InterruptedException e )
    {
      e.printStackTrace();
      setErrorMessage( e.getLocalizedMessage() );
    }

  }
}