package org.kalypso.ui.calcwizard;

import java.awt.Frame;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
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
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
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
      final SashForm rightSash = new SashForm( sashForm, SWT.VERTICAL );
      createTablePanel( rightSash );
      createDiagramPanel( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
      final int rightWeight = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH, "50" ) );

      // TODO: konfigure
      sashForm.setWeights( new int[]
      { mainWeight, 100 - mainWeight } );

      rightSash.setWeights( new int[]
      { rightWeight, 100 - rightWeight } );

      setControl( sashForm );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
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
      final Gistableview template = GisTemplateHelper.loadGisTableview( templateFile );

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

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    m_mapModell = new GisTemplateMapModell( gisview, getProject(), crs );
    m_mapModell.addModellListener( this );

    ///////////
    // Karte //
    ///////////
    final MapPanel mapPanel = new MapPanel( this, crs, SELECTION_ID );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    mapPanel.setMapModell( m_mapModell );
    mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    mapPanel.changeWidget( new ToggleSelectWidget() );
    
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
//    final String propNames = getArguments().getProperty(  PROP_TIMEPROPNAME, "" );
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
}