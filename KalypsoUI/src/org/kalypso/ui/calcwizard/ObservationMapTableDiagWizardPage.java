package org.kalypso.ui.calcwizard;

import java.awt.Frame;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.mapactions.FullExtentMapAction;
import org.kalypso.ogc.gml.mapactions.PanToWidgetAction;
import org.kalypso.ogc.gml.mapactions.SelectWidgetAction;
import org.kalypso.ogc.gml.mapactions.ToggleSelectWidgetAction;
import org.kalypso.ogc.gml.mapactions.UnselectWidgetAction;
import org.kalypso.ogc.gml.mapactions.ZoomInWidgetAction;
import org.kalypso.ogc.gml.mapactions.ZoomOutMapAction;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author schlienger
 */
public class ObservationMapTableDiagWizardPage extends AbstractCalcWizardPage implements
    ModellEventListener
{
  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  public final static String PROP_MAPTEMPLATE = "mapTemplate";

  /** Der Titel der Seite */
  public static final String PROP_MAPTITLE = "mapTitle";

  /** Pfad auf Vorlage für die Observation-Table (.ott Datei) */
  public final static String PROP_TABLETEMPLATE = "tableTemplate";

  /** Pfad auf die Vorlage für das Diagramm (.odt Datei) */
  public final static String PROP_DIAGTEMPLATE = "diagTemplate";

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  public final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  public final static String PROP_RIGHTSASH = "rightSash";

  /**
   * Basisname der Zeitreihen-Properties. Es kann mehrere Zeitreihen
   * geben-Property geben: eine für jede Kurventyp.
   */
  public final static String PROP_TIMEPROPNAME = "timeserie";

  private static final int SELECTION_ID = 0x10;

  private IMapModell m_mapModell;

  public ObservationMapTableDiagWizardPage()
  {
    super( "<ObservationMapTableDiagWizardPage>" );
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
      // TODO handling
      throw new RuntimeException( e );
    }
  }

  private void createDiagramPanel( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );

    final Frame vFrame = SWT_AWT.new_Frame( composite );

    //    final JFreeChart chart = ChartFactory.createTimeSeriesChart( "", "Datum",
    // "Wert", m_tsCol,
    //        false, false, false );
    //
    //    final ChartPanel chartPanel = new ChartPanel( chart );
    //    chartPanel.setMouseZoomable( true, false );
    //
    //    vFrame.setVisible( true );
    //    chartPanel.setVisible( true );
    //    vFrame.add( chartPanel );
  }

  private void createTablePanel( final Composite parent )
  {
    try
    {
      final String templateFileName = getArguments().getProperty( PROP_TABLETEMPLATE );
      final IFile templateFile = (IFile)getProject().findMember( templateFileName );

      //      final Gistableview template = GisTemplateHelper.loadGisTableview(
      // templateFile );

      //      m_viewer = new LayerTableViewer( parent, getProject(),
      // KalypsoGisPlugin.getDefault()
      //          .createFeatureTypeCellEditorFactory(), SELECTION_ID );
      //      m_viewer.applyTableTemplate( template, getProject() );
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

    final String mapFileName = getArguments().getProperty( PROP_MAPTEMPLATE );
    final IFile mapFile = (IFile)getProject().findMember( mapFileName );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    m_mapModell = new GisTemplateMapModell( gisview, getProject(), crs );
    m_mapModell.addModellListener( this );

    final MapPanel mapPanel = new MapPanel( this, crs, SELECTION_ID );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    mapPanel.setMapModell( m_mapModell );
    mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    final ToolBarManager tbm = new ToolBarManager();
    tbm.add( new GroupMarker( "radio_group" ) );
    tbm.appendToGroup( "radio_group", new ZoomInWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new PanToWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new ToggleSelectWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new SelectWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new UnselectWidgetAction( mapPanel ) );

    tbm.add( new Separator() );

    tbm.add( new FullExtentMapAction( this, mapPanel ) );
    tbm.add( new ZoomOutMapAction( this, mapPanel ) );

    final ToolBar toolBar = tbm.createControl( mapView );

    /////////////
    // Legende //
    /////////////
    final GisMapOutlineViewer outlineViewer = new GisMapOutlineViewer( this, m_mapModell );
    outlineViewer.createControl( mapView );

    mapView.setContent( mapComposite );
    mapView.setTopCenter( toolBar );
    mapView.setTopLeft( outlineViewer.getControl() );
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    return true;
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    final TimeserieFeatureProps[] tsProps = KalypsoWizardHelper
        .parseTimeserieFeactureProps( getArguments() );

    final IKalypsoLayer layer = m_mapModell.getActiveTheme().getLayer();
    if( !( layer instanceof KalypsoFeatureLayer ) )
      return;

    List selectedFeatures = new ArrayList();

    final KalypsoFeatureLayer kfl = (KalypsoFeatureLayer)layer;
    final KalypsoFeature[] allFeatures = kfl.getAllFeatures();
    for( int i = 0; i < allFeatures.length; i++ )
      if( allFeatures[i].isSelected( SELECTION_ID ) )
        selectedFeatures.add( allFeatures[i] );

    final String diagFileName = getArguments().getProperty( PROP_DIAGTEMPLATE );
    final IFile diagFile = (IFile)getProject().findMember( diagFileName );

    // create the diagram template
    KalypsoWizardHelper.createDiagramTemplate( tsProps, selectedFeatures, diagFile );

    // TODO: create chart

    // TODO: update observation table
  }
}