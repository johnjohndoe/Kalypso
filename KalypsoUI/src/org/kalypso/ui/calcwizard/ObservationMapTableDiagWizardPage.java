package org.kalypso.ui.calcwizard;

import java.awt.Frame;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.sensor.deegree.TimeserieFeatureProps;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.tableview.swing.renderer.DateTableCellRenderer;
import org.kalypso.ogc.sensor.tableview.swing.renderer.MaskedNumberTableCellRenderer;
import org.kalypso.ogc.sensor.template.LinkedTableViewTemplate;
import org.kalypso.ogc.widgets.ToggleSelectWidget;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.ObservationTemplateHelper;
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

  private IMapModell m_mapModell = null;

  private Frame m_diagFrame = null;

  private Frame m_tableFrame = null;

  private SashForm m_sashForm = null;

  private IDiagramTemplate m_diagTemplate = null;

  private ObservationChart m_obsChart = null;

  private final ObservationTableModel m_tableModel = new ObservationTableModel();

  private LinkedTableViewTemplate m_tableTemplate = null;

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

    if( m_diagTemplate != null )
      m_diagTemplate.removeTemplateEventListener( m_obsChart );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    try
    {
      m_sashForm = new SashForm( parent, SWT.HORIZONTAL );
      createMapPanel( m_sashForm );
      final SashForm rightSash = new SashForm( m_sashForm, SWT.VERTICAL );
      createTablePanel( rightSash );
      createDiagramPanel( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
      final int rightWeight = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH, "50" ) );

      m_sashForm.setWeights( new int[]
      { mainWeight, 100 - mainWeight } );

      rightSash.setWeights( new int[]
      { rightWeight, 100 - rightWeight } );

      setControl( m_sashForm );
    }
    catch( final Exception e )
    {
      // TODO handling
      throw new RuntimeException( e );
    }
  }

  private void createDiagramPanel( final Composite parent )
  {
    final String diagFileName = getArguments().getProperty( PROP_DIAGTEMPLATE );
    final IFile diagFile = (IFile)getProject().findMember( diagFileName );

    try
    {
      // actually creates the template
      m_diagTemplate = ObservationTemplateHelper.loadDiagramTemplate( diagFile );

      final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
      m_diagFrame = SWT_AWT.new_Frame( composite );
      m_diagFrame.setVisible( true );

      m_obsChart = new ObservationChart( m_diagTemplate );
      m_diagTemplate.addTemplateEventListener( m_obsChart );

      final ChartPanel chartPanel = new ChartPanel( m_obsChart );
      chartPanel.setMouseZoomable( true, false );

      m_diagFrame.setVisible( true );
      chartPanel.setVisible( true );
      m_diagFrame.add( chartPanel );
    }
    catch( Exception e )
    {
      e.printStackTrace();

      final Text text = new Text( parent, SWT.CENTER );
      text.setText( "Kein Diagram vorhanden" );
    }
  }

  private void createTablePanel( final Composite parent )
  {
    try
    {
      final String templateFileName = getArguments().getProperty( PROP_TABLETEMPLATE );
      final IFile templateFile = (IFile)getProject().findMember( templateFileName );

      m_tableTemplate = ObservationTemplateHelper.loadTableViewTemplate( templateFile );
      m_tableModel.setRules( m_tableTemplate );
      m_tableTemplate.addTemplateEventListener( m_tableModel );

      final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
      m_tableFrame = SWT_AWT.new_Frame( composite );

      final JTable table = new JTable( m_tableModel );
      table.setDefaultRenderer( Date.class, new DateTableCellRenderer() );
      table.setDefaultRenderer( Number.class, new MaskedNumberTableCellRenderer() );
      table.setVisible( true );

      final JScrollPane pane = new JScrollPane( table );
      pane.setBorder( BorderFactory.createEmptyBorder() );

      m_tableFrame.setVisible( true );
      table.setVisible( true );
      m_tableFrame.add( pane );
    }
    catch( Exception e )
    {
      e.printStackTrace();

      final Text text = new Text( parent, SWT.CENTER );
      text.setText( "Keine Tabelle vorhanden" );
    }
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final ViewForm mapView = new ViewForm( parent, SWT.FLAT );

    final String mapFileName = getArguments().getProperty( PROP_MAPTEMPLATE );
    final IFile mapFile = (IFile)getProject().findMember( mapFileName );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile, getReplaceProperties() );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    m_mapModell = new GisTemplateMapModell( gisview, getProject(), crs );
    m_mapModell.addModellListener( this );

    final MapPanel mapPanel = new MapPanel( this, crs, SELECTION_ID );
    mapPanel.setBoundingBox( GisTemplateHelper.getBoundingBox( gisview ) );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    mapPanel.setMapModell( m_mapModell );
    mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    mapPanel.changeWidget( new ToggleSelectWidget() );

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
    return true;
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_diagFrame == null )
      return;

    final IKalypsoLayer layer = m_mapModell.getActiveTheme().getLayer();
    if( !( layer instanceof KalypsoFeatureLayer ) )
      return;

    final List selectedFeatures = new ArrayList();

    final KalypsoFeatureLayer kfl = (KalypsoFeatureLayer)layer;
    final KalypsoFeature[] allFeatures = kfl.getAllFeatures();
    for( int i = 0; i < allFeatures.length; i++ )
      if( allFeatures[i].isSelected( SELECTION_ID ) )
        selectedFeatures.add( allFeatures[i] );

    m_diagTemplate.removeAllCurves();
    m_tableTemplate.removeAllColumns();
      
    if( selectedFeatures.size() > 0 )
    {
      final TimeserieFeatureProps[] tsProps = KalypsoWizardHelper
          .parseTimeserieFeatureProps( getArguments() );

      KalypsoWizardHelper.updateDiagramTemplate( tsProps, selectedFeatures, m_diagTemplate );
      KalypsoWizardHelper.updateTableTemplate( tsProps, selectedFeatures, m_tableTemplate );
    }
  }
}