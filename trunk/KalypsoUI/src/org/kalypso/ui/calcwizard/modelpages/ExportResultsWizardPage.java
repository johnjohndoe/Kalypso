package org.kalypso.ui.calcwizard.modelpages;

import java.awt.Frame;
import java.io.File;
import java.io.FileOutputStream;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.actions.FullExtentMapAction;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.sensor.diagview.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public class ExportResultsWizardPage extends AbstractCalcWizardPage implements ModellEventListener
{
  // Beispiel:
  //   <page className="org.kalypso.ui.calcwizard.ViewResultsWizardPage"
  // pageTitle="Kontrolle der Ergebnisse"
  // imageLocation="icons/calcwizard/boden.gif" >
  //        <arg name="mapTemplate" value=".modellTyp/vorlagen/rechenfall/karte2.gmt"/>
  //        <arg name="tableTemplate"
  // value=".modellTyp/vorlagen/rechenfall/table2.gtt"/>
  //        <arg name="timeseriesPropertyNames"
  // value="Wasserstand#Wasserstand_gerechnet"/>
  //        <arg name="mainSash" value="50"/>
  //        <arg name="rightSash" value="40"/>
  //        <arg name="grafikToolTemplate" value=".modellTyp/grafik.exe_"/>
  //    </page>
  //  

  /** Der Titel der Seite */
  public static final String PROP_MAPTITLE = "mapTitle";

  /** Pfad auf Vorlage f?r die Gis-Tabell (.gtt Datei) */
  public final static String PROP_TABLETEMPLATE = "tableTemplate";

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  public final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  public final static String PROP_RIGHTSASH = "rightSash";

  /** Pfad auf die Vorlage f?r das Diagramm (.odt Datei) */
  public final static String PROP_DIAGTEMPLATE = "diagTemplate";

  /**
   * Basisname der Zeitreihen-Properties. Es kann mehrere Zeitreihen
   * geben-Property geben: eine f?r jede Kurventyp.
   */
  public final static String PROP_TIMEPROPNAME = "timeserie";

  private LayerTableViewer m_viewer;

  protected IMapModell m_mapModell;

  protected ObsdiagviewType m_obsdiagviewType;

  protected TimeserieFeatureProps[] m_tsProps;

  public ExportResultsWizardPage()
  {
    super( "<ViewResultsWizardPage>" );
  }

  public ExportResultsWizardPage( String title )
  {
    super( title );
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
      createExportPanel( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
      final int rightWeight = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH, "50" ) );

      m_tsProps = KalypsoWizardHelper.parseTimeserieFeatureProps( getArguments() );

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

  private void createExportPanel( final Composite parent )
  {
    final String diagFileName = getArguments().getProperty( PROP_DIAGTEMPLATE );
    final IFile diagFile = (IFile)getProject().findMember( diagFileName );

    try
    {
      m_obsdiagviewType = ObservationTemplateHelper.loadDiagramTemplateXML( diagFile );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    final Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 2, false ) );

    final Button button = new Button( composite, SWT.PUSH );
    button.setText( "Zeitreihe bearbeiten" );
    button.addSelectionListener( new GraficToolStarter() );
    //button.setLayoutData( new GridData(  ) );
    
    final Button doItButton = new Button( composite ,SWT.PUSH );
    doItButton.setText( "Bericht(e) ablegen" );

    new Label( composite, SWT.NONE );
    
    final Button exportQDiagramm = new Button( composite, SWT.CHECK );
    exportQDiagramm.setText( "Durchflussgrafik" );
    
    new Label( composite, SWT.NONE );

    final Button exportWRadio = new Button( composite, SWT.CHECK );
    exportWRadio.setText( "Wasserstandsgrafik" );
    
    new Label( composite, SWT.NONE );
    
    final Button exportTableRadio = new Button( composite, SWT.CHECK );
    exportTableRadio.setText( "Tabelle" );
    
    new Label( composite, SWT.NONE );

    final Button exportMap = new Button( composite, SWT.CHECK );
    exportMap.setText( "Kartenansicht" );
  }

  private void createTablePanel( final Composite parent )
  {
    try
    {
      final String templateFileName = getArguments().getProperty( PROP_TABLETEMPLATE );
      final IFile templateFile = (IFile)getProject().findMember( templateFileName );
      final Gistableview template = GisTemplateHelper.loadGisTableview( templateFile,
          getReplaceProperties() );

      m_viewer = new LayerTableViewer( parent, this, KalypsoGisPlugin.getDefault()
          .createFeatureTypeCellEditorFactory(), SELECTION_ID, true );
      m_viewer.applyTableTemplate( template, getContext() );
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
    m_mapModell = new GisTemplateMapModell( gisview, getContext(), crs );
    m_mapModell.addModellListener( this );

    //////////////
    // MapPanel //
    //////////////
    final MapPanel mapPanel = new MapPanel( this, crs, SELECTION_ID );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    mapPanel.setMapModell( m_mapModell );
    mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    /////////////
    // Toolbar //
    /////////////
    final ToolBarManager tbm = new ToolBarManager();

    tbm.add( new GroupMarker( "radio_group" ) );

//    tbm.appendToGroup( "radio_group", new ToggleSingleSelectWidgetAction( mapPanel ) );

    tbm.add( new Separator() );

    tbm.add( new FullExtentMapAction( this, mapPanel ) );

    final ToolBar toolBar = tbm.createControl( mapView );

    /////////////
    // Outline //
    /////////////
    final GisMapOutlineViewer outlineViewer = new GisMapOutlineViewer( this, m_mapModell );
    outlineViewer.createControl( mapView );

    mapView.setContent( mapComposite );
    mapView.setTopCenter( toolBar );
    mapView.setTopLeft( outlineViewer.getControl() );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    try
    {
      // TODO: error handling?
      m_viewer.saveData( new NullProgressMonitor() );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
      return false;
    }

    return true;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    //
  }

  private class GraficToolStarter implements SelectionListener
  {
    /**
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected( SelectionEvent e )
    {
      final IKalypsoTheme theme = m_mapModell.getActiveTheme();
      if( !( theme instanceof IKalypsoFeatureTheme ) )
        return;

      m_obsdiagviewType.getObservation().clear();

      FileOutputStream fos = null;

      try
      {
        final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)theme;

        final List selectedFeatures = GetSelectionVisitor.getSelectedFeatures( kft.getWorkspace(), kft.getFeatureType(), SELECTION_ID );
        
        if( selectedFeatures.size() > 0 )
          KalypsoWizardHelper.updateXMLDiagramTemplate( m_tsProps, selectedFeatures,
              m_obsdiagviewType );

        // create tmp odt template
        final File file = File.createTempFile( "diag", ".odt" );
        file.deleteOnExit();
        
        fos = new FileOutputStream( file );
        ObservationTemplateHelper.saveDiagramTemplateXML( m_obsdiagviewType, fos );

        ObservationTemplateHelper.openGrafik4odt( file, getProject() );

        file.delete();
      }
      catch( Exception ex )
      {
        ex.printStackTrace();
      }
      finally
      {
        IOUtils.closeQuietly( fos );
      }
    }

    /**
     * @see org.eclipse.swt.events.SelectionListener#widgetDefaultSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetDefaultSelected( SelectionEvent e )
    {
      // empty
    }
  }
}