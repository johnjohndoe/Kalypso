/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.calcwizard.modelpages;

import java.awt.Frame;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.swing.JScrollPane;
import javax.xml.bind.JAXBException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizard;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.jfree.chart.ChartPanel;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.widgets.WidgetHelper;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.util.GisTemplateLoadedThread;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.calcwizard.Arguments;
import org.kalypso.ui.calcwizard.CalcWizard;
import org.kalypso.ui.nature.ModelNature;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.UnselectFeatureVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public abstract class AbstractCalcWizardPage extends WizardPage implements IModelWizardPage, ICommandTarget,
    ModellEventListener
{
  public static final int SELECT_FROM_MAPVIEW = 0;

  public static final int SELECT_FROM_TABLEVIEW = 1;

  public static final int SELECT_FROM_FEATUREVIEW = 2;

  private int m_selectionID = 0x1;

  /** name der modelspec datei, die verwendet wird */
  public final static String PROP_MODELSPEC = "modelspec";

  private static final String PROP_IGNORETYPE1 = "ignoreType1";

  private static final String PROP_IGNORETYPE2 = "ignoreType2";

  private static final String PROP_IGNORELABEL1 = "ignoreLabel1";

  private static final String PROP_IGNORELABEL2 = "ignoreLabel2";

  /**
   * One of 'selected' or 'all'. If 'selected', only Timeseries of selectede features will be shown, else, timeseries of
   * all features will be shown (in zmlTable)
   */
  private final static String PROP_ZMLTABLE_SHOW = "zmlTableShow";

  /** Pfad auf Vorlage für die Gis-Tabell (.gtt Datei) */
  public final static String PROP_TABLETEMPLATE = "tableTemplate";

  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  private final static String PROP_MAPTEMPLATE = "mapTemplate";

  /**
   * Falls true, wird der Context der Karte stets auf den CalcCaseFolder gesetzt. Ansonten wie üblich auf die .gtt
   * Kartei
   */
  public final static String PROP_MAPTEMPLATEISCALC = "mapTemplateContextIsCalcCase";

  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  private final static String PROP_SELECTIONID = "selectionID";

  /**
   * Falls gesetzt, wird das Feature mit dieser ID selektiert, nachdem die Karte geladen wurde. Ansonsten das erste
   * Feature
   */
  private static final String PROP_FEATURE_TO_SELECT_ID = "selectFeatureID";

  /**
   * Falls true, wird die Karte auf den FullExtent maximiert, sonst wird {@link #m_wishBoundingBox}angesetzt
   */
  private static final String PROP_MAXIMIZEMAP = "maximizeMap";

  /**
   * feature with this id will be in the center of the map
   */
  private static final String PROP_PAN_TO_FEATURE_ID = "pantoFeatureId";

  /**
   * select first feature of acitve layer by default ?<br>
   * default is "true" <br>
   * valid values are "true" or "false"
   */
  private static final String PROP_SELECT_FIRST_FEATURE_BY_DEFAULT = "selectFirstFeatureByDefault";

  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), null );

  private Arguments m_arguments = null;

  private IProject m_project = null;

  private IFolder m_calcFolder = null;

  private Properties m_replaceProperties = new Properties();

  private IMapModell m_mapModell = null;

  private MapPanel m_mapPanel;

  private GM_Envelope m_wishBoundingBox;

  private Frame m_diagFrame = null;

  private ObservationChart m_obsChart = null;

  // protected so that subclasses can access it
  protected DiagView m_diagView = null;

  private Frame m_tableFrame = null;

  private TableView m_tableTemplate = null;

  private ObservationTable m_table = null;

  private TimeserieFeatureProps[] m_tsProps;

  private LayerTableViewer m_gisTableViewer;

  private final ControlAdapter m_controlAdapter = new ControlAdapter()
  {
    public void controlResized( final ControlEvent e )
    {
      maximizeMap();
    }
  };

  private boolean m_showZmlTableOnlySelected = true;

  private final int m_selectSource;

  private String m_ignoreType;

  public AbstractCalcWizardPage( final String name, final int selectSource )
  {
    super( name );
    m_selectSource = selectSource;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    if( m_mapModell != null )
    {
      m_mapModell.removeModellListener( this );
      m_mapModell.dispose();
    }

    if( m_gisTableViewer != null )
      m_gisTableViewer.dispose();

    if( m_table != null )
      m_table.dispose();
    if( m_tableTemplate != null )
      m_tableTemplate.dispose();

    if( m_obsChart != null )
      m_obsChart.dispose();

    if( m_diagView != null )
      m_diagView.dispose();
  }

  public Arguments getArguments()
  {
    return m_arguments;
  }

  public IProject getProject()
  {
    return m_project;
  }

  public IFolder getCalcFolder()
  {
    return m_calcFolder;
  }

  public URL getContext()
  {
    try
    {
      return ResourceUtilities.createURL( getCalcFolder() );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#init(org.eclipse.core.resources.IProject,
   *      java.lang.String, org.eclipse.jface.resource.ImageDescriptor, org.kalypso.ui.calcwizard.Arguments,
   *      org.eclipse.core.resources.IFolder)
   */
  public void init( final IProject project, final String pagetitle, final ImageDescriptor imagedesc,
      final Arguments arguments, final IFolder calcFolder )
  {
    setTitle( pagetitle );
    setImageDescriptor( imagedesc );
    m_project = project;
    m_arguments = arguments;
    m_tsProps = KalypsoWizardHelper.parseTimeserieFeatureProps( arguments );

    m_calcFolder = calcFolder;

    final String zmlTableSelected = m_arguments.getProperty( PROP_ZMLTABLE_SHOW, "selected" );
    m_showZmlTableOnlySelected = "selected".equalsIgnoreCase( zmlTableSelected );

    try
    {
      m_selectionID = Integer.parseInt( m_arguments.getProperty( PROP_SELECTIONID, "1" ) );
    }
    catch( final NumberFormatException nfe )
    {
      nfe.printStackTrace();
    }

    try
    {
      final URL calcURL = ResourceUtilities.createURL( calcFolder );
      m_replaceProperties.setProperty( "calcdir:", calcURL.toString() );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand, java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  /**
   * Diese Properties werden benutzt, um die Vorlagendateien zu parsen
   * 
   * @return properties
   */
  protected Properties getReplaceProperties()
  {
    return m_replaceProperties;
  }

  /**
   * Erzeugt die Karte und alle Daten die dranhängen und gibt die enthaltende Control zurück
   * 
   * @param parent
   * @param widgetID
   * @return control
   * @throws IOException
   * @throws JAXBException
   * @throws CoreException
   */
  protected Control initMap( final Composite parent, final String widgetID ) throws IOException, JAXBException,
      CoreException
  {
    final String mapFileName = getArguments().getProperty( PROP_MAPTEMPLATE );
    final boolean mapContextIsCalcCase = Boolean.valueOf(
        getArguments().getProperty( PROP_MAPTEMPLATEISCALC, Boolean.TRUE.toString() ) ).booleanValue();
    final IFile mapFile = (IFile)getProject().findMember( mapFileName );
    if( mapFile == null )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Vorlagendatei existiert nicht: " + mapFileName,
          null ) );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile, getReplaceProperties() );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    final URL context = mapContextIsCalcCase ? getContext() : ResourceUtilities.createURL( mapFile );
    m_mapModell = new GisTemplateMapModell( gisview, context, crs, mapFile.getProject() );
    m_mapModell.addModellListener( this );

    m_mapPanel = new MapPanel( this, crs, m_selectionID );
    //   MapPanelHelper.createWidgetsForMapPanel( parent.getShell(), m_mapPanel );

    m_wishBoundingBox = GisTemplateHelper.getBoundingBox( gisview );
    final String panToFid = getArguments().getProperty( PROP_PAN_TO_FEATURE_ID, null );
    if( panToFid != null )
    {
      final IMapModell mapModell = getMapModell();
      IKalypsoTheme activeTheme = mapModell.getActiveTheme();
      final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)activeTheme;
      final GMLWorkspace workspace = kft.getWorkspace();
      final Feature feature = workspace.getFeature( panToFid );
      if( feature != null )
      {
        GM_Object defaultGeometryProperty = feature.getDefaultGeometryProperty();
        GM_Point centroid = defaultGeometryProperty.getCentroid();
        m_wishBoundingBox = m_wishBoundingBox.getPaned( centroid );
      }
    }
    if( "true".equals( getArguments().getProperty( PROP_MAXIMIZEMAP, "false" ) ) )
      m_wishBoundingBox = null;

    final Composite mapComposite = new Composite( parent, SWT.BORDER | SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    m_mapPanel.setMapModell( m_mapModell );
    m_mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    // TODO: use widgetID from Konfiguration

    m_mapPanel.getWidgetManager().setActualWidget( WidgetHelper.getWidget( widgetID ) );

    m_mapPanel.setBoundingBox( m_wishBoundingBox );

    return mapComposite;
  }

  protected IMapModell getMapModell()
  {
    return m_mapModell;
  }

  public final void maximizeMap()
  {
    if( m_wishBoundingBox == null )
    {
      final GM_Envelope fullExtentBoundingBox = m_mapPanel.getMapModell().getFullExtentBoundingBox();
      if( fullExtentBoundingBox != null )
      {
        final double buffer = Math.max( fullExtentBoundingBox.getWidth(), fullExtentBoundingBox.getHeight() ) * 0.025;
        m_mapPanel.setBoundingBox( fullExtentBoundingBox.getBuffer( buffer ) );
      }
    }
    else
      m_mapPanel.setBoundingBox( m_wishBoundingBox );
  }

  protected Control initDiagram( final Composite parent )
  {
    try
    {
      // actually creates the template
      m_diagView = new DiagView( true );

      final String ignoreType = m_arguments.getProperty( PROP_IGNORETYPE1, null );
      m_ignoreType = ignoreType;

      final Composite composite = new Composite( parent, SWT.BORDER | SWT.RIGHT | SWT.EMBEDDED );
      m_diagFrame = SWT_AWT.new_Frame( composite );
      m_diagFrame.setVisible( true );

      m_obsChart = new ObservationChart( m_diagView );

      // chart panel without any popup menu
      final ChartPanel chartPanel = new ChartPanel( m_obsChart, false, false, false, false, false );
      chartPanel.setMouseZoomable( true, false );

      m_diagFrame.setVisible( true );
      chartPanel.setVisible( true );
      m_diagFrame.add( chartPanel );

      return composite;
    }
    catch( Exception e )
    {
      e.printStackTrace();

      final Text text = new Text( parent, SWT.CENTER );
      text.setText( "Kein Diagram vorhanden" );

      return text;
    }
  }

  protected ControlAdapter getControlAdapter()
  {
    return m_controlAdapter;
  }

  public void refreshDiagram()
  {
    final TSLinkWithName[] obs = getObservationsToShow( true );
    refreshDiagramForContext( obs, getContext() );
  }

  public void refreshZMLTable()
  {
    final TSLinkWithName[] obs = getObservationsToShow( m_showZmlTableOnlySelected );
    refreshZmlTableForContext( obs, getContext() );
  }

  protected void refreshDiagramForContext( final TSLinkWithName[] obs, final URL context )
  {
    if( m_diagView != null )
      KalypsoWizardHelper.updateZMLView( m_diagView, obs, context, true, m_ignoreType );
  }

  protected void refreshZmlTableForContext( final TSLinkWithName[] obs, final URL context )
  {
    if( m_tableTemplate != null )
      KalypsoWizardHelper.updateZMLView( m_tableTemplate, obs, context, true, m_ignoreType );
  }

  protected abstract TSLinkWithName[] getObservationsToShow( final boolean onlySelected );

  protected void initFeatureTable( final Composite parent )
  {
    try
    {
      final String templateFileName = (String)getArguments().get( PROP_TABLETEMPLATE );
      final IFile templateFile = (IFile)getProject().findMember( templateFileName );
      final Gistableview template = GisTemplateHelper.loadGisTableview( templateFile, getReplaceProperties() );

      m_gisTableViewer = new LayerTableViewer( parent, SWT.BORDER, this, KalypsoGisPlugin.getDefault()
          .createFeatureTypeCellEditorFactory(), getSelectionID() );
      m_gisTableViewer.applyTableTemplate( template, getContext() );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final Text text = new Text( parent, SWT.NONE );
      text.setText( "Fehler beim Laden des TableTemplate" );
    }
  }

  protected Control initZmlTable( final Composite parent )
  {
    try
    {
      m_tableTemplate = new TableView();
      m_table = new ObservationTable( m_tableTemplate );

      final String ignoreType = m_arguments.getProperty( PROP_IGNORETYPE1, null );
      m_ignoreType = ignoreType;

      final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
      m_tableFrame = SWT_AWT.new_Frame( composite );

      m_table.setVisible( true );

      final JScrollPane pane = new JScrollPane( m_table );

      m_tableFrame.setVisible( true );
      m_table.setVisible( true );
      m_tableFrame.add( pane );

      return composite;
    }
    catch( Exception e )
    {
      // todo error handling?
      e.printStackTrace();

      final Text text = new Text( parent, SWT.CENTER );
      text.setText( "Keine Tabelle vorhanden" );

      return text;
    }
  }

  public TSLinkWithName[] getObservations( boolean onlySelected )
  {
    final List selectedFeatures = onlySelected ? getSelectedFeatures() : getFeatures();

    final Collection foundObservations = new ArrayList( selectedFeatures.size() );

    for( final Iterator it = selectedFeatures.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      for( int i = 0; i < m_tsProps.length; i++ )
      {
        final TimeserieFeatureProps tsprop = m_tsProps[i];

        final String nameColumn = tsprop.getNameColumn();
        String name = tsprop.getNameString();
        if( nameColumn != null )
        {
          final Object fname = kf.getProperty( tsprop.getNameColumn() );
          if( fname != null )
            name = name.replaceAll( "%featureprop%", fname.toString() );
        }
        else
          name = tsprop.getNameString();

        final TimeseriesLink obsLink = (TimeseriesLink)kf.getProperty( tsprop.getLinkColumn() );
        if( obsLink != null )
        {
          final TSLinkWithName linkWithName = new TSLinkWithName( name, obsLink.getLinktype(), obsLink.getHref(),
              tsprop.getFilter(), tsprop.getColor() );
          foundObservations.add( linkWithName );
        }
      }
    }

    return (TSLinkWithName[])foundObservations.toArray( new TSLinkWithName[foundObservations.size()] );
  }

  protected TSLinkWithName[] getTimeseriesForProperty( final String name, final List features, final String property,
      final String filter )
  {
    final Collection foundObservations = new ArrayList( features.size() );

    for( final Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature)it.next();

      final TimeseriesLink obsLink = (TimeseriesLink)kf.getProperty( property );
      if( obsLink != null )
      {
        final TSLinkWithName linkWithName = new TSLinkWithName( name, obsLink.getLinktype(), obsLink.getHref(), filter,
            null );
        foundObservations.add( linkWithName );
      }
    }

    return (TSLinkWithName[])foundObservations.toArray( new TSLinkWithName[foundObservations.size()] );
  }

  protected int getSelectionID()
  {
    return m_selectionID;
  }

  protected LayerTableViewer getLayerTable()
  {
    return m_gisTableViewer;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public final void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent != null && modellEvent.isType( ModellEvent.SELECTION_CHANGED ) )
    {
      refreshDiagram();

      if( m_showZmlTableOnlySelected )
      {
        // Save the observations before refreshing else
        // changes are lost
        saveDirtyObservations( new NullProgressMonitor() );

        refreshZMLTable();
      }
    }
  }

  public TimeserieFeatureProps[] getTsProps()
  {
    return m_tsProps;
  }

  protected List getSelectedFeatures()
  {
    return getFeatures( true );
  }

  protected FeatureList getFeatures()
  {
    return getFeatures( false );
  }

  private FeatureList getFeatures( boolean selected )
  {
    final IKalypsoTheme activeTheme;
    switch( m_selectSource )
    {
    case SELECT_FROM_MAPVIEW:
      final IMapModell mapModell = getMapModell();
      if( mapModell == null )
        return FeatureFactory.createFeatureList( null, null );
      activeTheme = mapModell.getActiveTheme();
      break;
    case SELECT_FROM_TABLEVIEW:
      activeTheme = m_gisTableViewer.getTheme();
      break;
    case SELECT_FROM_FEATUREVIEW:
      activeTheme = null;
      break;
    default:
      activeTheme = null;
    }
    if( activeTheme == null )
      return FeatureFactory.createFeatureList( null, null );
    final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)activeTheme;
    final FeatureList featureList = kft.getFeatureList();
    if( featureList == null )
      return FeatureFactory.createFeatureList( null, null );
    if( selected )
      return FeatureFactory.createFeatureList( null, null, GetSelectionVisitor.getSelectedFeatures( featureList,
          m_selectionID ) );
    return featureList;
  }

  protected void setObsIgnoreType( final String ignoreType )
  {
    m_ignoreType = ignoreType;

    refreshDiagram();
    refreshZMLTable();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#performHelp()
   */
  public void performHelp()
  {
  // TODO
  // get helpid
  // show help
  }

  protected Composite createIgnoreButtonPanel( final Composite parent )
  {
    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 3, false ) );

    // properties lesen
    final String ignoreType1 = m_arguments.getProperty( PROP_IGNORETYPE1, null );
    final String ignoreType2 = m_arguments.getProperty( PROP_IGNORETYPE2, null );

    final String ignoreLabel1 = m_arguments.getProperty( PROP_IGNORELABEL1, ignoreType2 );
    final String ignoreLabel2 = m_arguments.getProperty( PROP_IGNORELABEL2, ignoreType1 );

    if( ignoreType1 == null || ignoreType2 == null )
      return null;

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "Diagrammanzeige:" );
    final GridData gridData = new GridData();
    gridData.grabExcessHorizontalSpace = true;
    gridData.horizontalAlignment = GridData.END;
    label.setLayoutData( gridData );

    final Button radioQ = new Button( panel, SWT.RADIO );
    radioQ.setText( ignoreLabel1 );

    final Button radioW = new Button( panel, SWT.RADIO );
    radioW.setText( ignoreLabel2 );

    radioQ.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( final SelectionEvent e )
      {
        if( radioQ.getSelection() )
          setObsIgnoreType( ignoreType1 );
      }
    } );

    radioW.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        if( radioW.getSelection() )
          setObsIgnoreType( ignoreType2 );
      }
    } );

    radioQ.setSelection( true );

    return panel;
  }

  /**
   * Saves the dirty observations that were edited in the table.
   */
  protected void saveDirtyObservations( final IProgressMonitor monitor )
  {
    if( m_table == null )
      return;

    final ObservationTableModel model = (ObservationTableModel)m_table.getModel();
    model.saveDirtyObservations( monitor );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#saveData(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void saveData( final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Daten speichern", 2000 );

    try
    {
      saveDirtyObservations( new SubProgressMonitor( monitor, 1000 ) );
      if( m_gisTableViewer != null )
        m_gisTableViewer.saveData( new SubProgressMonitor( monitor, 1000 ) );
      else
        monitor.worked( 1000 );
    }
    finally
    {
      monitor.done();
    }
  }

  protected void runCalculation()
  {
    final IWizard wizard = getWizard();
    final RunnableContextHelper op = new RunnableContextHelper( getContainer() )
    {
      public void run( IProgressMonitor monitor ) throws InvocationTargetException
      {
        try
        {
          final IWizardPage[] pages = wizard.getPages();

          monitor.beginTask( "Berechnung wird durchgeführt", 1000 + pages.length * 100 );

          for( int i = 0; i < pages.length; i++ )
          {
            final IWizardPage page = pages[i];
            if( page instanceof IModelWizardPage )
              ( (IModelWizardPage)page ).saveData( new SubProgressMonitor( monitor, 100 ) );
            else
              monitor.worked( 100 );
          }

          final ModelNature nature = (ModelNature)getCalcFolder().getProject().getNature( ModelNature.ID );
          final String modelspec = getArguments().getProperty( PROP_MODELSPEC, null );
          final IStatus status = nature.runCalculation( getCalcFolder(), new SubProgressMonitor( monitor, 1000 ),
              modelspec );

          // alle Modellseiten refreshen
          for( int i = 0; i < pages.length; i++ )
          {
            final IWizardPage page = pages[i];
            if( page instanceof AbstractCalcWizardPage )
              ( (AbstractCalcWizardPage)page ).onModellChange( new ModellEvent( null, ModellEvent.SELECTION_CHANGED ) );
          }

          if( !status.isOK() )
            throw new CoreException( status );
        }
        catch( final CoreException e )
        {
          throw new InvocationTargetException( e );
        }
      }
    };

    // lock buttons
    try
    {
      ( (CalcWizard)getWizard() ).setButtonsLocked( true );
      op.runAndHandleOperation( getShell(), true, true, "Hochwasser Vorhersage", "Berechnung" );
    }
    finally
    {
      ( (CalcWizard)getWizard() ).setButtonsLocked( false );
    }
  }

  protected void postCreateControl()
  {
    new GisTemplateLoadedThread( getMapModell(), new Runnable()
    {
      public void run()
      {
        // erstes feature des aktiven themas selektieren
        final IKalypsoTheme activeTheme = getMapModell().getActiveTheme();
        if( activeTheme instanceof IKalypsoFeatureTheme )
        {
          final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme)activeTheme;
          final GMLWorkspace workspace = kft.getWorkspace();

          final FeatureList featureList = kft.getFeatureListVisible( null );
          if( featureList != null && featureList.size() != 0 )
          {
            int selectionID = getSelectionID();
            featureList.accept( new UnselectFeatureVisitor( selectionID ) );

            final String fid = getArguments().getProperty( PROP_FEATURE_TO_SELECT_ID, null );
            final Feature feature;
            if( fid != null )
            {
              feature = workspace.getFeature( fid );
            }
            else
            {
              if( selectFirstFeatureByDefault() )
                feature = (Feature)featureList.get( 0 );
              else
                feature = null;
            }

            if( feature != null )
              feature.select( selectionID );
          }
        }
        refreshDiagram();
        refreshZMLTable();
        maximizeMap();
      }
    } ).start();
  }

  boolean selectFirstFeatureByDefault()
  {
    return "true".equals( getArguments().getProperty( PROP_SELECT_FIRST_FEATURE_BY_DEFAULT, "true" ) );
  }
}