package org.kalypso.ui.calcwizard.modelpages;

import java.awt.Frame;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import javax.swing.JScrollPane;
import javax.xml.bind.JAXBException;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureList;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.feature.visitors.GetSelectionVisitor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.MapPanelHelper;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.diagview.impl.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.impl.LinkedTableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.ogc.sensor.zml.ZmlObservation;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.DefaultCommandManager;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypso.zml.obslink.TimeseriesLink;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public abstract class AbstractCalcWizardPage extends WizardPage implements
    IModelWizardPage, ICommandTarget, ModellEventListener
{
  private int m_selectionID = 0x1;

  private static final String PROP_IGNORETYPE1 = "ignoreType1";

  private static final String PROP_IGNORETYPE2 = "ignoreType2";

  private static final String PROP_IGNORELABEL1 = "ignoreLabel1";

  private static final String PROP_IGNORELABEL2 = "ignoreLabel2";

  /** Pfad auf Vorlage für die Gis-Tabell (.gtt Datei) */
  public final static String PROP_TABLETEMPLATE = "tableTemplate";

  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  private final static String PROP_MAPTEMPLATE = "mapTemplate";

  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  private final static String PROP_SELECTIONID = "selectionID";

  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget(
      new DefaultCommandManager(), null );

  private Properties m_arguments = null;

  private IProject m_project = null;

  private IFolder m_calcFolder = null;

  private Properties m_replaceProperties = new Properties();

  private IMapModell m_mapModell = null;

  private MapPanel m_mapPanel;

  private GM_Envelope m_boundingBox;

  private Frame m_diagFrame = null;

  private ObservationChart m_obsChart = null;

  private LinkedDiagramTemplate m_diagTemplate = null;

  private Frame m_tableFrame = null;

  private final ObservationTableModel m_tableModel = new ObservationTableModel();

  private LinkedTableViewTemplate m_tableTemplate = null;

  private ObservationTable m_table = null;

  private TimeserieFeatureProps[] m_tsProps;

  private LayerTableViewer m_viewer;

  private final ControlAdapter m_controlAdapter = new ControlAdapter()
  {
    public void controlResized( final ControlEvent e )
    {
      maximizeMap();
    }
  };

  public AbstractCalcWizardPage( final String name )
  {
    super( name );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose( )
  {
    if( m_mapModell != null )
    {
      m_mapModell.removeModellListener( this );
      m_mapModell.dispose();
    }

    if( m_diagTemplate != null )
    {
      m_diagTemplate.removeTemplateEventListener( m_obsChart );
      m_diagTemplate.dispose();
    }

    if( m_tableTemplate != null )
    {
      m_tableTemplate.removeTemplateEventListener( m_table );
      m_tableTemplate.dispose();
    }
  }

  public Properties getArguments( )
  {
    return m_arguments;
  }

  public IProject getProject( )
  {
    return m_project;
  }

  public IFolder getCalcFolder( )
  {
    return m_calcFolder;
  }

  public URL getContext( )
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
   *      java.lang.String, org.eclipse.jface.resource.ImageDescriptor,
   *      java.util.Properties, org.eclipse.core.resources.IFolder)
   */
  public void init( final IProject project, final String pagetitle,
      final ImageDescriptor imagedesc, final Properties arguments,
      final IFolder calcFolder )
  {
    setTitle( pagetitle );
    setImageDescriptor( imagedesc );
    m_project = project;
    m_arguments = arguments;
    m_tsProps = KalypsoWizardHelper.parseTimeserieFeatureProps( arguments );

    m_calcFolder = calcFolder;

    try
    {
      m_selectionID = Integer.parseInt( m_arguments.getProperty(
          PROP_SELECTIONID, "1" ) );
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
   * @see org.kalypso.util.command.ICommandTarget#postCommand(org.kalypso.util.command.ICommand,
   *      java.lang.Runnable)
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
  protected Properties getReplaceProperties( )
  {
    return m_replaceProperties;
  }

  /**
   * Erzeugt die Karte und alle Daten die dranhängen und gibt die enthaltende
   * Control zurück
   * 
   * @param parent
   * @param widgetID
   * @return control
   * @throws IOException
   * @throws JAXBException
   * @throws CoreException
   */
  protected Control initMap( final Composite parent, final String widgetID )
      throws IOException, JAXBException, CoreException
  {
    final String mapFileName = getArguments().getProperty( PROP_MAPTEMPLATE );
    final IFile mapFile = (IFile) getProject().findMember( mapFileName );
    if( mapFile == null )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Vorlagendatei existiert nicht: " + mapFileName, null ) );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile,
        getReplaceProperties() );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault()
        .getCoordinatesSystem();
    m_mapModell = new GisTemplateMapModell( gisview, getContext(), crs,
        m_selectionID );

    m_mapModell.addModellListener( this );

    m_mapPanel = new MapPanel( this, crs, m_selectionID );
    MapPanelHelper.createWidgetsForMapPanel( parent.getShell(), m_mapPanel );

    m_boundingBox = GisTemplateHelper.getBoundingBox( gisview );
    final Composite mapComposite = new Composite( parent, SWT.BORDER
        | SWT.RIGHT | SWT.EMBEDDED );

    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    m_mapPanel.setMapModell( m_mapModell );
    m_mapPanel
        .onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    m_mapPanel.changeWidget( widgetID );

    m_mapPanel.setBoundingBox( m_boundingBox );

    return mapComposite;
  }

  protected IMapModell getMapModell( )
  {
    return m_mapModell;
  }

  public void maximizeMap( )
  {
    m_mapPanel.setBoundingBox( m_boundingBox );
  }

  protected Control initDiagram( final Composite parent )
  {
    try
    {
      // actually creates the template
      m_diagTemplate = new LinkedDiagramTemplate();

      final String ignoreType = m_arguments
          .getProperty( PROP_IGNORETYPE1, null );
      m_diagTemplate.setIgnoreType( ignoreType );

      final Composite composite = new Composite( parent, SWT.BORDER | SWT.RIGHT
          | SWT.EMBEDDED );
      m_diagFrame = SWT_AWT.new_Frame( composite );
      m_diagFrame.setVisible( true );

      m_obsChart = new ObservationChart( m_diagTemplate );
      m_diagTemplate.addTemplateEventListener( m_obsChart );

      final ChartPanel chartPanel = new ChartPanel( m_obsChart );
      chartPanel.setMouseZoomable( true, false );

      m_diagFrame.setVisible( true );
      chartPanel.setVisible( true );
      m_diagFrame.add( chartPanel );

      refreshTimeseries();

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

  public void clean( final IProgressMonitor monitor )
  {
    // nix zu tun
  }

  public void doNext( final IProgressMonitor monitor )
  {
    // nix zu tun
  }

  /**
   * @see org.kalypso.ui.calcwizard.ICalcWizardPage#update(org.eclipse.core.runtime.IProgressMonitor)
   */
  public void update( final IProgressMonitor monitor )
  {
    // nix tun
  }

  protected ControlAdapter getControlAdapter( )
  {
    return m_controlAdapter;
  }

  public void refreshTimeseries( )
  {
    final TSLinkWithName[] obs = getObservationsToShow();
    refreshObservationsForContext( obs, getContext() );
  }

  protected void refreshObservationsForContext( final TSLinkWithName[] obs,
      final URL context )
  {
    final LinkedDiagramTemplate diagTemplate = m_diagTemplate;
    final LinkedTableViewTemplate tableTemplate = m_tableTemplate;

    if( diagTemplate != null )
      KalypsoWizardHelper.updateDiagramTemplate( diagTemplate, obs, context,
          true );
    if( tableTemplate != null )
      KalypsoWizardHelper.updateTableTemplate( tableTemplate, obs, context,
          true );
  }

  protected abstract TSLinkWithName[] getObservationsToShow( );

  protected void initFeatureTable( final Composite parent )
  {
    try
    {
      final String templateFileName = getArguments().getProperty(
          PROP_TABLETEMPLATE );
      final IFile templateFile = (IFile) getProject().findMember(
          templateFileName );
      final Gistableview template = GisTemplateHelper.loadGisTableview(
          templateFile, getReplaceProperties() );

      m_viewer = new LayerTableViewer( parent, this, KalypsoGisPlugin
          .getDefault().createFeatureTypeCellEditorFactory(), getSelectionID(),
          false );
      m_viewer.applyTableTemplate( template, getContext() );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final Text text = new Text( parent, SWT.NONE );
      text.setText( "Fehler beim Laden des TableTemplate" );
    }
  }

  protected void initDiagramTable( final Composite parent )
  {
    try
    {
      m_table = new ObservationTable( m_tableModel );

      m_tableTemplate = new LinkedTableViewTemplate();
      final String ignoreType = m_arguments
          .getProperty( PROP_IGNORETYPE1, null );
      m_tableTemplate.setIgnoreType( ignoreType );

      m_tableModel.setRules( m_tableTemplate );
      m_tableTemplate.addTemplateEventListener( m_table );

      final Composite composite = new Composite( parent, SWT.RIGHT
          | SWT.EMBEDDED );
      m_tableFrame = SWT_AWT.new_Frame( composite );

      m_table.setVisible( true );

      final JScrollPane pane = new JScrollPane( m_table );

      m_tableFrame.setVisible( true );
      m_table.setVisible( true );
      m_tableFrame.add( pane );
    }
    catch( Exception e )
    {
      // TODO error handling
      e.printStackTrace();

      final Text text = new Text( parent, SWT.CENTER );
      text.setText( "Keine Tabelle vorhanden" );
    }
  }

  protected List getSelectedFeatures( boolean useTable )
  {
    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return new ArrayList();

    final IKalypsoTheme activeTheme;
    if( useTable )
      activeTheme = m_viewer.getTheme();
    else
      activeTheme = mapModell.getActiveTheme();

    if( activeTheme == null )
      return new ArrayList();

    final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) activeTheme;
    final FeatureList featureList = kft.getFeatureList();

    if( featureList == null )
      return new ArrayList();

    return GetSelectionVisitor.getSelectedFeatures( featureList, m_selectionID );
  }

  public TSLinkWithName[] getObservationsFromMap( final boolean useTable )
  {
    final List selectedFeatures = getSelectedFeatures( useTable );

    final Collection foundObservations = new ArrayList( selectedFeatures.size() );

    for( final Iterator it = selectedFeatures.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature) it.next();

      for( int i = 0; i < m_tsProps.length; i++ )
      {
        final String name = (String) kf.getProperty( m_tsProps[i]
            .getNameColumn() );
        final TimeseriesLink obsLink = (TimeseriesLink) kf
            .getProperty( m_tsProps[i].getLinkColumn() );
        if( obsLink != null )
        {
          final TSLinkWithName linkWithName = new TSLinkWithName( name, obsLink
              .getLinktype(), obsLink.getHref(), m_tsProps[i].getFilter() );
          foundObservations.add( linkWithName );
        }
      }
    }

    return (TSLinkWithName[]) foundObservations
        .toArray( new TSLinkWithName[foundObservations.size()] );
  }

  protected TSLinkWithName[] getTimeseriesForProperty( final String name,
      final List features, final String property, final String filter )
  {
    final Collection foundObservations = new ArrayList( features.size() );

    for( final Iterator it = features.iterator(); it.hasNext(); )
    {
      final Feature kf = (Feature) it.next();

      final TimeseriesLink obsLink = (TimeseriesLink) kf.getProperty( property );
      if( obsLink != null )
      {
        final TSLinkWithName linkWithName = new TSLinkWithName( name, obsLink
            .getLinktype(), obsLink.getHref(), filter );
        foundObservations.add( linkWithName );
      }
    }

    return (TSLinkWithName[]) foundObservations
        .toArray( new TSLinkWithName[foundObservations.size()] );
  }

  protected int getSelectionID( )
  {
    return m_selectionID;
  }

  protected LayerTableViewer getLayerTable( )
  {
    return m_viewer;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public final void onModellChange( final ModellEvent modellEvent )
  {
    if( modellEvent != null
        && modellEvent.getType() == ModellEvent.SELECTION_CHANGED )
      refreshTimeseries();
  }

  public TimeserieFeatureProps[] getTsProps( )
  {
    return m_tsProps;
  }

  protected List getFeatures( boolean useTable )
  {
    final IMapModell mapModell = getMapModell();
    if( mapModell == null )
      return new ArrayList();

    final IKalypsoTheme activeTheme;
    if( useTable )
      activeTheme = m_viewer.getTheme();
    else
      activeTheme = mapModell.getActiveTheme();

    if( activeTheme == null )
      return new ArrayList();

    final IKalypsoFeatureTheme kft = (IKalypsoFeatureTheme) activeTheme;
    final FeatureList featureList = kft.getFeatureList();
    if( featureList == null )
      return new ArrayList();

    return featureList;
  }

  protected void setObsIgnoreType( final String ignoreType )
  {
    if( m_diagTemplate != null )
      m_diagTemplate.setIgnoreType( ignoreType );
    if( m_tableTemplate != null )
      m_tableTemplate.setIgnoreType( ignoreType );

    refreshTimeseries();
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#performHelp()
   */
  public void performHelp( )
  {
    // TODO
    // get helpid
    // show help
  }

  protected Composite createIgnoreButtonPanel( final Composite parent )
  {
    // properties lesen
    final String ignoreType1 = m_arguments.getProperty( PROP_IGNORETYPE1, "Q" );
    final String ignoreType2 = m_arguments.getProperty( PROP_IGNORETYPE2, "W" );

    final String ignoreLabel1 = m_arguments.getProperty( PROP_IGNORELABEL1,
        "Abfluss" );
    final String ignoreLabel2 = m_arguments.getProperty( PROP_IGNORELABEL2,
        "Wasserstand" );

    final Composite panel = new Composite( parent, SWT.NONE );
    panel.setLayout( new GridLayout( 3, false ) );
    panel.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );

    final Label label = new Label( panel, SWT.NONE );
    label.setText( "Diagrammanzeige:" );
    final GridData gridData = new GridData();
    gridData.grabExcessHorizontalSpace = true;
    gridData.horizontalAlignment = GridData.END;
    label.setLayoutData( gridData );

    final Button radioQ = new Button( panel, SWT.RADIO );
    radioQ.setText( ignoreLabel1 );
    radioQ.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        setObsIgnoreType( ignoreType1 );
      }
    } );

    final Button radioW = new Button( panel, SWT.RADIO );
    radioW.setText( ignoreLabel2 );

    radioW.addSelectionListener( new SelectionAdapter()
    {
      /**
       * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
       */
      public void widgetSelected( SelectionEvent e )
      {
        setObsIgnoreType( ignoreType2 );
      }
    } );

    radioQ.setSelection( true );

    return panel;
  }

  /**
   * Saves the dirty observations that were edited in the table.
   */
  protected void saveDirtyObservations( )
  {
    final LinkedTableViewTemplate template = m_tableTemplate;
    final ObservationTableModel model = (ObservationTableModel) m_table
        .getModel();

    final Collection themes = template.getThemes();

    for( final Iterator it = themes.iterator(); it.hasNext(); )
    {
      final ITableViewTheme theme = (ITableViewTheme) it.next();

      boolean dirty = false;

      for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
      {
        dirty = ((ITableViewColumn) itcol.next()).isDirty();

        // at least one col dirty?
        if( dirty )
          break;
      }

      final IObservation obs = theme.getObservation();

      if( dirty && obs instanceof ZmlObservation )
      {
        for( Iterator itcol = theme.getColumns().iterator(); itcol.hasNext(); )
          ((ITableViewColumn) itcol.next()).setDirty( false );

        final ITuppleModel values = model.getValues( theme.getColumns() );

        final Job job = new Job( "" )
        {
          protected IStatus run( IProgressMonitor monitor )
          {
            try
            {
              obs.setValues( values );

              template.saveObservation( obs, monitor );
            }
            catch( Exception e )
            {
              e.printStackTrace();
              return KalypsoGisPlugin.createErrorStatus( "", e );
            }

            return Status.OK_STATUS;
          }
        };

        job.schedule();
      }
    }
  }
}