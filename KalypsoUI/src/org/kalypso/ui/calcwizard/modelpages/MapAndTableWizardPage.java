package org.kalypso.ui.calcwizard.modelpages;

import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.actions.WorkspaceModifyOperation;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.widgets.SingleElementSelectWidget;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.IDiagramTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.diagview.template.LinkedDiagramTemplate;
import org.kalypso.ogc.sensor.template.ObservationTemplateHelper;
import org.kalypso.ogc.sensor.timeseries.TimeserieFeatureProps;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.nature.ModelNature;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public class MapAndTableWizardPage extends AbstractCalcWizardPage implements ModellEventListener
{
  /** Pfad auf Vorlage f?r die Karte (.gmt Datei) */
  public final static String PROP_MAPTEMPLATE = "mapTemplate";

  /** Der Titel der Seite */
  public static final String PROP_MAPTITLE = "mapTitle";

  /** Pfad auf Vorlage f?r die Gis-Tabell (.gtt Datei) */
  public final static String PROP_TABLETEMPLATE = "tableTemplate";

  /** Pfad auf die Vorlage f?r das Diagramm (.odt Datei) */
  public final static String PROP_DIAGTEMPLATE = "diagTemplate";

  /** Position des Haupt-Sash: Integer von 0 bis 100 */
  public final static String PROP_MAINSASH = "mainSash";

  /** Position des rechten Sash: Integer von 0 bis 100 */
  public final static String PROP_RIGHTSASH = "rightSash";

  /**
   * Basisname der Zeitreihen-Properties. Es kann mehrere Zeitreihen
   * geben-Property geben: eine f?r jede Kurventyp.
   */
  public final static String PROP_TIMEPROPNAME = "timeserie";

  private static final int SELECTION_ID = 0x10;

  private LayerTableViewer m_viewer;

  private IMapModell m_mapModell;

  private GM_Envelope m_boundingBox;

  private MapPanel m_mapPanel;

  private Frame m_diagFrame = null;

  private IDiagramTemplate m_diagTemplate = null;

  private ObservationChart m_obsChart = null;

  private boolean m_useResolver = false;

  public MapAndTableWizardPage()
  {
    super( "<MapAndTableWizardPage>" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    if( m_mapModell != null )
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

      parent.getDisplay().asyncExec( new Runnable()
      {
        public void run()
        {
          maximizeMap();
        }
      } );
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
    {
        mainWeight,
        100 - mainWeight } );

    rightSash.setWeights( new int[]
    {
        rightWeight,
        100 - rightWeight } );

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
    final String diagFileName = getArguments().getProperty( PROP_DIAGTEMPLATE );
    final IFile diagFile = (IFile)getProject().findMember( diagFileName );

    try
    {
      // actually creates the template
      m_diagTemplate = ObservationTemplateHelper.loadDiagramTemplate( diagFile );

      // TODO tricky: to be ameliorated once pool geschichte is better!!!
      ( (LinkedDiagramTemplate)m_diagTemplate ).setUseResolver( m_useResolver );

      final Composite composite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED | SWT.BORDER );
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
      final Gistableview template = GisTemplateHelper.loadGisTableview( templateFile,
          getReplaceProperties() );

      m_viewer = new LayerTableViewer( parent, this, getProject(), KalypsoGisPlugin.getDefault()
          .createFeatureTypeCellEditorFactory(), SELECTION_ID, false );
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

    ////////
    // UI //
    ////////
    final Composite mapPanel = new Composite( parent, SWT.NONE );
    mapPanel.setLayout( new GridLayout() );

    final Composite mapComposite = new Composite( mapPanel, SWT.EMBEDDED | SWT.BORDER );
    mapComposite.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    m_mapPanel.setMapModell( m_mapModell );
    m_mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );
    m_boundingBox = GisTemplateHelper.getBoundingBox( gisview );

    // das kann erst passieren, wenn die Control fertig ist: beim ersten rezise
    // event
    m_mapPanel.setBoundingBox( m_boundingBox );
    m_mapPanel.changeWidget( new SingleElementSelectWidget() );
  }

  /**
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#performFinish()
   */
  public boolean performFinish()
  {
    // TODO: error handling?
    m_viewer.saveData();

    return true;
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_diagFrame == null || !isCurrentPage() )
      return;

    final IKalypsoLayer layer = m_mapModell.getActiveTheme().getLayer();
    if( !( layer instanceof KalypsoFeatureLayer ) )
      return;

    final List selectedFeatures = new ArrayList();

    final KalypsoFeatureLayer kfl = (KalypsoFeatureLayer)layer;
    final Feature[] allFeatures = kfl.getAllFeatures();
    for( int i = 0; i < allFeatures.length; i++ )
      if( allFeatures[i].isSelected( SELECTION_ID ) )
        selectedFeatures.add( allFeatures[i] );

    m_diagTemplate.removeAllCurves();

    if( selectedFeatures.size() > 0 )
    {
      final TimeserieFeatureProps[] tsProps = KalypsoWizardHelper
          .parseTimeserieFeatureProps( getArguments() );

      try
      {
        KalypsoWizardHelper.updateDiagramTemplate( tsProps, selectedFeatures, m_diagTemplate,
            m_useResolver, getContext() );
      }
      catch( final SensorException e )
      {
        e.printStackTrace();
        getShell().getDisplay().asyncExec( new Runnable()
            {
              public void run( )
              {
                MessageDialog.openError( getShell(), "Aktualisierungsfehler", e.getLocalizedMessage() );
              }
            });
      }
    }
  }

  public void maximizeMap()
  {
    m_mapPanel.setBoundingBox( m_boundingBox );
  }

  protected void runCalculation()
  {
    m_viewer.saveData();

    final WorkspaceModifyOperation op = new WorkspaceModifyOperation( null )
    {
      public void execute( final IProgressMonitor monitor ) throws CoreException
      {
//        monitor.beginTask( "Berechnung wird durchgeführt", 2000 );
        final ModelNature nature = (ModelNature)getCalcFolder().getProject().getNature( ModelNature.ID );
        nature.runCalculation( getCalcFolder(), monitor );
      }
    };

    try
    {
      getContainer().run( true, true, op );
    }
    catch( final InterruptedException e )
    {
      // canceled
      // TODO error message?
      return;
    }
    catch( final InvocationTargetException e )
    {
      e.printStackTrace();

      final Throwable te = e.getTargetException();
      if( te instanceof CoreException )
      {
        ErrorDialog.openError( getContainer().getShell(), "Fehler",
            "Fehler beim Aufruf der nächsten Wizard-Seite",
            ( (CoreException)e.getTargetException() ).getStatus() );
      }
      else
      {
        // CoreExceptions are handled above, but unexpected runtime exceptions
        // and errors may still occur.
        MessageDialog.openError( getContainer().getShell(), "Interner Fehler",
            "Fehler beim Aufruf der nächsten Wizard-Seite: " + te.getLocalizedMessage() );
      }
    }
  }
}