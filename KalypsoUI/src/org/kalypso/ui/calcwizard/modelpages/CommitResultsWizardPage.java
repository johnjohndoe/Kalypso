package org.kalypso.ui.calcwizard.modelpages;

import java.awt.Frame;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ViewForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.actions.FullExtentMapAction;
import org.kalypso.ogc.gml.map.actions.ToggleSingleSelectWidgetAction;
import org.kalypso.ogc.gml.map.actions.ZoomOutMapAction;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.outline.GisMapOutlineViewer;
import org.kalypso.ogc.gml.table.LayerTableViewer;
import org.kalypso.ogc.gml.widgets.ToggleSelectWidget;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.gistableview.Gistableview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */

public class CommitResultsWizardPage extends AbstractCalcWizardPage implements ModellEventListener
{
  // Beispiel:
  //   <page className="org.kalypso.ui.calcwizard.CommitResultsWizardPage"
  // pageTitle="Kontrolle der Ergebnisse"
  // imageLocation="icons/calcwizard/boden.gif" >
  //        <arg name="mapTemplate" value=".modellTyp/vorlagen/rechenfall/karte2.gmt"/>
  //        <arg name="tableTemplate"
  // value=".modellTyp/vorlagen/rechenfall/table2.gtt"/>
  //        <arg name="timeseriesPropertyNames"
  // value="Wasserstand#Wasserstand_gerechnet"/>
  //        <arg name="mainSash" value="50"/>
  //        <arg name="rightSash" value="40"/>
  //        <arg name="CommitTextTemplate" value="Vorhersageergebnis Spreemodell
  // berechnet mit Kalypso"/>
  //    </page>
  //  

  /** initialer Text f?r die Ergebnisablage */
  public final static String PROP_COMMITTEXTTEMPLATE = "CommitTextTemplate";

  /** Der Titel der Seite */
  public static final String PROP_MAPTITLE = "mapTitle";

  /** Pfad auf Vorlage f?r die Gis-Tabell (.gtt Datei) */
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

  private LayerTableViewer m_viewer;

  private IMapModell m_mapModell;

  private MapPanel m_mapPanel;

  public CommitResultsWizardPage()
  {
    super( "<CommitResultsWizardPage>" );
  }

  public CommitResultsWizardPage( String title )
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
      createCommitTextPanel( rightSash );
      createCommitButton( rightSash );

      final int mainWeight = Integer.parseInt( getArguments().getProperty( PROP_MAINSASH, "50" ) );
      final int rightWeight0 = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH + "0",
          "50" ) );
      final int rightWeight1 = Integer.parseInt( getArguments().getProperty( PROP_RIGHTSASH + "1",
          "20" ) );

      // TODO: konfigure
      sashForm.setWeights( new int[]
      {
          mainWeight,
          100 - mainWeight } );

      rightSash.setWeights( new int[]
      {
          rightWeight0,
          rightWeight0 + rightWeight1,
          100 - rightWeight0 - rightWeight1 } );

      setControl( sashForm );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private void createCommitButton( final Composite parent )
  {
    final Composite composite = new Composite( parent, SWT.RIGHT );

    final Button button = new Button( composite, SWT.NONE | SWT.PUSH );
    button.setText( "ausgew?hlte Pegel in Ergebnisablage speichern" );
    button.addSelectionListener( new CommitResults() );
    button.setVisible( true );
    composite.setVisible( true );
    //button.setEnabled(true);
  }

  private void createCommitTextPanel( final Composite parent )
  {

    final Text text = new Text( parent, SWT.MULTI );
    text.setVisible( true );
    text.setText( getArguments().getProperty( PROP_COMMITTEXTTEMPLATE ) );

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
    m_mapPanel = new MapPanel( this, crs, SELECTION_ID );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    m_mapPanel.setMapModell( m_mapModell );
    m_mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    /////////////
    // Toolbar //
    /////////////
    final ToolBarManager tbm = new ToolBarManager();

    tbm.add( new GroupMarker( "radio_group" ) );

    tbm.appendToGroup( "radio_group", new ToggleSingleSelectWidgetAction( m_mapPanel ) );

    tbm.add( new Separator() );

    tbm.add( new FullExtentMapAction( this, m_mapPanel ) );
    tbm.add( new ZoomOutMapAction( this, m_mapPanel ) );

    final ToolBar toolBar = tbm.createControl( mapView );

    /////////////
    // Outline //
    /////////////
    final GisMapOutlineViewer outlineViewer = new GisMapOutlineViewer( this, m_mapModell );
    outlineViewer.createControl( mapView );

    mapView.setContent( mapComposite );
    mapView.setTopCenter( toolBar );
    mapView.setTopLeft( outlineViewer.getControl() );
    m_mapPanel.changeWidget( new ToggleSelectWidget() );

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
  //
  }

  public IMapModell getMapModel()
  {
    return m_mapModell;
  }

  private class CommitResults extends SelectionAdapter
  {
    /**
     * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org.eclipse.swt.events.SelectionEvent)
     */
    public void widgetSelected( SelectionEvent e )
    {
      System.out.println( "export Grafics" );
      final String propNames = getArguments().getProperty( PROP_TIMEPROPNAME, "" );
      final String[] timeNames = propNames.split( "#" );

      final IKalypsoTheme theme = getMapModel().getActiveTheme();
      if( !( theme instanceof KalypsoFeatureTheme ) )
        return;

      final KalypsoFeatureTheme featureTheme = (KalypsoFeatureTheme)theme;
      final GMLWorkspace workspace = featureTheme.getWorkspace();

      try
      {
        workspace.accept( new FeatureVisitor()
        {
          public boolean visit( final Feature f ) throws Throwable
          {
            if( f.isSelected( SELECTION_ID ) )
            {
              for( int j = 0; j < timeNames.length; j++ )
              {
                Object observation = f.getProperty( timeNames[j] );
                if( observation == null )
                  System.out.println( "observation is null" );
                else
                  System.out.println( "observation is type:" + observation.getClass().toString() );
              }
            }
            
            return true;
          }
        }, featureTheme.getFeatureType(), FeatureVisitor.DEPTH_ZERO );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }

    }
  }
}