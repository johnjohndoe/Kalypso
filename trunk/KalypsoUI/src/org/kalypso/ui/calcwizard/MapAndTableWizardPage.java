package org.kalypso.ui.calcwizard;

import java.awt.Frame;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.GroupMarker;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
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
import org.kalypso.ogc.gml.GisTemplateMapModell;
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
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public class MapAndTableWizardPage extends WizardPage
{
  public final static String PROP_MAPTEMPLATE = "mapTemplate";
  public static final String PROP_MAPTITLE = "mapTitle";

  public final static String PROP_TABLETEMPLATE = "tableTemplate";


  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private final Properties m_arguments;

  private IProject m_project;


  public MapAndTableWizardPage( final IProject project, final String pagetitle,
      final ImageDescriptor imagedesc, final Properties arguments )
  {
    super( "MapAndTableWizardPage", pagetitle, imagedesc );

    m_arguments = arguments;
    m_project = project;
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {

    final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );

    try
    {
      // links karte
      createMapPanel( sashForm );
      createTablePanel( sashForm );
      
      sashForm.setWeights( new int[] { 70, 30 } );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    setControl( sashForm );
  }

  private void createTablePanel( final Composite parent )
  {
    // rechts tabelle
    final Text text = new Text( parent, SWT.NONE );
    text.setText( m_arguments.getProperty(PROP_TABLETEMPLATE) );
  }

  private void createMapPanel( final Composite parent ) throws Exception, CoreException
  {
    final ViewForm mapView = new ViewForm( parent, SWT.FLAT );

    ///////////////
    // MapModell //
    ///////////////
    final String mapFileName = m_arguments.getProperty( PROP_MAPTEMPLATE );
    final IFile mapFile = (IFile)m_project.findMember( mapFileName );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    final IMapModell mapModell = new GisTemplateMapModell( gisview, m_project, crs );

    ///////////
    // Karte //
    ///////////
    // Karte
    final MapPanel mapPanel = new MapPanel( m_commandTarget, crs );
    final Composite mapComposite = new Composite( mapView, SWT.RIGHT | SWT.EMBEDDED );
    final Frame virtualFrame = SWT_AWT
        .new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    mapPanel.setVisible( true );
    virtualFrame.add( mapPanel );

    mapPanel.setMapModell( mapModell );
    mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );
    
    
    /////////////
    // Toolbar //
    /////////////
    final ToolBarManager tbm = new ToolBarManager( );

    tbm.add( new GroupMarker( "radio_group" ) );
    
    tbm.appendToGroup( "radio_group", new ZoomInWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new PanToWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new ToggleSelectWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new SelectWidgetAction( mapPanel ) );
    tbm.appendToGroup( "radio_group", new UnselectWidgetAction( mapPanel ) );
    
    tbm.add( new Separator() );
    
    tbm.add( new FullExtentMapAction( m_commandTarget, mapPanel ) );
    tbm.add( new ZoomOutMapAction( m_commandTarget, mapPanel ) );
    
    final ToolBar toolBar = tbm.createControl( mapView );

    /////////////
    // Legende //
    /////////////
    final GisMapOutlineViewer outlineViewer = new GisMapOutlineViewer( m_commandTarget, mapModell );
    outlineViewer.createControl( mapView );
    
    mapView.setContent( mapComposite );
    mapView.setTopCenter( toolBar );
    mapView.setTopLeft( outlineViewer.getControl() );
  }
}