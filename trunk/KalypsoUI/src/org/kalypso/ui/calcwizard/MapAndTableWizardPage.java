package org.kalypso.ui.calcwizard;

import java.awt.Frame;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.MapPanel;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.kalypso.template.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public class MapAndTableWizardPage extends WizardPage
{
  public final static String PROP_MAPTEMPLATE = "mapTemplate";

  public final static String PROP_TABLETEMPLATE = "tableTemplate";

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
      final String mapFileName = m_arguments.getProperty( PROP_MAPTEMPLATE );
      final IFile mapFile = (IFile)m_project.findMember( mapFileName );

      final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile );
      final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      final IMapModell mapModell = new GisTemplateMapModell( gisview, m_project, crs );

      final MapPanel mapPanel = new MapPanel( crs );

      final Frame virtualFrame = SWT_AWT
          .new_Frame( new Composite( sashForm, SWT.RIGHT | SWT.EMBEDDED ) );

      virtualFrame.setVisible( true );
      mapPanel.setVisible( true );
      virtualFrame.add( mapPanel );

      mapPanel.setMapModell( mapModell );
      mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // rechts tabelle
    final Text text = new Text( sashForm, SWT.NONE );
    text.setText( m_arguments.getProperty(PROP_TABLETEMPLATE) );
    

    setControl( sashForm );
  }
}