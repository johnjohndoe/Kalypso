package org.kalypso.ui.calcwizard.modelpages;

import java.awt.Frame;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Properties;

import javax.xml.bind.JAXBException;

import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.deegree.model.geometry.GM_Envelope;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Belger
 */
public abstract class AbstractCalcWizardPage extends WizardPage implements IModelWizardPage,
    ICommandTarget, ModellEventListener
{
  public static final int SELECTION_ID = 0x10;
  
  /** Pfad auf Vorlage für die Karte (.gmt Datei) */
  public final static String PROP_MAPTEMPLATE = "mapTemplate";
  
  private final ICommandTarget m_commandTarget = new JobExclusiveCommandTarget( null );

  private Properties m_arguments = null;

  private IProject m_project = null;

  private IFolder m_calcFolder = null;
  
  private Properties m_replaceProperties = new Properties();
  
  private IMapModell m_mapModell = null;

  private MapPanel m_mapPanel;
  
  private GM_Envelope m_boundingBox;

  public AbstractCalcWizardPage( final String name )
  {
    super( name );
  }
  
  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#dispose()
   */
  public void dispose()
  {
    if( m_mapModell != null )
      m_mapModell.removeModellListener( this );
  }

  public Properties getArguments()
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
   * @see org.kalypso.ui.calcwizard.modelpages.IModelWizardPage#init(org.eclipse.core.resources.IProject, java.lang.String, org.eclipse.jface.resource.ImageDescriptor, java.util.Properties, org.eclipse.core.resources.IFolder)
   */
  public void init( final IProject project, final String pagetitle,
      final ImageDescriptor imagedesc, final Properties arguments, final IFolder calcFolder )
  {
    setTitle( pagetitle );
    setImageDescriptor( imagedesc );
    m_project = project;
    m_arguments = arguments;
    m_calcFolder = calcFolder;
    
    try
    {
      final URL calcURL = ResourceUtilities.createURL( calcFolder );
      m_replaceProperties.setProperty( "calcdir:", calcURL.toString() );
    }
    catch( final MalformedURLException e )
    {
      // TODO: error handling
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

  /** Diese Properties werden benutzt, um die Vorlagendateien zu parsen */
  protected Properties getReplaceProperties()
  {
    return m_replaceProperties;
  }


  /** Erzeugt die Karte und alle Daten die dranhängen und gibt die 
   * enthaltende Control zurück */
  protected Control initMap( final Composite parent, final IWidget widget ) throws IOException, JAXBException, CoreException
  {
    final String mapFileName = getArguments().getProperty( PROP_MAPTEMPLATE );
    final IFile mapFile = (IFile)getProject().findMember( mapFileName );
    if( mapFile == null )
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Vorlagendatei existiert nicht: " + mapFileName, null ) );

    final Gismapview gisview = GisTemplateHelper.loadGisMapView( mapFile, getReplaceProperties() );
    final CS_CoordinateSystem crs = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
    m_mapModell = new GisTemplateMapModell( gisview, getContext(), crs );

    m_mapModell.addModellListener( this );

    m_mapPanel = new MapPanel( this, crs, SELECTION_ID );
    m_boundingBox = GisTemplateHelper.getBoundingBox( gisview );
    final Composite mapComposite = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
    
    final Frame virtualFrame = SWT_AWT.new_Frame( mapComposite );

    virtualFrame.setVisible( true );
    m_mapPanel.setVisible( true );
    virtualFrame.add( m_mapPanel );

    m_mapPanel.setMapModell( m_mapModell );
    m_mapPanel.onModellChange( new ModellEvent( null, ModellEvent.THEME_ADDED ) );

    m_mapPanel.changeWidget( widget );

    m_mapPanel.setBoundingBox( m_boundingBox );
    
    return mapComposite;
  }

  protected IMapModell getMapModell()
  {
    return m_mapModell;
  }
  
  public void maximizeMap()
  {
    m_mapPanel.setBoundingBox( m_boundingBox );
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
}