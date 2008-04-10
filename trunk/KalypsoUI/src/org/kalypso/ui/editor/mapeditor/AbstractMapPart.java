/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.editor.mapeditor;

import java.awt.Rectangle;
import java.net.URL;

import org.apache.commons.configuration.Configuration;
import org.eclipse.core.commands.common.CommandException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.contexts.IContextActivation;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.internal.util.StatusLineContributionItem;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.commands.CommandUtilities;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter2;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.ImageExportPage;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.BaseMapSchedulingRule;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.mapmodel.MapModellContextSwitcher;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypsodeegree.model.feature.event.ModellEventProvider;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Abstract superclass for map editor and map view. Inherits from AbstractEditorPart for editor behavior (save when
 * dirty, command target). Based on the old {@link GisMapEditor} implementation.
 * 
 * @author Stefan Kurzbach
 */
// TODO: Why is it right here to inherit from AbstractEdtiorPart even when used within a View? Please comment on that.
// (SK) This might have to be looked at. GisMapEditor used to implement AbstractEditorPart for basic gml editor
// functionality (save when dirty, command target).
@SuppressWarnings("restriction")
public abstract class AbstractMapPart extends AbstractEditorPart implements IExportableObjectFactory, IMapPanelProvider
{
  private final IFeatureSelectionManager m_selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

  public final StatusLineContributionItem m_statusBar = new StatusLineContributionItem( "MapViewStatusBar", 100 );

  private final MapModellContextSwitcher m_mapModellContextSwitcher = new MapModellContextSwitcher();

  private IWorkbenchPartSite m_site;

  private MapPanel m_mapPanel;

  private GisTemplateMapModell m_mapModell;

  private Control m_control;

  private boolean m_disposed = false;

  private String m_partName;

  private IFile m_file;

  private IResourceChangeListener m_resourceChangeListener;

  protected boolean m_saving;

  public static final String MAP_COMMAND_CATEGORY = "org.kalypso.ogc.gml.map.category";

  private final IMapPanelListener m_mapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.MapPanelAdapter#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel, java.lang.String)
     */
    @Override
    public void onMessageChanged( final MapPanel source, final String message )
    {
      final Display display = getSite().getShell().getDisplay();

      /* Update the text. */
      display.asyncExec( new Runnable()
      {
        public void run( )
        {
          m_statusBar.setText( message );
        }
      } );
    }
  };

  private final IPartListener2 m_partListener = new PartAdapter2()
  {
    private static final String MAP_CONTEXT = "org.kalypso.ogc.gml.map.context";

    private IContextActivation m_activateContext;

    /**
     * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter2#partActivated(org.eclipse.ui.IWorkbenchPartReference)
     */
    @Override
    public void partActivated( final IWorkbenchPartReference partRef )
    {
      if( partRef.getPart( false ) == AbstractMapPart.this )
      {
        final IContextService contextService = (IContextService) getSite().getService( IContextService.class );
        if( contextService != null )
          m_activateContext = contextService.activateContext( MAP_CONTEXT );
      }
    }

    /**
     * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter2#partDeactivated(org.eclipse.ui.IWorkbenchPartReference)
     */
    @Override
    public void partDeactivated( IWorkbenchPartReference partRef )
    {
      if( partRef.getPart( false ) == AbstractMapPart.this )
      {
        final IContextService contextService = (IContextService) getSite().getService( IContextService.class );
        if( contextService != null && m_activateContext != null )
          contextService.deactivateContext( m_activateContext );
      }
    }
  };

  protected AbstractMapPart( )
  {
    super();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
   */
  @Override
  public void init( final IEditorSite site, final IEditorInput input )
  {
    super.init( site, input );
    initMapPanel( site );

    if( input instanceof IStorageEditorInput )
      try
      {
        startLoadJob( ((IStorageEditorInput) input).getStorage() );
      }
      catch( final CoreException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
  }

  /**
   * @see org.eclipse.ui.IViewPart#init(org.eclipse.ui.IViewSite)
   */
  public void init( final IViewSite site )
  {
    setSite( site );

    initMapPanel( site );
  }

  private void initMapPanel( final IWorkbenchPartSite site )
  {
    m_statusBar.setText( "< Welcome to the Map >" );

    // both IViewSite und IEditorSite give access to actionBars
    final IActionBars actionBars = getActionBars( site );

    actionBars.getStatusLineManager().add( m_statusBar );
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), getCommandTarget().undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), getCommandTarget().redoAction );
    actionBars.updateActionBars();

    /* Register this view at the mapPanel. */
    m_mapPanel = new MapPanel( this, m_selectionManager );
    m_mapPanel.addMapPanelListener( m_mapPanelListener );

    m_resourceChangeListener = new IResourceChangeListener()
    {
      public void resourceChanged( final IResourceChangeEvent event )
      {
        if( m_saving )
          return;
        final IFile file = getFile();
        if( file == null )
          return;
        if( event.getType() != IResourceChangeEvent.POST_CHANGE )
          return;
        final IResourceDelta rootDelta = event.getDelta();
        final IResourceDelta fileDelta = rootDelta.findMember( file.getFullPath() );
        if( fileDelta == null )
          return;
        if( (fileDelta.getFlags() & IResourceDelta.CONTENT) != 0 )
          startLoadJob( file );
      }
    };
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    final IWorkbenchPartSite site = getSite();

    m_control = MapPartHelper.createMapPanelPartControl( parent, m_mapPanel, site );
    site.setSelectionProvider( m_mapPanel );

    site.getPage().addPartListener( m_partListener );

    m_mapModellContextSwitcher.setMapModell( m_mapPanel.getMapModell() );

    final AbstractMapPart thisPart = this;
    getSite().getPage().addPartListener( new IPartListener2()
    {

      public void partActivated( final IWorkbenchPartReference partRef )
      {
        try
        {
          CommandUtilities.refreshElementsForWindow( thisPart.getSite().getWorkbenchWindow(), MAP_COMMAND_CATEGORY );
        }
        catch( final CommandException e )
        {
          KalypsoGisPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
        }
      }

      public void partBroughtToTop( final IWorkbenchPartReference partRef )
      {
        // nothing to do
      }

      public void partClosed( final IWorkbenchPartReference partRef )
      {
        // nothing to do
      }

      public void partDeactivated( final IWorkbenchPartReference partRef )
      {
        // nothing to do
      }

      public void partHidden( final IWorkbenchPartReference partRef )
      {
        // nothing to do
      }

      public void partInputChanged( final IWorkbenchPartReference partRef )
      {
        // nothing to do
      }

      public void partOpened( final IWorkbenchPartReference partRef )
      {
        // nothing to do

      }

      public void partVisible( final IWorkbenchPartReference partRef )
      {
        // nothing to do
      }

    } );

    final IContextService siteContextService = (IContextService) site.getService( IContextService.class );
    if( siteContextService != null )
      m_mapModellContextSwitcher.addContextService( siteContextService );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus( )
  {
    if( m_control != null && !m_control.isDisposed() )
      m_control.setFocus();
  }

  /**
   * @see org.eclipse.ui.IViewPart#getViewSite()
   */
  public IViewSite getViewSite( )
  {
    return (IViewSite) getSite();
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getSite()
   */
  @Override
  public IWorkbenchPartSite getSite( )
  {
    return m_site;
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#setSite(org.eclipse.ui.IWorkbenchPartSite)
   */
  @Override
  protected void setSite( final IWorkbenchPartSite site )
  {
    m_site = site;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IStorageEditorInput)
   */
  @Override
  protected void loadInternal( final IProgressMonitor monitor, final IStorageEditorInput input ) throws Exception, CoreException
  {
    if( m_mapPanel != null )
      loadMap( monitor, input.getStorage() );
  }

  /**
   * Loads a map (i.e. a .gmt file) from a storage inside a {@link Job}.
   * <p>
   * The method starts a (user-)job, which loads the map.
   * </p>.
   * 
   * @param waitFor
   *            <code>true</code> if this method should return when the job has finished, if <code>false</code>
   *            returns immediately
   */
  public void startLoadJob( final IStorage storage )
  {
    final Job job = new Job( "Karte laden: " + storage.getName() )
    {
      @Override
      public IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          loadMap( monitor, storage );
        }
        catch( final CoreException e )
        {
          e.printStackTrace(); // TODO: remove
          return e.getStatus();
        }
        return Status.OK_STATUS;
      }
    };

    final IFile file = storage instanceof IFile ? (IFile) storage : null;
    job.setRule( new BaseMapSchedulingRule( m_mapPanel, file ) );
    job.schedule();
  }

  /**
   * Use this method to set a new map-file to this map-view.
   */
  public void loadMap( final IProgressMonitor monitor, final IStorage storage ) throws CoreException
  {
    if( m_saving )
      return;

    monitor.beginTask( "Kartenvorlage laden", 2 );

    String partName = null;
    try
    {
      // prepare for exception
      setMapModell( null );

      final Gismapview gisview = GisTemplateHelper.loadGisMapView( storage );
      monitor.worked( 1 );

      final URL context;
      final IProject project;
      if( storage instanceof IFile )
      {
        setFile( (IFile) storage );
        context = ResourceUtilities.createURL( getFile() );
        project = ((IFile) storage).getProject();
      }
      else
      {
        context = null;
        project = null;
      }

      if( !m_disposed )
      {
        final GisTemplateMapModell mapModell = new GisTemplateMapModell( context, KalypsoCorePlugin.getDefault().getCoordinatesSystem(), project, m_selectionManager );
        mapModell.createFromTemplate( gisview );
        setMapModell( mapModell );

        final GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );
        m_mapPanel.setBoundingBox( env );
      }
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );

      setMapModell( null );

      setFile( null );

      throw new CoreException( status );
    }
    finally
    {
      monitor.done();

      final String fileName = getFile() != null ? FileUtilities.nameWithoutExtension( getFile().getName() ) : "<input not a file>";
      if( partName == null )
        partName = fileName;
      setCustomName( partName );
    }
  }

  public BaseMapSchedulingRule getSchedulingRule( )
  {
    return new BaseMapSchedulingRule( m_mapPanel, m_file );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  @Override
  protected void doSaveInternal( final IProgressMonitor monitor, final IFileEditorInput input ) throws CoreException
  {
    final IFile file = input.getFile();
    saveMap( monitor, file );
  }

  public void saveMap( final IProgressMonitor monitor, final IFile file ) throws CoreException
  {
    if( m_mapModell == null || m_saving )
      return;

    m_saving = true;
    try
    {
      monitor.beginTask( "Kartenvorlage speichern", 2000 );
      final GM_Envelope boundingBox = m_mapPanel.getBoundingBox();
      final String srsName = KalypsoCorePlugin.getDefault().getCoordinatesSystem();
      m_mapModell.createGismapTemplate( boundingBox, srsName, monitor, file );
    }
    catch( final CoreException e )
    {
      m_saving = false;
      throw e;
    }
    catch( final Throwable e )
    {
      m_saving = false;
      throw new CoreException( StatusUtilities.statusFromThrowable( e, "XML-Vorlagendatei konnte nicht erstellt werden." ) );
    }
    m_saving = false;

  }

  protected void setMapModell( final GisTemplateMapModell mapModell )
  {
    if( m_mapModell != null )
      m_mapModell.dispose();

    m_mapModell = mapModell;

    m_mapModellContextSwitcher.setMapModell( mapModell );

    final IWorkbench workbench = getSite().getWorkbenchWindow().getWorkbench();
    if( !workbench.isClosing() )
    {
      workbench.getDisplay().asyncExec( new Runnable()
      {
        @SuppressWarnings("synthetic-access")
        public void run( )
        {
          if( m_mapModell == null )
            setPartName( "<Keine Karte geladen>" );
          else
            setPartName( m_mapModell.getLabel( m_mapModell ) );
        }
      } );
    }

    if( m_mapPanel != null )
      m_mapPanel.setMapModell( m_mapModell );
  }

  protected GisTemplateMapModell getMapModell( )
  {
    return m_mapModell;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  public void setCustomName( final String name )
  {
    m_partName = name;
    final IWorkbench workbench = getSite().getWorkbenchWindow().getWorkbench();
    if( !workbench.isClosing() )
    {
      workbench.getDisplay().asyncExec( new Runnable()
      {
        @SuppressWarnings("synthetic-access")
        public void run( )
        {
          setPartName( m_partName );
        }
      } );
    }
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( IExportableObjectFactory.class.equals( adapter ) )
      return this;

    if( IContentOutlinePage.class.equals( adapter ) )
    {
      final GisMapOutlinePage page = new GisMapOutlinePage( getCommandTarget() );
      page.setMapPanel( getMapPanel() );
      return page;
    }

    if( adapter == IFile.class )
    {
      final IEditorInput input = getEditorInput();
      if( input instanceof IFileEditorInput )
        return ((IFileEditorInput) getEditorInput()).getFile();
    }

    if( adapter == MapPanel.class )
      return m_mapPanel;

    if( adapter == ModellEventProvider.class )
      return new MapPanelModellEventProvider( m_mapPanel );

    if( adapter == Control.class )
      return m_control;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createExportableObjects(org.apache.commons.configuration.Configuration)
   */
  public IExportableObject[] createExportableObjects( final Configuration conf )
  {
    return new IExportableObject[] { new ExportableMap( getMapPanel(), conf.getInt( ImageExportPage.CONF_IMAGE_WIDTH, 640 ), conf.getInt( ImageExportPage.CONF_IMAGE_HEIGHT, 480 ), conf.getString( ImageExportPage.CONF_IMAGE_FORMAT, "png" ) ) };
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createWizardPages(org.kalypso.metadoc.configuration.IPublishingConfiguration,
   *      ImageDescriptor)
   */
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration, final ImageDescriptor defaultImage )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoGisPlugin.getId(), "icons/util/img_props.gif" );
    final Rectangle bounds = getMapPanel().getBounds();
    final double width = bounds.width;
    final double height = bounds.height;
    final double actualWidthToHeigthRatio = width / height;
    final IWizardPage page = new ImageExportPage( configuration, "mapprops", "Export Optionen", imgDesc, actualWidthToHeigthRatio );

    return new IWizardPage[] { page };
  }

  public void setFile( final IFile file )
  {
    m_file = file;
    if( m_file != null )
    {
      try
      {
        m_file.refreshLocal( IResource.DEPTH_ONE, null );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
      m_file.getWorkspace().addResourceChangeListener( m_resourceChangeListener );
    }
  }

  public IFile getFile( )
  {
    return m_file;
  }

  @Override
  public void dispose( )
  {
    getSite().getPage().removePartListener( m_partListener );

    m_disposed = true;

    setMapModell( null );

    m_mapPanel.dispose();

    if( m_file != null )
      m_file.getWorkspace().removeResourceChangeListener( m_resourceChangeListener );
    super.dispose();
  }

  public void setStatusBarMessage( final String message )
  {
    m_statusBar.setText( message );
  }
}