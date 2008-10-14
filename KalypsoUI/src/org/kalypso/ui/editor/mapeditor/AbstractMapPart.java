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
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.StatusLineContributionItem;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.i18n.Messages;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.ImageExportPage;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.BaseMapSchedulingRule;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.MapPanelSourceProvider;
import org.kalypso.ogc.gml.map.listeners.IMapPanelListener;
import org.kalypso.ogc.gml.map.listeners.MapPanelAdapter;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.KalypsoDeegreePlugin;
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
public abstract class AbstractMapPart extends AbstractEditorPart implements IExportableObjectFactory, IMapPanelProvider
{
  // TODO: we probably should move this elsewhere
  public static final String MAP_COMMAND_CATEGORY = "org.kalypso.ogc.gml.map.category"; //$NON-NLS-1$

  private final IFeatureSelectionManager m_selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

  public final StatusLineContributionItem m_statusBar = new StatusLineContributionItem( "MapViewStatusBar", 100 ); //$NON-NLS-1$

  private IWorkbenchPartSite m_site;

  private IMapPanel m_mapPanel;

  private GisTemplateMapModell m_mapModell;

  private Form m_control;

  private boolean m_disposed = false;

  private String m_partName;

  private IFile m_file;

  private IResourceChangeListener m_resourceChangeListener;

  protected boolean m_saving;

  // TODO: this would also probably better made by a general map context: a general status line item that looks
  // for map context changes; it then always gets the current message from the map
  private final IMapPanelListener m_mapPanelListener = new MapPanelAdapter()
  {
    /**
     * @see org.kalypso.ogc.gml.map.MapPanelAdapter#onMessageChanged(org.kalypso.ogc.gml.map.MapPanel, java.lang.String)
     */
    @Override
    public void onMessageChanged( final IMapPanel source, final String message )
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

  private MapPanelSourceProvider m_mapSourceProvider;

  private GM_Envelope m_initialEnv;

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
    final JobExclusiveCommandTarget commandTarget = getCommandTarget();

    m_statusBar.setText( Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.4" ) ); //$NON-NLS-1$

    // both IViewSite und IEditorSite give access to actionBars
    final IActionBars actionBars = getActionBars( site );
    actionBars.getStatusLineManager().add( m_statusBar );
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), commandTarget.redoAction );
    actionBars.updateActionBars();

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

        switch( fileDelta.getKind() )
        {
          case IResourceDelta.REMOVED:
            // Unhook from that file; else we still try to save it even if it is already deleted
            setFile( null );
            return;
        }

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

    m_control = MapPartHelper.createMapForm( parent );
    m_mapPanel = MapPartHelper.createMapPanelInForm( m_control, this, m_selectionManager );
    setMapModell( m_mapModell, m_initialEnv );
    m_mapPanel.addMapPanelListener( m_mapPanelListener );
    m_mapSourceProvider = new MapPanelSourceProvider( site, m_mapPanel );

    if( m_mapModell == null )
      m_mapPanel.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.1" ), null ) );//$NON-NLS-1$;

    // HACK: at the moment views never have a menu... maybe we could get the information,
    // if a context menu is desired from the defining extension
    if( this instanceof IEditorPart )
    {
      final MenuManager contextMenu = MapPartHelper.createMapContextMenu( m_control.getBody(), m_mapPanel, site );
      site.registerContextMenu( contextMenu, m_mapPanel );
    }

    site.setSelectionProvider( m_mapPanel );

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
   * </p>
   * .
   * 
   * @param waitFor
   *          <code>true</code> if this method should return when the job has finished, if <code>false</code> returns
   *          immediately
   */
  public void startLoadJob( final IStorage storage )
  {
    final Job job = new Job( Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.5" ) + storage.getName() ) //$NON-NLS-1$
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

    monitor.beginTask( Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.6" ), 2 ); //$NON-NLS-1$

    try
    {
      // prepare for exception
      setMapModell( null, null );

      /* "Loading map..." */
      showBusy( true );

      if( m_mapPanel != null )
        m_mapPanel.setStatus( StatusUtilities.createStatus( IStatus.INFO, Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.1" ), null ) );//$NON-NLS-1$;

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
        final GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );
        final GisTemplateMapModell mapModell = new GisTemplateMapModell( context, KalypsoDeegreePlugin.getDefault().getCoordinateSystem(), project, m_selectionManager );
        mapModell.createFromTemplate( gisview );
        setMapModell( mapModell, env );
      }
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );

      setMapModell( null, null );
      setFile( null );

      m_mapPanel.setStatus( new MultiStatus( KalypsoGisPlugin.getId(), -1, new IStatus[] { status }, Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.2" ), null ) );//$NON-NLS-1$;

      throw new CoreException( status );
    }
    finally
    {
      monitor.done();

      final IFile file = getFile();
      if( m_partName == null )
      {
        final String fileName = file != null ? FileUtilities.nameWithoutExtension( getFile().getName() ) : Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.7" ); //$NON-NLS-1$
        setCustomName( fileName );
      }
      if( file != null )
        setTitleToolTip( file.getFullPath().toPortableString() );

      // At the moment, we stop after the .gmt file has loaded. One day we may change this to wait until
      // all themes have finished loading, but this might be a little bit tricky.
      showBusy( false );
    }
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#showBusy(boolean)
   */
  @Override
  public void showBusy( final boolean busy )
  {
    super.showBusy( busy );

    final Form control = m_control;
    if( control != null && !control.isDisposed() )
    {
      control.getDisplay().syncExec( new Runnable()
      {
        public void run( )
        {
          if( !control.isDisposed() )
            control.setBusy( false );
        }
      } );
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
      monitor.beginTask( Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.8" ), 2000 ); //$NON-NLS-1$
      final GM_Envelope boundingBox = m_mapPanel.getBoundingBox();
      final String srsName = KalypsoDeegreePlugin.getDefault().getCoordinateSystem();
      m_mapModell.saveGismapTemplate( boundingBox, srsName, monitor, file );
    }
    catch( final CoreException e )
    {
      m_saving = false;
      throw e;
    }
    catch( final Throwable e )
    {
      m_saving = false;
      throw new CoreException( StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.9" ) ) ); //$NON-NLS-1$
    }
    m_saving = false;

  }

  protected void setMapModell( final GisTemplateMapModell mapModell, final GM_Envelope env )
  {
    if( m_mapModell != null )
      m_mapModell.dispose();

    m_mapModell = mapModell;
    m_initialEnv = env; // only needed, if mapPanel not yet available

    final String partName;
    if( m_mapModell == null )
      partName = Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.11" ); //$NON-NLS-1$
    else
      partName = m_mapModell.getLabel( m_mapModell );
    setCustomName( partName );
//
// final IWorkbench workbench = getSite().getWorkbenchWindow().getWorkbench();
// if( !workbench.isClosing() )
// {
// workbench.getDisplay().asyncExec( new Runnable()
// {
// @SuppressWarnings("synthetic-access")
// public void run( )
// {
// setPartName( partName );
// }
// } );
// }

    if( m_mapPanel != null )
    {
      m_mapPanel.setMapModell( m_mapModell );
      if( env != null )
        m_mapPanel.setBoundingBox( env );
    }
  }

  protected GisTemplateMapModell getMapModell( )
  {
    return m_mapModell;
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapPanelProvider#getMapPanel()
   */
  public IMapPanel getMapPanel( )
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
  @SuppressWarnings("unchecked")
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

    if( adapter == IMapPanel.class )
      return m_mapPanel;

    if( adapter == ModellEventProvider.class )
      return new MapPanelModellEventProvider( m_mapPanel );

    if( adapter == Form.class )
      return m_control;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createExportableObjects(org.apache.commons.configuration.Configuration)
   */
  public IExportableObject[] createExportableObjects( final Configuration conf )
  {
    return new IExportableObject[] { new ExportableMap( getMapPanel(), conf.getInt( ImageExportPage.CONF_IMAGE_WIDTH, 640 ), conf.getInt( ImageExportPage.CONF_IMAGE_HEIGHT, 480 ), conf.getString( ImageExportPage.CONF_IMAGE_FORMAT, "png" ) ) }; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createWizardPages(org.kalypso.metadoc.configuration.IPublishingConfiguration,
   *      ImageDescriptor)
   */
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration, final ImageDescriptor defaultImage )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoGisPlugin.getId(), "icons/util/img_props.gif" ); //$NON-NLS-1$
    final Rectangle bounds = getMapPanel().getScreenBounds();
    final double width = bounds.width;
    final double height = bounds.height;
    final double actualWidthToHeigthRatio = width / height;
    final IWizardPage page = new ImageExportPage( configuration, "mapprops", Messages.getString( "org.kalypso.ui.editor.mapeditor.AbstractMapPart.16" ), imgDesc, actualWidthToHeigthRatio ); //$NON-NLS-1$ //$NON-NLS-2$

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
    m_mapSourceProvider.dispose();

    m_disposed = true;

    setMapModell( null, null );

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