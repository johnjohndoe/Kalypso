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
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
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
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.internal.util.StatusLineContributionItem;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.ImageExportPage;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.ITemplateTheme;
import org.kalypso.ogc.gml.map.IMapPanelListener;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModellView;
import org.kalypso.ogc.gml.mapmodel.IMapPanelProvider;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ogc.gml.widgets.IWidget;
import org.kalypso.ogc.gml.widgets.IWidgetChangeListener;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.AbstractEditorPart;
import org.kalypso.ui.editor.mapeditor.views.ActionOptionsView;
import org.kalypso.ui.editor.mapeditor.views.IWidgetWithOptions;
import org.kalypso.ui.views.map.MapView;
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
public abstract class AbstractMapPart extends AbstractEditorPart implements IExportableObjectFactory, IMapPanelProvider, IMapPanelListener
{
  private final IFeatureSelectionManager m_selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

  private IWorkbenchPartSite m_site;

  private MapPanel m_mapPanel;

  private GisTemplateMapModell m_mapModell;

  private IMapModellView m_mapModellView;

  private Control m_control;

  @SuppressWarnings("restriction")
  private StatusLineContributionItem m_statusBar = new StatusLineContributionItem( "MapViewStatusBar", 100 );

  private boolean m_disposed = false;

  private String m_partName;

  private IFile m_file;

  private IPartListener m_partListener;

  private final IWidgetChangeListener m_wcl = new IWidgetChangeListener()
  {
    public void widgetChanged( final IWidget newWidget )
    {
      // the widget changed and there is something to show, so bring this
      // view to top
      try
      {
        final IWorkbenchPartSite site = getSite();
        if( site != null )
        {
          final IWorkbenchPage page = site.getPage();
          final IViewPart view = page.findView( ActionOptionsView.class.getName() );
          if( newWidget instanceof IWidgetWithOptions )
          {
            page.showView( ActionOptionsView.class.getName(), null, IWorkbenchPage.VIEW_VISIBLE );
          }
          else if( view != null )
          {
            page.hideView( view );
          }
        }
      }
      catch( final PartInitException e )
      {
        e.printStackTrace();
      }
    }
  };

  private IResourceChangeListener m_resourceChangeListener;

  private boolean m_saving;

  /**
   *
   */
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
    {
      try
      {
        startLoadJob( ((IStorageEditorInput) input).getStorage() );
      }
      catch( CoreException e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
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

  /**
   * @see org.eclipse.ui.IViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  public void init( final IViewSite site, final IMemento memento )
  {
    init( site );
  }

  private void initMapPanel( final IWorkbenchPartSite site )
  {
    m_statusBar.setText( "< Welcome to the Map >" );

    // both IViewSite und IEditorSite give access to actionBars
    final IActionBars actionBars = getActionBars( site );

    actionBars.getStatusLineManager().add( m_statusBar );
    actionBars.setGlobalActionHandler( ActionFactory.UNDO.getId(), m_commandTarget.undoAction );
    actionBars.setGlobalActionHandler( ActionFactory.REDO.getId(), m_commandTarget.redoAction );
    actionBars.updateActionBars();

    /* Register this view at the mapPanel. */

    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    m_mapPanel = new MapPanel( this, plugin.getCoordinatesSystem(), m_selectionManager );
    m_mapPanel.getWidgetManager().addWidgetChangeListener( m_wcl );
    m_mapPanel.addMapPanelListener( this );
    m_partListener = new PartAdapter()
    {

      /**
       * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter#partActivated(org.eclipse.ui.IWorkbenchPart)
       */
      @Override
      public void partActivated( final IWorkbenchPart part )
      {
        if( part instanceof GisMapOutlineView )
        {
          initMapModellView( (IMapModellView) part );
        }
      }

      /**
       * @see org.kalypso.contribs.eclipse.ui.partlistener.PartAdapter#partClosed(org.eclipse.ui.IWorkbenchPart)
       */
      @Override
      public void partClosed( final IWorkbenchPart part )
      {
        if( part instanceof GisMapOutlineView )
        {
          setMapModellView( null );
        }
      }

      private void initMapModellView( final IMapModellView mapModellView )
      {
        setMapModellView( mapModellView );
      }
    };
    site.getPage().addPartListener( m_partListener );

    m_resourceChangeListener = new IResourceChangeListener()
    {
      public void resourceChanged( final IResourceChangeEvent event )
      {
        if( m_saving )
        {
          return;
        }
        final IFile file = getFile();
        if( file == null )
        {
          return;
        }
        if( event.getType() != IResourceChangeEvent.POST_CHANGE )
        {
          return;
        }
        final IResourceDelta rootDelta = event.getDelta();
        final IResourceDelta fileDelta = rootDelta.findMember( file.getFullPath() );
        if( fileDelta == null )
        {
          return;
        }
        if( (fileDelta.getFlags() & IResourceDelta.CONTENT) != 0 )
        {
          startLoadJob( file );
        }
      }
    };
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_control = MapPartHelper.createMapPanelPartControl( parent, m_mapPanel, getSite() );
    getSite().setSelectionProvider( m_mapPanel );
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
   * Loads a map (i.e. a .gmv file) from a storage inside a {@link Job}.
   * <p>
   * The method starts a (user-)job, which loads the map.
   * </p>.
   * 
   * @param waitFor
   *          <code>true</code> if this method should return when the job has finished, if <code>false</code>
   *          returns immediately
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

      /**
       * @see org.eclipse.core.runtime.jobs.Job#belongsTo(java.lang.Object)
       */
      @Override
      public boolean belongsTo( final Object family )
      {
        return MapView.JOB_FAMILY.equals( family );
      }
    };
    job.setUser( true );
    job.schedule();
  }

  /**
   * Use this method to set a new map-file to this map-view.
   */
  public void loadMap( final IProgressMonitor monitor, final IStorage storage ) throws CoreException
  {
    monitor.beginTask( "Kartenvorlage laden", 2 );

    final IWorkbenchPartSite site = getSite();
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
        final GisTemplateMapModell mapModell = new GisTemplateMapModell( context, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), project, m_selectionManager );
        setMapModell( mapModell );
        mapModell.createFromTemplate( gisview );

        final GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );
        m_mapPanel.setBoundingBox( env );

        if( site != null )
        {

          final UIJob job = new UIJob( "" )
          {
            @Override
            public IStatus runInUIThread( final IProgressMonitor uiMonitor )
            {
              // Apply action filters to action bars and refresh them
              final IActionBars actionBars = getActionBars( site );
              GisTemplateHelper.applyActionFilters( actionBars, gisview );

              // must frce on toolBarManager, just update action bars is not enough
              final IToolBarManager toolBarManager = actionBars.getToolBarManager();
              toolBarManager.update( true );

              actionBars.updateActionBars();

              return Status.OK_STATUS;
            }

            /**
             * @see org.eclipse.core.runtime.jobs.Job#belongsTo(java.lang.Object)
             */
            @Override
            public boolean belongsTo( final Object family )
            {
              return MapView.JOB_FAMILY.equals( family );
            }
          };
          job.schedule();
        }
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
      final String partName = fileName;
      setCustomName( partName );
    }
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
    // if( m_mapModell == null )
    // return;
    //
    // m_saving = true;
    // ByteArrayInputStream bis = null;
    // try
    // {
    // monitor.beginTask( "Kartenvorlage speichern", 2000 );
    // final GM_Envelope boundingBox = getMapPanel().getBoundingBox();
    // final String srsName = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
    // final Gismapview modellTemplate = m_mapModell.createGismapTemplate( boundingBox, srsName );
    //
    // final ByteArrayOutputStream bos = new ByteArrayOutputStream();
    //
    // GisTemplateHelper.saveGisMapView( modellTemplate, bos, file.getCharset() );
    //
    // bis = new ByteArrayInputStream( bos.toByteArray() );
    // bos.close();
    // monitor.worked( 1000 );
    //
    // if( file.exists() )
    // file.setContents( bis, false, true, monitor );
    // else
    // file.create( bis, false, monitor );
    // }
    // catch( final CoreException e )
    // {
    // m_saving = false;
    // throw e;
    // }
    // catch( final Throwable e )
    // {
    // System.out.println( e.getLocalizedMessage() );
    // e.printStackTrace();
    // m_saving = false;
    // throw new CoreException( StatusUtilities.statusFromThrowable( e, "XML-Vorlagendatei konnte nicht erstellt
    // werden." ) );
    // }
    // finally
    // {
    // monitor.done();
    //
    // if( bis != null )
    // try
    // {
    // bis.close();
    // }
    // catch( IOException e1 )
    // {
    // // never occurs with a byteinputstream
    // e1.printStackTrace();
    // }
    // }
    // m_saving = false;
  }

  public void saveMap( final IProgressMonitor monitor, final IFile file ) throws CoreException
  {
    if( m_mapModell == null )
      return;

    m_saving = true;
    ByteArrayInputStream bis = null;
    try
    {
      monitor.beginTask( "Kartenvorlage speichern", 2000 );
      final GM_Envelope boundingBox = m_mapPanel.getBoundingBox();
      final String srsName = KalypsoGisPlugin.getDefault().getCoordinatesSystem().getName();
      final Gismapview modellTemplate = m_mapModell.createGismapTemplate( boundingBox, srsName );

      final ByteArrayOutputStream bos = new ByteArrayOutputStream();

      GisTemplateHelper.saveGisMapView( modellTemplate, bos, file.getCharset() );

      bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      monitor.worked( 1000 );

      if( file.exists() )
        file.setContents( bis, false, true, monitor );
      else
        file.create( bis, false, monitor );
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
    finally
    {
      monitor.done();

      if( bis != null )
        try
        {
          bis.close();
        }
        catch( IOException e1 )
        {
          // never occurs with a byteinputstream
          e1.printStackTrace();
        }
    }
    m_saving = false;
  }

  public void saveTheme( final ITemplateTheme theme, final IProgressMonitor monitor ) throws CoreException
  {
    m_mapModell.saveTheme( theme, monitor );
  }

  protected void setMapModell( final GisTemplateMapModell mapModell )
  {
    // dispose old one
    if( m_mapModell != null )
    {
      m_mapModell.dispose();
    }

    m_mapModell = mapModell;

    if( m_mapPanel != null )
    {
      m_mapPanel.setMapModell( m_mapModell );
    }

    if( m_mapModellView != null )
    {
      m_mapModellView.setMapModell( m_mapModell );
    }
  }

  /**
   * @see org.kalypso.ogc.gml.mapmodel.IMapPanelProvider#getMapPanel()
   */
  public MapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  public String getCustomName( )
  {
    return m_partName;
  }

  public void setCustomName( final String name )
  {
    m_partName = name;
    getSite().getShell().getDisplay().asyncExec( new Runnable()
    {
      @SuppressWarnings("synthetic-access")
      public void run( )
      {
        setPartName( m_partName );
      }
    } );
  }

  public void setMapModellView( final IMapModellView mapModellView )
  {
    m_mapModellView = mapModellView;
    if( m_mapModellView != null )
    {
      m_mapModellView.setMapModell( m_mapModell );
    }
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( IMapModellView.class.equals( adapter ) )
    {
      return m_mapModellView;
    }

    if( IExportableObjectFactory.class.equals( adapter ) )
      return this;

    if( adapter == IFile.class )
    {
      final IEditorInput input = getEditorInput();
      if( input instanceof IFileEditorInput )
        return ((IFileEditorInput) getEditorInput()).getFile();
    }

    if( adapter == MapPanel.class )
      return m_mapPanel;

    if( adapter == ModellEventProvider.class )
      return m_mapPanel;

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
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration, ImageDescriptor defaultImage )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoGisPlugin.getId(), "icons/util/img_props.gif" );
    Rectangle bounds = getMapPanel().getBounds();
    double width = bounds.width;
    double height = bounds.height;
    final double actualWidthToHeigthRatio = width / height;
    final IWizardPage page = new ImageExportPage( configuration, "mapprops", "Export Optionen", imgDesc, actualWidthToHeigthRatio );

    return new IWizardPage[] { page };
  }

  /**
   * @see org.kalypso.ogc.gml.map.IMapPanelListener#onMessageChanged(java.lang.String)
   */
  public void onMessageChanged( final String message )
  {
    Display display = getSite().getShell().getDisplay();

    /* Update the text. */
    display.asyncExec( new Runnable()
    {

      @SuppressWarnings("restriction")
      public void run( )
      {
        m_statusBar.setText( message );
      }
    } );
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
    m_disposed = true;

    setMapModell( null );

    m_mapPanel.getWidgetManager().removeWidgetChangeListener( m_wcl );
    m_mapPanel.dispose();

    getSite().getPage().removePartListener( m_partListener );

    if( m_file != null )
    {
      m_file.getWorkspace().removeResourceChangeListener( m_resourceChangeListener );
    }
    super.dispose();
  }
}