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
package org.kalypso.ui.views.map;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.contexts.IContextService;
import org.eclipse.ui.internal.util.StatusLineContributionItem;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanelListener;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.mapeditor.GisMapOutlinePage;
import org.kalypso.ui.editor.mapeditor.MapPartHelper;
import org.kalypso.util.command.JobExclusiveCommandTarget;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * <p>
 * Eclipse-Editor zum editieren der GML-Gis-Templates.
 * </p>
 * <p>
 * Zeigt das ganze als Kartendarstellug, die einzelnen Datenquellen k?nnen potentiell editiert werden
 * </p>
 * <p>
 * Implementiert {@link org.kalypso.commons.command.ICommandManager}für die Undo und Redo Action. Gibt alles an den
 * DefaultCommandManager weiter, es wird zusätzlich eine Aktualisierung der View bei jeder Aktion durchgef?hrt
 * </p>
 * 
 * @author belger
 */
public class MapView extends ViewPart implements ICommandTarget, IMapPanelListener
{
  private static final String MEMENTO_FILE = "file";

  private static final String MEMENTO_PARTNAME = "partName";

  public static final String ID = "org.kalypso.ui.views.mapView";

  public static final String JOB_FAMILY = "mapViewJobFamily";

  private final Runnable m_dirtyRunnable = new Runnable()
  {
    public void run( )
    {
      // getEditorSite().getShell().getDisplay().asyncExec( new Runnable()
      // {
      // public void run( )
      // {
      // fireDirty();
      // }
      // } );

      // TODO: set dirty state of this view
    }
  };

  protected JobExclusiveCommandTarget m_commandTarget = new JobExclusiveCommandTarget( new DefaultCommandManager(), m_dirtyRunnable );

  private MapPanel m_mapPanel;

  private GisTemplateMapModell m_mapModell;

  private final IFeatureSelectionManager m_selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

  private boolean m_disposed = false;

  private Control m_control;

  IFile m_file;

  private String m_partName;

  @SuppressWarnings("restriction")
  StatusLineContributionItem m_statusBar = new StatusLineContributionItem( "MapViewStatusBar", 100 );

  private GisMapOutlinePage m_outlinePage = null;

  public MapView( )
  {
    m_partName = null;
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_control = MapPartHelper.createMapPanelPartControl( parent, m_mapPanel, getSite() );

    if( m_file != null )
    {
      final IFile storage = m_file;
      loadMap( storage );
    }

    getSite().setSelectionProvider( m_mapPanel );
    m_mapPanel.fireSelectionChanged();
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
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @SuppressWarnings("restriction")
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );

    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    final IContextService service = (IContextService) site.getWorkbenchWindow().getWorkbench().getService( IContextService.class );
    m_mapPanel = new MapPanel( this, plugin.getCoordinatesSystem(), m_selectionManager, service );

    /* Register this view at the mapPanel. */
    m_mapPanel.addMapPanelListener( this );

    m_statusBar.setText( "< Welcome to the MapView. >" );

    final IActionBars actionBars = site.getActionBars();
    actionBars.getStatusLineManager().add( m_statusBar );
    actionBars.updateActionBars();

    if( memento != null )
    {
      final String fullPath = memento.getString( MEMENTO_FILE );
      if( fullPath != null )
      {
        final IPath path = Path.fromPortableString( fullPath );
        m_file = ResourcesPlugin.getWorkspace().getRoot().getFile( path );
      }
      final String partName = memento.getString( MEMENTO_PARTNAME );
      m_partName = partName;
    }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
    super.saveState( memento );

    if( m_file != null )
    {
      final IPath fullPath = m_file.getFullPath();
      if( fullPath != null )
        memento.putString( MEMENTO_FILE, fullPath.toPortableString() );
    }

    if( m_partName != null )
    {
      memento.putString( MEMENTO_PARTNAME, m_partName );
    }

    final Job disposeJob = new Job( "Saving map state..." )
    {

      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          doSaveInternal( monitor, m_file );
        }
        catch( final CoreException e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
        return Status.OK_STATUS;
      }
    };
    disposeJob.setUser( true );
    disposeJob.schedule();
  }

  /**
   * Loads a map (i.e. a .gmv file) from a storage insdie a {@link Job}.
   * <p>
   * The method starts a (user-)job, which loads the map.
   * </p>.
   * 
   * @param waitFor
   *          <code>true</code> if this method should return when the job has finished, if <code>false</code>
   *          returns immediately
   */
  public Job loadMap( final IFile storage )
  {
    final Job job = new Job( "Karte laden: " + storage.getName() )
    {
      @Override
      public IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          loadMap( storage, monitor );
        }
        catch( final CoreException e )
        {
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
    return job;
  }

  /**
   * Use this method to set a new map-file to this map-view.
   */
  void loadMap( final IFile storage, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Kartenvorlage laden", 2 );

    String fileName = null;
    try
    {
      // prepare for exception
      setMapModell( null );
      m_file = storage;
      fileName = FileUtilities.nameWithoutExtension( storage.getName() );

      final Gismapview gisview = GisTemplateHelper.loadGisMapView( storage );
      monitor.worked( 1 );

      final URL context;
      final IProject project;
      context = ResourceUtilities.createURL( storage );
      project = storage.getProject();

      if( !m_disposed )
      {
        final GisTemplateMapModell mapModell = new GisTemplateMapModell( context, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), project, m_selectionManager );
        setMapModell( mapModell );
        mapModell.createFromTemplate( gisview );

        final GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );
        m_mapPanel.setBoundingBox( env );

        final UIJob job = new UIJob( "" )
        {
          @Override
          public IStatus runInUIThread( final IProgressMonitor uiMonitor )
          {
            // Apply action filters to action bars and refresh them
            final IActionBars actionBars = getViewSite().getActionBars();
            GisTemplateHelper.applyActionFilters( actionBars, gisview );

            // must frce on toolBarManager, just update action bars is not enough
            final IToolBarManager toolBarManager = actionBars.getToolBarManager();
            toolBarManager.update( true );

            actionBars.updateActionBars();

            return Status.OK_STATUS;
          }
        };
        job.schedule();
      }
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );

      setMapModell( null );
      m_file = null;

      fileName = null;

      throw new CoreException( status );
    }
    finally
    {
      monitor.done();

// TODO: call setCustomName here
      final String partName = m_partName == null ? fileName : m_partName;
      // must set part name in ui thread
      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        @SuppressWarnings("synthetic-access")
        public void run( )
        {
          setPartName( partName );
        }
      } );
    }
  }

  void doSaveInternal( final IProgressMonitor monitor, final IFile file ) throws CoreException
  {
    if( m_mapModell == null )
      return;

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
      throw e;
    }
    catch( final Throwable e )
    {
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
  }

  public void saveTheme( final IKalypsoFeatureTheme theme, final IProgressMonitor monitor ) throws CoreException
  {
    m_mapModell.saveTheme( theme, monitor );
  }

  private void setMapModell( final GisTemplateMapModell mapModell )
  {
    // dispose old one
    if( m_mapModell != null )
      m_mapModell.dispose();

    m_mapModell = mapModell;

    m_mapPanel.setMapModell( m_mapModell );

    if( m_outlinePage != null )
      m_outlinePage.setMapModell( m_mapModell );
  }

  @Override
  public void dispose( )
  {
    m_disposed = true;

    setMapModell( null );

    /* Remove this view from the mapPanel. */
    m_mapPanel.removeMapPanelListener( MapView.this );
    m_mapPanel.dispose();

    if( m_outlinePage != null )
      m_outlinePage.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.commons.command.ICommandTarget#postCommand(org.kalypso.commons.command.ICommand,
   *      java.lang.Runnable)
   */
  public void postCommand( final ICommand command, final Runnable runnable )
  {
    m_commandTarget.postCommand( command, runnable );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    if( IContentOutlinePage.class.equals( adapter ) )
    {
      if( m_outlinePage == null )
      {
        m_outlinePage = new GisMapOutlinePage( m_commandTarget );
        m_outlinePage.setMapModell( m_mapModell );
      }

      return m_outlinePage;
    }

    if( adapter == Control.class )
      return m_control;

    if( adapter == MapPanel.class )
      return m_mapPanel;

    return super.getAdapter( adapter );
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
}