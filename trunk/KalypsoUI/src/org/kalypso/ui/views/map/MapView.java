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

import java.net.URL;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.ViewPart;
import org.kalypso.commons.command.DefaultCommandManager;
import org.kalypso.commons.command.ICommand;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.ogc.gml.GisTemplateMapModell;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.ui.KalypsoGisPlugin;
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
public class MapView extends ViewPart implements ICommandTarget
{
  public static final String ID = MapView.class.getName();

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

  private final MapPanel m_mapPanel;

  private GisTemplateMapModell m_mapModell;

  private final IFeatureSelectionManager m_selectionManager = KalypsoCorePlugin.getDefault().getSelectionManager();

  private boolean m_disposed = false;

  private Control m_control;

  private IStorage m_storage;

  public MapView( )
  {
    final KalypsoGisPlugin plugin = KalypsoGisPlugin.getDefault();
    m_mapPanel = new MapPanel( this, plugin.getCoordinatesSystem(), m_selectionManager );
  }

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    m_control = MapPartHelper.createMapPanelPartControl( parent, m_mapPanel, getSite() );

    if( m_storage != null )
    {
      final IStorage storage = m_storage;
      loadMap( storage );
    }
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
  @Override
  public void init( final IViewSite site, final IMemento memento ) throws PartInitException
  {
    super.init( site, memento );
    if( memento == null )
      return;

    final String fullPath = memento.getString( "file" );
    if( fullPath != null )
    {
      final IPath path = Path.fromPortableString( fullPath );
      m_storage = ResourcesPlugin.getWorkspace().getRoot().getFile( path );
    }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState( final IMemento memento )
  {
    super.saveState( memento );

    final IPath fullPath = m_storage.getFullPath();
    if( fullPath != null )
      memento.putString( "file", fullPath.toPortableString() );
  }

  /**
   * Loads a map (i.e. a .gmv file) from a storage insdie a {@link Job}.
   * <p>
   * The method returns immediately, but starts a (user-)job, which loads the map.
   * </p>.
   */
  public void loadMap( final IStorage storage )
  {
    final Job job = new Job( "Karte laden: " + storage.getName() )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
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
    };
    job.setUser( true );
    job.schedule();
  }

  public void loadMap( final IStorage storage, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Kartenvorlage laden", 2 );

    String partName = null;
    try
    {
      // prepare for exception
      setMapModell( null );
      m_storage = storage;
      partName = FileUtilities.nameWithoutExtension( storage.getName() );

      final Gismapview gisview = GisTemplateHelper.loadGisMapView( storage );
      monitor.worked( 1 );

      final URL context;
      final IProject project;
      if( storage instanceof IFile )
      {
        context = ResourceUtilities.createURL( (IResource) storage );
        project = ((IFile) storage).getProject();
      }
      else
      {
        context = null;
        project = null;
      }

      if( !m_disposed )
      {
        final GisTemplateMapModell mapModell = new GisTemplateMapModell( gisview, context, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), project, m_selectionManager );
        setMapModell( mapModell );

        final GM_Envelope env = GisTemplateHelper.getBoundingBox( gisview );

        m_mapPanel.setBoundingBox( env );
      }
    }
    catch( final Throwable e )
    {
      final IStatus status = StatusUtilities.statusFromThrowable( e );

      setMapModell( null );
      m_storage = null;

      partName = null;

      throw new CoreException( status );
    }
    finally
    {
      monitor.done();

      final String name = partName;
      // must set part name in ui thread
      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        @SuppressWarnings("synthetic-access")
        public void run( )
        {
          setPartName( name );
        }
      } );
    }
  }

  private void setMapModell( final GisTemplateMapModell mapModell )
  {
    // dispose old one
    if( m_mapModell != null )
      m_mapModell.dispose();

    m_mapModell = mapModell;

    m_mapPanel.setMapModell( m_mapModell );
  }

  @Override
  public void dispose( )
  {
    m_disposed = true;

    setMapModell( null );

    m_mapPanel.dispose();

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
}