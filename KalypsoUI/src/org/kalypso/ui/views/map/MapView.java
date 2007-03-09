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

import org.eclipse.core.resources.IFile;
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
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ui.editor.mapeditor.AbstractMapPart;

/**
 * <p>
 * View on a {@link org.kalypso.ogc.gml.mapmodel.IMapModell}.
 * </p>
 * <p>
 * Shows a map of all themes. The sources of the themes can be edited.
 * </p>
 * 
 * @author Stefan Kurzbach, belger
 */
public class MapView extends AbstractMapPart implements IViewPart
{
  private static final String MEMENTO_FILE = "file";

  private static final String MEMENTO_PARTNAME = "partName";

  public static final String ID = "org.kalypso.ui.views.mapView";

//  public static final String JOB_FAMILY = "mapViewJobFamily";

  private static final String SAVE_MAP_ON_CLOSE = "saveMapOnClose";

  /**
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl( final Composite parent )
  {
    super.createPartControl( parent );

    final IFile file = getFile();
    if( file != null )
    {
      startLoadJob( file );
    }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @SuppressWarnings("restriction")
  @Override
  public void init( final IViewSite site, final IMemento memento )
  {
    super.init( site, memento );

    if( memento != null )
    {
      final String fullPath = memento.getString( MEMENTO_FILE );
      if( fullPath != null )
      {
        final IPath path = Path.fromPortableString( fullPath );
        setFile( ResourcesPlugin.getWorkspace().getRoot().getFile( path ) );
      }
      final String partName = memento.getString( MEMENTO_PARTNAME );
      setCustomName( partName );
    }
  }

  /**
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  public void saveState( final IMemento memento )
  {
    final IFile file = getFile();
    if( file != null )
    {
      final IPath fullPath = file.getFullPath();
      if( fullPath != null )
        memento.putString( MEMENTO_FILE, fullPath.toPortableString() );
    }

    final String customName = getCustomName();
    if( customName != null )
    {
      memento.putString( MEMENTO_PARTNAME, customName );
    }
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#dispose()
   */
  @Override
  public void dispose( )
  {
    final String saveOnCloseString = getConfigurationElement().getAttribute( SAVE_MAP_ON_CLOSE );
    if( "true".equals( saveOnCloseString ) )
    {
      startSaveJob();
    }
    super.dispose();
  }

  /**
   * @see org.kalypso.ui.editor.mapeditor.AbstractMapPart#startLoadJob(org.eclipse.core.resources.IStorage)
   */
  @Override
  public void startLoadJob( final IStorage storage )
  {
    final IFile file = getFile();
    if( file != null && !file.equals( storage))
    {
      startSaveJob();
    }
    super.startLoadJob( storage );
  }

  public void startSaveJob( )
  {
    final IFile file = getFile();
    final Job disposeJob = new Job( "Saving map state..." )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        try
        {
          saveMap( monitor, file );
        }
        catch( final CoreException e )
        {
          return StatusUtilities.statusFromThrowable( e );
        }
        return Status.OK_STATUS;
      }
//
//      /**
//       * @see org.eclipse.core.runtime.jobs.Job#belongsTo(java.lang.Object)
//       */
//      @Override
//      public boolean belongsTo( final Object family )
//      {
//        return MapView.JOB_FAMILY.equals( family );
//      }
    };
    disposeJob.setRule( file );
    disposeJob.setUser( true );
    disposeJob.schedule();
  }
}