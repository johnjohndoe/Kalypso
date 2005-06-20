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
package org.kalypso.ui.repository.actions;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.repository.view.ObservationChooser;

/**
 * @author schlienger
 */
public class ReloadAction extends AbstractRepositoryExplorerAction implements ISelectionChangedListener
{
  public ReloadAction( final ObservationChooser explorer )
  {
    super( explorer, "Aktualisieren", ImageProvider.IMAGE_ZML_REPOSITORY_RELOAD,
        "Aktualisiert den aktuellen Repository" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IRepository rep = getExplorer().isRepository( getExplorer().getSelection() );
    if( rep == null )
      return;

    final Job reloadJob = new Job( "Aktualisieren" )
    {
      protected IStatus run( IProgressMonitor monitor )
      {
        monitor.beginTask( "Repository aktualisieren", 2 );

        // Important: clear the cache
        ObservationCache.clearCache();

        try
        {
          rep.reload();

          monitor.worked( 1 );

          // trick: direct call to update view
          getExplorer().onRepositoryContainerChanged();

          monitor.worked( 1 );

          return Status.OK_STATUS;
        }
        catch( RepositoryException e )
        {
          return new Status( IStatus.WARNING, KalypsoGisPlugin.getId(), 0, "Fehler während der Aktualisierung", e );
        }
        finally
        {
          monitor.done();
        }
      }
    };

    reloadJob.schedule();
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }
}