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
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 */
public class ReloadAction extends AbstractRepositoryExplorerAction implements
    ISelectionChangedListener
{
  public ReloadAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Aktualisieren",
        ImageProvider.IMAGE_ZML_REPOSITORY_RELOAD,
        "Aktualisiert den aktuellen Repository" );

    explorer.addSelectionChangedListener( this );

    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run( )
  {
    final IRepository rep = getExplorer().isRepository(
        getExplorer().getSelection() );
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

  public void dispose( )
  {
    getExplorer().removeSelectionChangedListener( this );
  }
}