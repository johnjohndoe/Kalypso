package org.kalypso.ui.repository.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 */
public class ReloadAction extends AbstractRepositoryExplorerAction implements ISelectionChangedListener
{
  public ReloadAction( final RepositoryExplorerPart explorer  )
  {
    super( explorer, "Aktualisieren", ImageProvider.IMAGE_ZML_REPOSITORY_RELOAD, "Aktualisiert den aktuellen Repository" );
   
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
    
    ObservationCache.clear();
    
    try
    {
      rep.reload();
      
      // trick: direct call to update view
      getExplorer().onRepositoryContainerChanged();
    }
    catch( RepositoryException e )
    {
      MessageDialog.openError( getShell(), "Fehler während Aktualisierung", e.getLocalizedMessage() );
    }
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
