package org.kalypso.ui.repository.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Ein Repository hinzufügen.
 * 
 * @author schlienger
 */
public class RemoveRepositoryAction extends AbstractRepositoryExplorerAction implements ISelectionChangedListener
{
  public RemoveRepositoryAction( final RepositoryExplorerPart explorer )
  {
    super( explorer, "Repository entfernen", ImageProvider.IMAGE_ZML_REPOSITORY_REMOVE, "Entfernt ein Repository..." );

    explorer.addSelectionChangedListener( this );
    
    setEnabled( explorer.isRepository( explorer.getSelection() ) != null );
  }

  public void dispose()
  {
    getExplorer().removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final IRepository rep = getExplorer().isRepository( getExplorer().getSelection() );
    if( rep == null )
      return;

    if( !MessageDialog.openConfirm( getShell(), "Repository entfernen", "Repository '"
        + rep.toString() + "' wirklich entfernen?" ) )
      return;

    getExplorer().getRepositoryContainer().removeRepository( rep );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( getExplorer().isRepository( event.getSelection() ) != null );
  }
}