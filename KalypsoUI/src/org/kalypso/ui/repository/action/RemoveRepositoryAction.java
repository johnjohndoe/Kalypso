package org.kalypso.ui.repository.action;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.repository.IRepository;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * Ein Repository hinzufügen.
 * 
 * @author schlienger
 *  
 */
public class RemoveRepositoryAction extends FullAction implements ISelectionChangedListener
{
  private final Shell m_shell;

  private RepositoryExplorerPart m_explorer;

  public RemoveRepositoryAction( final Shell shell, final RepositoryExplorerPart explorer )
  {
    super( "Repository entfernen", null, "Entfernt ein Repository..." );

    m_explorer = explorer;
    m_shell = shell;

    m_explorer.addSelectionChangedListener( this );
    
    setEnabled( isRepository( m_explorer.getSelection() ) != null );
  }

  public void dispose()
  {
    m_explorer.removeSelectionChangedListener( this );
  }

  /**
   * @see org.eclipse.jface.action.IAction#run()
   */
  public void run()
  {
    final IRepository rep = isRepository( m_explorer.getSelection() );
    if( rep == null )
      return;

    if( !MessageDialog.openConfirm( m_shell, "Repository entfernen", "Repository '"
        + rep.toString() + "' wirklich entfernen?" ) )
      return;

    m_explorer.getRepositoryContainer().removeRepository( rep );
  }

  private IRepository isRepository( final ISelection selection )
  {
    final IStructuredSelection sel = (IStructuredSelection)selection;
    if( sel.isEmpty() )
      return null;

    final Object element = sel.getFirstElement();
    if( !( element instanceof IRepository ) )
      return null;

    return (IRepository)element;
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    setEnabled( isRepository( event.getSelection() ) != null );
  }
}