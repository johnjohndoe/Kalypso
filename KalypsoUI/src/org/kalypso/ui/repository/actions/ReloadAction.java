package org.kalypso.ui.repository.actions;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.RepositoryException;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.repository.view.RepositoryExplorerPart;

/**
 * @author schlienger
 */
public class ReloadAction extends FullAction implements ISelectionChangedListener
{
  private final RepositoryExplorerPart m_explorer;
  private final Shell m_shell;

  public ReloadAction( final Shell shell, final RepositoryExplorerPart explorer  )
  {
    super( "Aktualisieren", ImageProvider.IMAGE_ZML_REPOSITORY_RELOAD, "Aktualisiert den aktuellen Repository" );
   
    m_shell = shell;
    
    m_explorer = explorer;
    m_explorer.addSelectionChangedListener( this );

    setEnabled( m_explorer.isRepository( m_explorer.getSelection() ) != null );
  }

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    final IRepository rep = m_explorer.isRepository( m_explorer.getSelection() );
    if( rep == null )
      return;
    
    try
    {
      rep.reload();
    }
    catch( RepositoryException e )
    {
      MessageDialog.openError( m_shell, "Fehler während Aktualisierung", e.getLocalizedMessage() );
    }
  }
  
  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    setEnabled( m_explorer.isRepository( event.getSelection() ) != null );
  }

  public void dispose()
  {
    m_explorer.removeSelectionChangedListener( this );
  }
}
