package org.kalypso.editor.mapeditor.commands;

import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.xml.types.ILayerlistProvider;

/**
 * @author belger
 */
public class RemoveLayerAction extends FullAction implements ISelectionChangedListener
{
  private final ILayerlistProvider m_listProvider;
  private final ISelectionProvider m_selectionProvider;
  private final ICommandManager m_commandManager;
  
  public RemoveLayerAction( final ICommandManager commandManager, final ILayerlistProvider listProvider, final ISelectionProvider selectionProvider )
  {
      super(  "Remove", ImageProvider.IMAGE_MAPVIEW_OUTLINE_REMOVE, "Thema entfernen" );

      m_selectionProvider = selectionProvider;
      m_listProvider = listProvider;
      m_commandManager = commandManager;
      
      m_selectionProvider.addSelectionChangedListener( this );
      
      refresh();
  }
  
  public void dispose( )
  {
    m_selectionProvider.removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    m_commandManager.postCommand( new RemoveLayerCommand( m_listProvider, m_selectionProvider ) );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  private void refresh()
  {
    final IStructuredSelection s = (IStructuredSelection)m_selectionProvider.getSelection();

    setEnabled( !s.isEmpty() );
  }
}
