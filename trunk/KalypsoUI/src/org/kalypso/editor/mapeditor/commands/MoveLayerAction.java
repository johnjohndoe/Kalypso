package org.kalypso.editor.mapeditor.commands;

import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.xml.types.ILayerlistProvider;

/**
 * @author belger
 */
public class MoveLayerAction extends FullAction implements ISelectionChangedListener
{
  private final ICommandManager m_commandManager;
  private final ILayerlistProvider m_listProvider;
  private final ISelectionProvider m_selectionProvider;
  private final int m_shift;

  public MoveLayerAction( final String text, final ImageDescriptor image, final String tooltipText, final int shift, final ILayerlistProvider listProvider, final ISelectionProvider selectionProvider, final ICommandManager commandManager )
  {
    super( text, image, tooltipText );
    
    m_shift = shift;
    
    m_commandManager = commandManager;
    m_listProvider = listProvider;
    m_selectionProvider = selectionProvider;
    
    m_selectionProvider.addSelectionChangedListener( this );
    
    refresh();
  }
  
  public void dispose()
  {
    m_selectionProvider.removeSelectionChangedListener( this );
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    m_commandManager.postCommand( new MoveLayerCommand( m_shift, m_listProvider, m_selectionProvider ) );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( SelectionChangedEvent event )
  {
    refresh();
  }

  private void refresh()
  {
    boolean bEnable = false;
    
    final IStructuredSelection s = (IStructuredSelection)m_selectionProvider.getSelection();
    if( !s.isEmpty() )
    {
      final List list = m_listProvider.getLayerlist();
      
      final int index = list.indexOf( s.getFirstElement() );
      
      bEnable = ( m_shift > 0 && index < list.size() - 1 ) ||
                ( m_shift < 0 && index > 0 );
      
    }

    setEnabled( bEnable );
  }
}
