package org.kalypso.editor.mapeditor.commands;

import java.util.List;

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.util.command.ICommand;
import org.kalypso.xml.types.ILayerlistProvider;
import org.kalypso.xml.types.LayerType;
import org.kalypso.xml.types.LayerlistHelper;

/**
 * @author belger
 */
public class RemoveLayerCommand implements ICommand
{
  private LayerType m_layer;
  private int m_index;
  
  private final ISelectionProvider m_selectionProvider;
  private final ILayerlistProvider m_listProvider;

  public RemoveLayerCommand( final ILayerlistProvider listProvider, final ISelectionProvider selectionProvider  )
  {
    m_listProvider = listProvider;
    m_selectionProvider = selectionProvider;
  }

  /**
   * @see org.kalypso.util.command.ICommand#isUndoable()
   */
  public boolean isUndoable()
  {
    return true;
  }

  /**
   * @see org.kalypso.util.command.ICommand#process()
   */
  public void process() throws Exception
  {
    final IStructuredSelection s = (IStructuredSelection)m_selectionProvider.getSelection();
    if( s.isEmpty() )
      return;

    m_layer = (LayerType)s.getFirstElement();

    final List layers = m_listProvider.getLayerlist();
    
    m_index = layers.indexOf( m_layer );
    
    layers.remove( m_layer );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_listProvider.getLayerlist().remove( m_layer );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    final List layerlist = m_listProvider.getLayerlist();
    layerlist.add( m_index, m_layer );
    
    LayerlistHelper.selectLayer( layerlist, m_layer, m_selectionProvider );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Thema entfernen";
  }

}
