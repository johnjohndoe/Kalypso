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
public class MoveLayerCommand implements ICommand
{
  private final ILayerlistProvider m_listProvider;
  private ISelectionProvider m_selectionProvider;

  private final int m_shift;

  private LayerType m_layer = null;

  
  public MoveLayerCommand( final int shift, final ILayerlistProvider listProvider, final ISelectionProvider selectionProvider )
  {
    m_shift = shift;
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
    
    shiftLayer( m_layer, m_shift );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    shiftLayer( m_layer, m_shift );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    shiftLayer( m_layer, -m_shift );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Thema verschieben";
  }
  
  private final void shiftLayer( final LayerType layer, final int shift )
  {
    final List layers = m_listProvider.getLayerlist();

    final int targetIndex = Math.max( 0, layers.indexOf( layer ) + shift );

    // move item in list
    layers.remove( layer );

    if( targetIndex >= layers.size() )
      layers.add( layer );
    else
      layers.add( targetIndex, layer );
    
    LayerlistHelper.selectLayer( layers, layer, m_selectionProvider );
  }
}
