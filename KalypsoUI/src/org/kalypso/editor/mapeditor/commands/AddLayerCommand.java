package org.kalypso.editor.mapeditor.commands;

import java.util.List;

import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.kalypso.util.command.ICommand;
import org.kalypso.xml.types.ILayerTypeFactory;
import org.kalypso.xml.types.ILayerlistProvider;
import org.kalypso.xml.types.LayerType;
import org.kalypso.xml.types.LayerlistHelper;

/**
 * @author belger
 */
public class AddLayerCommand implements ICommand
{
  private int m_index;

  private final ILayerlistProvider m_listProvider;

  private final ISelectionProvider m_selectionProvider;

  private final ILayerTypeFactory m_layerFactory;

  private LayerType m_layer;

  public AddLayerCommand( final ILayerlistProvider listProvider,
      final ISelectionProvider selectionProvider, final ILayerTypeFactory layerFactory )
  {
    m_listProvider = listProvider;
    m_selectionProvider = selectionProvider;
    m_layerFactory = layerFactory;
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
    m_layer = m_layerFactory.createNewLayer();
    if( m_layer == null )
      return;

    final List layerlist = m_listProvider.getLayerlist();

    final IStructuredSelection selection = (IStructuredSelection)m_selectionProvider.getSelection();
    m_index = selection.isEmpty() ? 0 : layerlist.indexOf( selection.getFirstElement() );

    layerlist.add( m_index, m_layer );
    LayerlistHelper.selectLayer( layerlist, m_layer, m_selectionProvider );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    if( m_layer != null )
    {
      final List layerList = m_listProvider.getLayerlist();
      layerList.add( m_index, m_layer );
      LayerlistHelper.selectLayer( layerList, m_layer, m_selectionProvider );
    }
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    if( m_layer != null )
    {
      m_listProvider.getLayerlist().remove( m_layer );
      m_selectionProvider.setSelection( new StructuredSelection() );
    }
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Thema hinzufügen";
  }
}