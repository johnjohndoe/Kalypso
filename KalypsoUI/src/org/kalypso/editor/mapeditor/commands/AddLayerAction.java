package org.kalypso.editor.mapeditor.commands;

import org.eclipse.jface.viewers.ISelectionProvider;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.plugin.ImageProvider;
import org.kalypso.util.command.ICommandManager;
import org.kalypso.xml.types.ILayerTypeFactory;
import org.kalypso.xml.types.ILayerlistProvider;

/**
 * @author belger
 */
public class AddLayerAction extends FullAction
{
  private final ICommandManager m_commandManager;
  private final ILayerlistProvider m_listProvider;
  private ISelectionProvider m_selectionProvider;
  private ILayerTypeFactory m_layerFactory;

  public AddLayerAction( final ICommandManager commandManager, final ILayerlistProvider listProvider, final ISelectionProvider selectionProvider, final ILayerTypeFactory layerFactory )
  {
    super( "Add Layer", ImageProvider.IMAGE_MAPVIEW_OUTLINE_ADD, "Thema hinzufügen" );

    m_commandManager = commandManager;
    m_listProvider = listProvider;
    m_selectionProvider = selectionProvider;
    m_layerFactory = layerFactory;
  }
  
  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    m_commandManager.postCommand( new AddLayerCommand( m_listProvider, m_selectionProvider, m_layerFactory ) );
  }
}
