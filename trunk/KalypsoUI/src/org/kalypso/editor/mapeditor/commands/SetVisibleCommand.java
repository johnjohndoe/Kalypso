package org.kalypso.editor.mapeditor.commands;

import org.kalypso.util.command.ICommand;
import org.kalypso.xml.types.GisviewLayerType;

/**
 * @author gernot
 */
public class SetVisibleCommand implements ICommand
{
  private boolean m_bVisible;
  private GisviewLayerType m_layer;

  public SetVisibleCommand( final GisviewLayerType layer, final boolean bVisible )
  {
    m_layer = layer;
    m_bVisible = bVisible;
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
    m_layer.setVisible( m_bVisible );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_layer.setVisible( m_bVisible );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_layer.setVisible( !m_bVisible );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return m_bVisible ? "Thema anzeigen" : "Thema verbergen";
  }

}
