package org.kalypso.ogc.gml.command;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class MoveThemeDownCommand implements ICommand
{
  private final IMapModell m_MapModell;

  private final IKalypsoTheme m_theme;

  public MoveThemeDownCommand( final IMapModell mapModell, final IKalypsoTheme theme )
  {
    m_MapModell = mapModell;
    m_theme = theme;
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
    m_MapModell.moveDown( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_MapModell.moveDown( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_MapModell.moveUp( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Thema nach unten verschieben";
  }
}