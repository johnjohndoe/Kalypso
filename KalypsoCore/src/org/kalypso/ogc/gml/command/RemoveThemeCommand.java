package org.kalypso.ogc.gml.command;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class RemoveThemeCommand implements ICommand
{
  private final IMapModell m_mapModell;

  private final IKalypsoTheme m_theme;

  public RemoveThemeCommand( final IMapModell mapModell, final IKalypsoTheme theme )
  {
    m_mapModell = mapModell;
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
    m_mapModell.removeTheme( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_mapModell.removeTheme( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_mapModell.addTheme( m_theme );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Thema nach unten verschieben";
  }
}