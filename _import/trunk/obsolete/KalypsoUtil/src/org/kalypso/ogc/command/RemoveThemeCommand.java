package org.kalypso.ogc.command;

import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.gml.KalypsoTheme;
import org.kalypso.util.command.ICommand;

/**
 * @author belger
 */
public class RemoveThemeCommand implements ICommand
{
  private final MapModell m_mapModell;

  private final KalypsoTheme m_theme;

  public RemoveThemeCommand( final MapModell mapModell, final KalypsoTheme theme )
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