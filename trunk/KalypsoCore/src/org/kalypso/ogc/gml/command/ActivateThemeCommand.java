package org.kalypso.ogc.gml.command;

import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class ActivateThemeCommand implements ICommand
{
  private final IKalypsoTheme m_newActive;

  private final IKalypsoTheme m_oldActive;

  private final IMapModell m_mapModell;

  public ActivateThemeCommand( final IMapModell mapModell, final IKalypsoTheme activeTheme )
  {
    m_mapModell = mapModell;
    m_newActive = activeTheme;
    m_oldActive = mapModell.getActiveTheme();
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
    m_mapModell.activateTheme( m_newActive );
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_mapModell.activateTheme( m_newActive );
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_mapModell.activateTheme( m_oldActive );
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "aktives Thema setzen";
  }

}