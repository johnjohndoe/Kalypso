package org.kalypso.ogc.command;

import org.kalypso.ogc.IMapModell;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class EnableThemeCommand implements ICommand
{
  private final boolean m_bEnable;
  private final IKalypsoTheme m_theme;
  private final IMapModell m_mapModell;

  public EnableThemeCommand( final IMapModell mapModell, final IKalypsoTheme theme, final boolean bEnable )
  {
    m_mapModell = mapModell;
    m_theme = theme;
    m_bEnable = bEnable;
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
  m_mapModell.enableTheme(m_theme, m_bEnable);  
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_mapModell.enableTheme(m_theme, m_bEnable);  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_mapModell.enableTheme(m_theme, !m_bEnable);  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Theme " + ( m_bEnable ? "aktivieren" : "deaktivieren" );
  }

}
