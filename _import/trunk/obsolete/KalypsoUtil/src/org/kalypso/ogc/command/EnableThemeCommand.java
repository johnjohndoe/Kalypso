package org.kalypso.ogc.command;

import org.deegree.graphics.MapView;
import org.deegree.graphics.Theme;
import org.kalypso.util.command.ICommand;

/**
 * @author bce
 */
public class EnableThemeCommand implements ICommand
{
  private final boolean m_bEnable;
  private final Theme m_theme;
  private final MapView m_mapView;

  public EnableThemeCommand( final MapView mapView, final Theme theme, final boolean bEnable )
  {
    m_mapView = mapView;
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
  m_mapView.enableTheme(m_theme, m_bEnable);  
  }

  /**
   * @see org.kalypso.util.command.ICommand#redo()
   */
  public void redo() throws Exception
  {
    m_mapView.enableTheme(m_theme, m_bEnable);  
  }

  /**
   * @see org.kalypso.util.command.ICommand#undo()
   */
  public void undo() throws Exception
  {
    m_mapView.enableTheme(m_theme, !m_bEnable);  
  }

  /**
   * @see org.kalypso.util.command.ICommand#getDescription()
   */
  public String getDescription()
  {
    return "Theme " + ( m_bEnable ? "aktivieren" : "deaktivieren" );
  }

}
