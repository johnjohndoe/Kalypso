package org.kalypso.commons.java.swing.jtable;

import javax.swing.JPopupMenu;
import javax.swing.JTable;


/**
 * TODO: this makes no sense here, move back to observation stuff.<br>
 * 
 * Simple PopupMenu with a predefined list of actions:
 * <ul>
 * <li>SetAllAction
 * <li>SetSelectedAction
 * <li>SetBottomAction
 * <li>SetTopAction
 * <li>InterpolateSelectionAction
 * </ul>
 * 
 * @author schlienger
 */
public class PopupMenu extends JPopupMenu
{
  private final JTable m_table;

  public PopupMenu( JTable table )
  {
    super();
    m_table = table;

    prepareMenu();
  }

  private void prepareMenu()
  {
    add( new SetAllAction( m_table ) );
    add( new SetSelectedAction( m_table ) );
    add( new SetBottomAction( m_table ) );
    add( new SetTopAction( m_table ) );
    add( new Separator() );
    add( new InterpolateSelectedAction( m_table ) );
  }
}