package org.kalypso.ogc.sensor.tableview.swing;

import javax.swing.JPopupMenu;

import org.kalypso.ogc.sensor.tableview.swing.actions.InterpolateSelectedAction;
import org.kalypso.ogc.sensor.tableview.swing.actions.SetAllAction;
import org.kalypso.ogc.sensor.tableview.swing.actions.SetBottomAction;
import org.kalypso.ogc.sensor.tableview.swing.actions.SetSelectedAction;
import org.kalypso.ogc.sensor.tableview.swing.actions.SetTopAction;

/**
 * PopupMenu
 * 
 * @author schlienger
 */
public class PopupMenu extends JPopupMenu
{
  private final ObservationTable m_table;

  public PopupMenu( ObservationTable table )
  {
    super();
    m_table = table;

    prepareMenu();
  }

  private void prepareMenu( )
  {
    add( new SetAllAction( m_table ) );
    add( new SetSelectedAction( m_table ) );
    add( new SetBottomAction( m_table ) );
    add( new SetTopAction( m_table ) );
    add( new Separator() );
    add( new InterpolateSelectedAction( m_table) );
  }
}