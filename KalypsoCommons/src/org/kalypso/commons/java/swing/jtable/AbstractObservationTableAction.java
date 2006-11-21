package org.kalypso.commons.java.swing.jtable;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.JTable;

/**
 * AbstractObservationTableAction
 * 
 * @author schlienger
 */
public abstract class AbstractObservationTableAction extends AbstractAction
{
  private final JTable m_table;

  public AbstractObservationTableAction( final JTable table, final String name, final String toolTip )
  {
    super( name );
    m_table = table;

    putValue( SHORT_DESCRIPTION, toolTip );
  }

  public JTable getTable()
  {
    return m_table;
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed( ActionEvent e )
  {
    try
    {
      internalActionPerformed( e );
    }
    finally
    {
      m_table.repaint();
    }
  }

  protected abstract void internalActionPerformed( ActionEvent e );
}