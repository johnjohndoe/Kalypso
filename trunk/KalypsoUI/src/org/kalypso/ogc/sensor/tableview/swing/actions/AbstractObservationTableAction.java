package org.kalypso.ogc.sensor.tableview.swing.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTableModel;

/**
 * AbstractObservationTableAction
 * 
 * @author schlienger
 */
public abstract class AbstractObservationTableAction extends AbstractAction
{
  private final ObservationTable m_table;

  public AbstractObservationTableAction( final ObservationTable table,
      final String name, final String toolTip )
  {
    super( name );
    m_table = table;

    putValue( SHORT_DESCRIPTION, toolTip );
  }

  public ObservationTable getTable( )
  {
    return m_table;
  }

  /**
   * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
   */
  public void actionPerformed( ActionEvent e )
  {
    final boolean sync = ((ObservationTableModel) m_table.getModel())
        .isSyncObservation();
    ((ObservationTableModel) m_table.getModel())
        .setSynchronizeObservations( false );

    try
    {
      internalActionPerformed( e );
    }
    finally
    {
      m_table.repaint();

      ((ObservationTableModel) m_table.getModel())
          .setSynchronizeObservations( sync );
    }
  }

  protected abstract void internalActionPerformed( ActionEvent e );
}