package org.kalypso.ogc.sensor.tableview.swing.actions;

import javax.swing.AbstractAction;

import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;

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
}