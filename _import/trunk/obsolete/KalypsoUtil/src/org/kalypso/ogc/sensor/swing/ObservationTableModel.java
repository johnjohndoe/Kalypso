package org.kalypso.ogc.sensor.swing;

import javax.swing.table.AbstractTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;

/**
 * TableModel für IObservation. Kann in eine JTable benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private IObservation m_obs = null;

  private ITuppleModel m_values = null;

  public ObservationTableModel()
  {
    super();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  public Class getColumnClass( int columnIndex )
  {
    return ( (IAxis)m_obs.getAxisList().get( columnIndex ) ).getClass();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  public String getColumnName( int column )
  {
    return ( (IAxis)m_obs.getAxisList().get( column ) ).getLabel();
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount()
  {
    if( m_obs == null )
      return 0;

    return m_obs.getAxisList().size();
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount()
  {
    if( m_obs == null )
      return 0;

    return m_values.getCount();
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
    return m_values.getElement( rowIndex, ( (IAxis)m_obs.getAxisList().get( columnIndex ) )
        .getPosition() );
  }

  /**
   * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
   */
  public boolean isCellEditable( int rowIndex, int columnIndex )
  {
    return false;
  }

  /**
   * Allows to change the currently displayed observation
   */
  public void setObservation( IObservation obs, ITuppleModel values )
  {
    m_obs = obs;
    m_values = values;

    fireTableDataChanged();

    fireTableStructureChanged();
  }
}
