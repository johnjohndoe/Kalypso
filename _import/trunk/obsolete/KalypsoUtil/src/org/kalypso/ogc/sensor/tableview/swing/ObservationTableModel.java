package org.kalypso.ogc.sensor.tableview.swing;

import javax.swing.table.AbstractTableModel;

import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private ITableViewColumn[] m_columns;
  private IVariableArguments m_args;

  public ObservationTableModel()
  {
    // nix
  }
  
  /**
   * Constructor with columns. Calls setColumns( ITableViewColumn[] ).
   */
  public ObservationTableModel( ITableViewColumn[] columns )
  {
    setColumns( columns );
  }

  /**
   * Sets the columns.
   * 
   * @param columns can be null. In that case the table model is empty.
   */
  public void setColumns( ITableViewColumn[] columns )
  {
    m_columns = columns;

    fireTableStructureChanged();
    fireTableDataChanged();
  }
  
  /**
   * Sets the arguments that will be used when fetching the values.
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;

    fireTableDataChanged();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  public Class getColumnClass( int columnIndex )
  {
    return m_columns[ columnIndex ].getDisplayAxis().getDataClass();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  public String getColumnName( int column )
  {
    return m_columns[ column ].getName();
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount()
  {
    if( m_columns == null )
      return 0;

    return m_columns.length;
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount()
  {
    if( m_columns == null || m_args == null )
      return 0;

    try
    {
      return m_columns[0].getObservation().getValues(m_args).getCount();
    }
    catch( SensorException e )
    {
      // TODO: handling
      e.printStackTrace();
      return 0;
    }
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
    try
    {
      return m_columns[ columnIndex ].getObservation().getValues(m_args).getElement(rowIndex, m_columns[ columnIndex ].getDisplayAxis().getPosition() );
    }
    catch( SensorException e )
    {
      // TODO: handling
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
   */
  public boolean isCellEditable( int rowIndex, int columnIndex )
  {
    return m_columns[ columnIndex ].isEditable();
  }
  
  /**
   * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object, int, int)
   */
  public void setValueAt( Object aValue, int rowIndex, int columnIndex )
  {
    try
    {
      m_columns[ columnIndex ].getObservation().getValues(m_args).setElement(rowIndex, aValue, m_columns[ columnIndex ].getDisplayAxis().getPosition() );
    }
    catch( SensorException e )
    {
      // TODO: handling
      e.printStackTrace();
    }
  }
}
