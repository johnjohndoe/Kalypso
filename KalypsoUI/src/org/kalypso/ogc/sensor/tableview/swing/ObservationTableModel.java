package org.kalypso.ogc.sensor.tableview.swing;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable
 * benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private ITableViewRules m_rules = null;

  private final DefaultTableModel m_valuesModel = new DefaultTableModel();

  private final TreeSet m_sharedModel = new TreeSet();

  private final List m_columns = new ArrayList();

  private IAxis m_sharedAxis = null;

  /**
   * Adds a column to this tablemodel.
   * 
   * @param col
   * @throws SensorException
   */
  public void addTableViewColumn( final ITableViewColumn col )
      throws SensorException
  {
    synchronized( m_columns )
    {
      if( col == null )
        return;

      m_columns.add( col );

      final IAxis keyAxis = col.getKeyAxis();

      if( m_sharedAxis == null )
        m_sharedAxis = keyAxis;
      else
      {
        // verify compatibility of the axes
        if( m_sharedAxis.getDataClass() != keyAxis.getDataClass()
            || !m_sharedAxis.getUnit().equals( keyAxis.getUnit() )
            || !m_sharedAxis.getType().equals( keyAxis.getType() ) )
        {
          throw new SensorException( m_sharedAxis + " ist nicht mit " + keyAxis
              + " kompatibel." );
        }
      }

      // values of observation of the column
      final ITuppleModel tupModel = col.getObservation().getValues( col.getArguments() );

      // fill shared column values
      for( int r = 0; r < tupModel.getCount(); r++ )
        m_sharedModel.add( tupModel.getElement( r, keyAxis ) );

      // add tablecolumn to tablemodel
      m_valuesModel.addColumn( col.getName() );
      final int colIndex = m_valuesModel.findColumn( col.getName() );

      if( m_sharedModel.size() > m_valuesModel.getRowCount() )
        m_valuesModel.setRowCount( m_sharedModel.size() );

      // fill valued column values
      int r = 0;
      for( final Iterator it = m_sharedModel.iterator(); it.hasNext(); )
      {
        final Object sharedElement = it.next();

        final int index = tupModel.indexOf( sharedElement, keyAxis );
        if( index != -1 )
        {
          m_valuesModel.setValueAt(
              tupModel.getElement( index, col.getAxis() ), r, colIndex );
        }

        r++;
      }

      fireTableStructureChanged();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  public Class getColumnClass( int columnIndex )
  {
    synchronized( m_columns )
    {
      if( columnIndex == 0 )
        return m_sharedAxis.getDataClass();

      return ((ITableViewColumn) m_columns.get( columnIndex - 1 ))
          .getColumnClass();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  public String getColumnName( int columnIndex )
  {
    synchronized( m_columns )
    {
      if( columnIndex == 0 )
        return m_sharedAxis.getName();

      return m_valuesModel.getColumnName( columnIndex - 1 );
    }
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    synchronized( m_columns )
    {
      if( m_columns.size() == 0 )
        return 0;

      return m_columns.size() + 1;
    }
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount( )
  {
    synchronized( m_columns )
    {
      if( m_columns.size() == 0 )
        return 0;

      return m_sharedModel.size();
    }
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
    synchronized( m_columns )
    {
      if( columnIndex == 0 )
        return m_sharedModel.toArray()[rowIndex];

      return m_valuesModel.getValueAt( rowIndex, columnIndex - 1 );
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
   */
  public boolean isCellEditable( int rowIndex, int columnIndex )
  {
    synchronized( m_columns )
    {
      rowIndex++; // fake, just to remove compile warnings
      
      if( columnIndex == 0 )
        return false;

      return ((ITableViewColumn) m_columns.get( columnIndex - 1 )).isEditable();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object, int,
   *      int)
   */
  public void setValueAt( Object aValue, int rowIndex, int columnIndex )
  {
    synchronized( m_columns )
    {
      m_valuesModel.setValueAt( aValue, rowIndex, columnIndex - 1 );

      ((ITableViewColumn) m_columns.get( columnIndex - 1 )).setDirty( true );
    }
  }

  /**
   * Sets the rules. Important: you should call this method if you want the
   * rules rendering to be enabled.
   * 
   * @param rules
   */
  public void setRules( final ITableViewRules rules )
  {
    m_rules = rules;
  }

  /**
   * Finds the renderingrules for the given element.
   * 
   * @param row
   * @param column
   * @return rendering rules or empty array if no rules found.
   */
  public RenderingRule[] findRules( int row, int column )
  {
    final String kStatusCol = KalypsoStatusUtils
        .getStatusAxisLabelFor( ((ITableViewColumn) m_columns.get( column - 1 ))
            .getAxis() );

    final IAxis valueAxis = findValueAxis( kStatusCol );
    if( valueAxis == null )
      return new RenderingRule[0];

    return m_rules.findRules( ((Integer) getValueAt( row, valueAxis
        .getPosition() + 1 )).intValue() );
  }

  /**
   * Finds a value axis with the given name.
   * 
   * @param name
   * @return value axis or null if not found.
   */
  private IAxis findValueAxis( final String name )
  {
    for( final Iterator it = m_columns.iterator(); it.hasNext(); )
    {
      final ITableViewColumn col = (ITableViewColumn) it.next();

      if( col.getAxis().getName().equals( name ) )
        return col.getAxis();
    }

    return null;
  }

  /**
   * Clears the columns of the model.
   */
  public void clearColumns( )
  {
    m_columns.clear();
    m_sharedModel.clear();
    m_valuesModel.setColumnCount( 0 );

    fireTableStructureChanged();
  }

  /**
   * Adds a row that contains only the value for the shared column.
   * 
   * @param sharedElement
   *          the object for the value of the common column
   * @return the index at which row was added
   */
  public int addRow( final Object sharedElement )
  {
    synchronized( m_columns )
    {
      m_sharedModel.add( sharedElement );

      final int index = Arrays.binarySearch( m_sharedModel.toArray(),
          sharedElement );

      m_valuesModel.insertRow( index, new Object[m_columns.size()] );

      fireTableDataChanged();

      return index;
    }
  }

  /**
   * Adds a whole row, including the common column.
   * 
   * @param row
   *          common column should be at position 0 in the vector
   * @return index where row was added
   */
  public int addRow( final List row )
  {
    synchronized( m_columns )
    {
      // first insert shared element (first in the row)
      final int index = addRow( row.get( 0 ) );

      // insert values
      for( int i = 1; i < row.size(); i++ )
        setValueAt( row.get( i ), index, i );

      return index;
    }
  }

  /**
   * Removes the row at the given position
   * 
   * @param index
   * @return the removed row
   */
  public List removeRow( int index )
  {
    synchronized( m_columns )
    {
      final ArrayList row = new ArrayList( m_columns.size() + 1 );

      row.add( m_sharedModel.toArray()[index] );

      int colIndex = 0;
      for( final Iterator it = m_columns.iterator(); it.hasNext(); )
      {
        final ITableViewColumn col = (ITableViewColumn) it.next();

        row.add( m_valuesModel.getValueAt( index, colIndex ) );
        col.setDirty( true );

        colIndex++;
      }

      m_valuesModel.removeRow( index );
      m_sharedModel.remove( row.get( 0 ) );

      fireTableDataChanged();

      return row;
    }
  }
}