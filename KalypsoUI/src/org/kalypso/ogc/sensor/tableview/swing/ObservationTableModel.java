package org.kalypso.ogc.sensor.tableview.swing;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
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
  private final static ITableViewColumn[] EMPTY_COLS = new ITableViewColumn[0];

  // columns used in this model
  private ITableViewColumn[] m_columns = EMPTY_COLS;

  // value axes of the observations in the columns
  private IAxis[] m_valueAxes = null;

  // shared axes of the observations in the columns
  private IAxis[] m_sharedAxes = null;

  /** common column */
  private Set m_commonColumn = null;

  /** common fake axis */
  private DefaultAxis m_ccAxis = null;

  private ITableViewRules m_rules = null;

  private DefaultTableModel m_valuesModel = null;

  /**
   * Adds a column.
   * 
   * @param column
   * 
   * @throws SensorException
   */
  public void addColumn( final ITableViewColumn column ) throws SensorException
  {
    synchronized( m_columns )
    {
      if( m_columns != EMPTY_COLS )
      {
        final Set cols = new HashSet( Arrays.asList( m_columns ) );
        cols.add( column );
        setColumns( (ITableViewColumn[]) cols.toArray( new ITableViewColumn[0] ) );
      }
      else
        setColumns( new ITableViewColumn[] { column } );
    }
  }

  /**
   * Sets the columns.
   * 
   * @param columns
   *          can be null. In that case the table model is empty.
   * @throws SensorException
   */
  private void setColumns( final ITableViewColumn[] columns )
      throws SensorException
  {
    synchronized( m_columns )
    {
      if( columns == null )
        throw new IllegalArgumentException(
            "columns null not allowed. (If you want to clear the table: use clearColumns() instead)" );

      m_columns = columns;

      // reset
      m_commonColumn = null;
      m_ccAxis = null;
      m_sharedAxes = null;
      m_valueAxes = null;
      m_valuesModel = null;

      if( m_columns != EMPTY_COLS )
      {
        // the common column, merges all the values from the common axes
        m_commonColumn = new TreeSet();

        final SimpleTuppleModel[] tupModels = new SimpleTuppleModel[m_columns.length];

        m_valueAxes = new IAxis[m_columns.length];
        m_sharedAxes = new IAxis[m_columns.length];
        
        for( int col = 0; col < m_columns.length; col++ )
        {
          final IAxis[] axes = m_columns[col].getObservation().getAxisList();
          final IAxis[] keys = ObservationUtilities.findAxisByKey( axes );

          // no key axis, do nothing
          if( keys.length == 0 )
            continue;

          // shared axis is the first key axis that was found
          m_sharedAxes[col] = keys[0];

          // value axis is given by its name according to column model
          m_valueAxes[col] = ObservationUtilities.findAxisByName( axes, m_columns[col].getAxisName() );

          // create common axis (fake)
          if( m_ccAxis == null )
            m_ccAxis = new DefaultAxis( m_sharedAxes[col] );

          // verify compatibility of the axes
          if( m_ccAxis.getDataClass() != m_sharedAxes[col].getDataClass()
              || !m_ccAxis.getUnit().equals( m_sharedAxes[col].getUnit() )
              || !m_ccAxis.getType().equals( m_sharedAxes[col].getType() ) )
            throw new SensorException( m_ccAxis + " ist nicht mit "
                + m_sharedAxes[col] + " kompatibel." );

          tupModels[col] = new SimpleTuppleModel( m_columns[col]
              .getObservation().getValues( null ) );

          for( int row = 0; row < tupModels[col].getCount(); row++ )
            m_commonColumn.add( tupModels[col].getElement( row,
                m_sharedAxes[col] ) );
        }

        // the common model, merges all values from all models for the value
        // axes
        m_valuesModel = new DefaultTableModel( m_commonColumn.size(),
            m_columns.length );

        int row = 0;

        for( final Iterator it = m_commonColumn.iterator(); it.hasNext(); )
        {
          final Object sharedElement = it.next();

          for( int col = 0; col < m_columns.length; col++ )
          {
            final int index = tupModels[col].indexOf( sharedElement,
                m_sharedAxes[col] );

            if( index != -1 )
              m_valuesModel.setValueAt( tupModels[col].getElement( index,
                  m_sharedAxes[col] ), row, col );
          }

          row++;
        }
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
        return m_ccAxis.getDataClass();

      return m_valueAxes[columnIndex - 1].getDataClass();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  public String getColumnName( int column )
  {
    synchronized( m_columns )
    {
      if( column == 0 )
        return m_ccAxis.getName();

      return m_columns[column - 1].getName();
    }
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    synchronized( m_columns )
    {
      if( m_columns == EMPTY_COLS || m_columns.length == 0 )
        return 0;

      return m_columns.length + 1;
    }
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount( )
  {
    synchronized( m_columns )
    {
      if( m_commonColumn == null )
        return 0;

      return m_commonColumn.size();
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
        return m_commonColumn.toArray()[rowIndex];

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
      if( columnIndex == 0 )
        return false;

      return m_columns[columnIndex - 1].isEditable();
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

      m_columns[columnIndex - 1].setDirty( true );
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
   * @param row
   * @param column
   * @return rendering rules
   * @throws NoSuchElementException
   */
  public RenderingRule[] findRules( int row, int column )
      throws NoSuchElementException
  {
    final String kStatusCol = KalypsoStatusUtils
        .getStatusAxisLabelFor( m_valueAxes[column - 1] );

    final IAxis valueAxis = findValueAxis( kStatusCol );

    return m_rules.findRules( ((Integer) getValueAt( row, valueAxis
        .getPosition() + 1 )).intValue() );
  }

  /**
   * Finds a value axis with the given name.
   * 
   * @param name
   * @return value axis
   * 
   * @throws NoSuchElementException
   *           when axis could not be found
   */
  private IAxis findValueAxis( final String name )
      throws NoSuchElementException
  {
    for( int i = 0; i < m_valueAxes.length; i++ )
    {
      if( m_sharedAxes[i].getName().equals( name ) )
        return m_valueAxes[i];
    }

    throw new NoSuchElementException();
  }

  /**
   * Clears the columns of the model.
   */
  public void clearColumns( )
  {
    m_columns = EMPTY_COLS;

    fireTableStructureChanged();
  }

  /**
   * Adds a row that contains only the value for the shared column.
   * 
   * @param object
   *          the object for the value of the common column
   * @return the index at which row was added
   */
  public int addRow( final Object object )
  {
    synchronized( m_columns )
    {
      m_commonColumn.add( object );

      final int index = Arrays.binarySearch( m_commonColumn.toArray(), object );

      m_valuesModel.insertRow( index, new Object[m_columns.length] );

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
  public int addRow( final Vector row )
  {
    synchronized( m_columns )
    {
      final int index = addRow( row.get( 0 ) );

      for( int i = 1; i < row.size(); i++ )
      {
        m_columns[i - 1].setDirty( true );
        m_valuesModel.setValueAt( row.get( i ), index, i - 1 );
      }

      return index;
    }
  }

  /**
   * Removes the row at the given position
   * 
   * @param index
   * @return the removed row
   */
  public Vector removeRow( int index )
  {
    synchronized( m_columns )
    {
      final Vector row = new Vector( m_columns.length );

      row.add( m_commonColumn.toArray()[index] );

      for( int col = 0; col < m_columns.length; col++ )
      {
        row.add( m_valuesModel.getValueAt( index, col ) );
        m_columns[col].setDirty( true );
      }

      m_valuesModel.removeRow( index );
      m_commonColumn.remove( row.get( 0 ) );

      fireTableDataChanged();

      return row;
    }
  }
}