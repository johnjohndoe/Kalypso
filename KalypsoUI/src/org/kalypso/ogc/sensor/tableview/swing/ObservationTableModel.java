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
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable
 * benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private final static ITableViewColumn[] EMPTY_COLS = new ITableViewColumn[0];

  private ITableViewColumn[] m_columns = EMPTY_COLS;

  private IVariableArguments m_args = null;

  /** common column */
  private Set m_commonColumn = null;

  /** common fake axis */
  private DefaultAxis m_ccAxis = null;

  private ITableViewRules m_rules = null;

  private DefaultTableModel m_valuesModel = null;

  public ObservationTableModel( )
  {
    // nix
  }

  /**
   * Constructor with columns. Calls setColumns( ITableViewColumn[] ).
   * 
   * @param columns
   * 
   * @throws SensorException
   */
  public ObservationTableModel( ITableViewColumn[] columns )
      throws SensorException
  {
    setColumns( columns, null );
  }

  /**
   * Adds a column.
   * 
   * @param column
   * 
   * @throws SensorException
   */
  public void addColumn( final ITableViewColumn column ) throws SensorException
  {
//    synchronized( m_columns )
//    {
      if( m_columns != EMPTY_COLS )
      {
        final Set cols = new HashSet( Arrays.asList( m_columns ) );
        cols.add( column );
        setColumns(
            (ITableViewColumn[]) cols.toArray( new ITableViewColumn[0] ),
            column.getArguments() );
      }
      else
        setColumns( new ITableViewColumn[] { column }, column.getArguments() );
//    }
  }

  /**
   * Sets the columns.
   * 
   * @param columns
   *          can be null. In that case the table model is empty.
   * @param args
   *          the arguments that will be used when fetching the values
   * @throws SensorException
   */
  public void setColumns( final ITableViewColumn[] columns,
      final IVariableArguments args ) throws SensorException
  {
    if( columns == null )
      throw new IllegalArgumentException(
          "columns null not allowed. (If you want to clear the table: use clearColumns() instead)" );

    m_columns = columns;
    m_args = args;

    // reset
    m_commonColumn = null;
    m_ccAxis = null;
    m_valuesModel = null;

    if( m_columns != EMPTY_COLS )
    {
      // the common column, merges all the values from the common axes
      m_commonColumn = new TreeSet();

      final SimpleTuppleModel[] tupModels = new SimpleTuppleModel[m_columns.length];

      for( int col = 0; col < m_columns.length; col++ )
      {
        IAxis sharedAxis = m_columns[col].getSharedAxis();

        // create common axis (fake)
        if( m_ccAxis == null )
          m_ccAxis = new DefaultAxis( sharedAxis );

        // verify compatibility of the axes
        if( m_ccAxis.getDataClass() != sharedAxis.getDataClass()
            || !m_ccAxis.getUnit().equals( sharedAxis.getUnit() ) )
          throw new SensorException( m_ccAxis + " ist nicht mit " + sharedAxis
              + " kompatibel." );

        tupModels[col] = new SimpleTuppleModel( m_columns[col].getObservation()
            .getValues( m_args ) );

        for( int row = 0; row < tupModels[col].getCount(); row++ )
          m_commonColumn.add( tupModels[col].getElement( row, sharedAxis ) );
      }

      // the common model, merges all values from all models for the value axes
      m_valuesModel = new DefaultTableModel( m_commonColumn.size(),
          m_columns.length );
      
      int row = 0;
      
      for( final Iterator it = m_commonColumn.iterator(); it.hasNext(); )
      {
        final Object sharedElement = it.next();

        for( int col = 0; col < m_columns.length; col++ )
        {
          final int index = tupModels[col].indexOf( sharedElement,
              m_columns[col].getSharedAxis() );

          if( index != -1 )
            m_valuesModel.setValueAt( tupModels[col].getElement( index, m_columns[col]
                .getValueAxis() ), row, col );
        }

        row++;
      }
    }
    
    fireTableStructureChanged();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  public Class getColumnClass( int columnIndex )
  {
    if( columnIndex == 0 )
      return m_ccAxis.getDataClass();

    return m_columns[columnIndex - 1].getValueAxis().getDataClass();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  public String getColumnName( int column )
  {
    if( column == 0 )
      return m_ccAxis.getLabel();

    return m_columns[column - 1].getName();
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount( )
  {
    if( m_columns == EMPTY_COLS || m_columns.length == 0 )
      return 0;

    return m_columns.length + 1;
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public int getRowCount( )
  {
    if( m_commonColumn == null )
      return 0;

    return m_commonColumn.size();
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
//    synchronized( m_columns )
//    {
      if( columnIndex == 0 )
        return m_commonColumn.toArray()[rowIndex];

      //    // Retrieve object of common column at given position
      //    Object obj = m_commonColumn.toArray()[rowIndex];
      //
      //    // Use this object to retrieve real index from tupple (with shared
      // axis)
      //    int index = m_tupModels[columnIndex - 1].indexOf( obj,
      //        m_columns[columnIndex - 1].getSharedAxis() );
      //
      //    if( index < 0 )
      //      return null;
      //
      //    // Now we can retrieve the element using value axis
      //    return m_tupModels[columnIndex - 1].getElement( index,
      //        m_columns[columnIndex - 1].getValueA xis() );

      return m_valuesModel.getValueAt( rowIndex, columnIndex - 1 );
//    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#isCellEditable(int, int)
   */
  public boolean isCellEditable( int rowIndex, int columnIndex )
  {
    if( columnIndex == 0 )
      return false;

    return m_columns[columnIndex - 1].isEditable();
  }

  /**
   * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object, int,
   *      int)
   */
  public void setValueAt( Object aValue, int rowIndex, int columnIndex )
  {
    //    // Retrieve object of common column at given position
    //    Object obj = m_commonColumn.toArray()[rowIndex];
    //
    //    // Use this object to retrieve real index from tupple (with shared axis)
    //    int index = m_tupModels[columnIndex - 1].indexOf( obj,
    //        m_columns[columnIndex - 1].getSharedAxis() );
    //
    //    // Now we can retrieve the element using value axis
    //    m_tupModels[columnIndex - 1].setElement( index, aValue,
    //        m_columns[columnIndex - 1].getValueAxis() );

    m_valuesModel.setValueAt( aValue, rowIndex, columnIndex - 1 );
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
        .getStatusAxisLabelFor( m_columns[column - 1].getValueAxis() );

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
    for( int i = 0; i < m_columns.length; i++ )
    {
      if( m_columns[i].getValueAxis().getLabel().equals( name ) )
        return m_columns[i].getValueAxis();
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
    m_commonColumn.add( object );

    final int index = Arrays.binarySearch( m_commonColumn.toArray(), object );

    m_valuesModel.insertRow( index, new Object[m_columns.length] );

    fireTableDataChanged();
    
    return index;
  }
  
  /**
   * Adds a whole row, including the common column.
   * 
   * @param row common column should be at position 0 in the vector
   * @return index where row was added
   */
  public int addRow( final Vector row )
  {
    final int index = addRow( row.get(0) );
    
    for( int i = 1; i < row.size(); i++ )
    {
      m_valuesModel.setValueAt( row.get(i), index, i - 1 );
    }
    
    return index;
  }

  /**
   * Removes the row at the given position
   * 
   * @param index
   * @return the removed row
   */
  public Vector removeRow( int index )
  {
    final Vector row = new Vector( m_columns.length );
    
    row.add( m_commonColumn.toArray()[index] );
    
    for( int col = 0; col < m_columns.length; col++ )
      row.add( m_valuesModel.getValueAt( index, col ) );
    
    m_valuesModel.removeRow( index );
    m_commonColumn.remove( row.get(0) );
    
    fireTableDataChanged();
    
    return row;
  }
}