package org.kalypso.ogc.sensor.tableview.swing;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable
 * benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private static final RenderingRule[] EMPTY_RENDERING_RULES = new RenderingRule[0];
  private static final Integer STATUS_USERMOD = new Integer( KalypsoStati.BIT_USER_MODIFIED );
  private ITableViewRules m_rules = RulesFactory.getDefaultRules();

  private final DefaultTableModel m_valuesModel = new DefaultTableModel();
  private final DefaultTableModel m_statusModel = new DefaultTableModel();

  private final TreeSet m_sharedModel = new TreeSet();

  private final List m_columns = new ArrayList();

  private IAxis m_sharedAxis = null;


  /**
   * @see javax.swing.table.AbstractTableModel#addTableModelListener(javax.swing.event.TableModelListener)
   */
  public void addTableModelListener( TableModelListener l )
  {
    m_valuesModel.addTableModelListener( l );
    
    super.addTableModelListener( l );
  }
  
  /**
   * @see javax.swing.table.AbstractTableModel#removeTableModelListener(javax.swing.event.TableModelListener)
   */
  public void removeTableModelListener( TableModelListener l )
  {
    m_valuesModel.removeTableModelListener( l );
    
    super.removeTableModelListener( l );
  }
  
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
        // verify compatibility of the axes. We do not use IAxis.equals()
        // since the position is not relevant here
        if( m_sharedAxis.getDataClass() != keyAxis.getDataClass()
            || !m_sharedAxis.getUnit().equals( keyAxis.getUnit() )
            || !m_sharedAxis.getType().equals( keyAxis.getType() ) )
        {
          throw new SensorException( m_sharedAxis + " ist nicht mit " + keyAxis
              + " kompatibel." );
        }
      }

      // values of observation of the column
      final IObservation obs = col.getTheme().getObservation();
      final ITuppleModel tupModel = obs.getValues(
          col.getTheme().getArguments() );

      // fill shared column values
      for( int r = 0; r < tupModel.getCount(); r++ )
      {
        final Object elt = tupModel.getElement( r, keyAxis );
        m_sharedModel.add( elt );
      }

      // add tablecolumn to tablemodel
      m_valuesModel.addColumn( col.getName() );
      final int colIndex = m_valuesModel.findColumn( col.getName() );

      // add tablecolumn to status model
      final IAxis statusAxis = getStatusAxis( col );
      if( statusAxis != null )
        m_statusModel.addColumn( statusAxis.getName() );
      else
        m_statusModel.addColumn( "no-status" );
      
      if( m_sharedModel.size() > m_valuesModel.getRowCount() )
      {
        m_valuesModel.setRowCount( m_sharedModel.size() );
        m_statusModel.setRowCount( m_sharedModel.size() );
      }

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
          
          if( statusAxis != null )
            m_statusModel.setValueAt( tupModel.getElement( index, statusAxis), r, colIndex );
        }

        r++;
      }

      fireTableStructureChanged();
    }
  }

  /**
   * Returns the status axis for the 'normal' axis found in the given column.
   * 
   * @param col
   * @return status axis or null if not found
   */
  private IAxis getStatusAxis( final ITableViewColumn col )
  {
    final IAxis[] obsAxes = col.getTheme().getObservation().getAxisList();
    final String statusAxisLabel = KalypsoStatusUtils.getStatusAxisLabelFor( col.getAxis() );
    final IAxis statusAxis = ObservationUtilities.findAxisByNameNoEx( obsAxes, statusAxisLabel );

    return statusAxis;
  }
  
  /**
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  public Class getColumnClass( int columnIndex )
  {
    synchronized( m_columns )
    {
      if( m_columns.size() == 0 )
        return String.class;

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
      if( m_columns.size() == 0 )
        return "Keine Daten vorhanden";

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
        return 1;

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
      final ITableViewColumn col = (ITableViewColumn) m_columns.get( columnIndex - 1 );
      col.setDirty( true );
      
      m_valuesModel.setValueAt( aValue, rowIndex, columnIndex - 1 );
      m_statusModel.setValueAt( STATUS_USERMOD, rowIndex, columnIndex - 1 );
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
    if( column == 0 )
      return EMPTY_RENDERING_RULES;
    
    final Number status = (Number) m_statusModel.getValueAt( row, column - 1 );
    if( status == null )
      return EMPTY_RENDERING_RULES;

    return m_rules.findRules( status.intValue() );
  }
  
  /**
   * Clears the columns of the model.
   */
  public void clearColumns( )
  {
    m_columns.clear();
    m_sharedModel.clear();
    m_sharedAxis = null;
    m_valuesModel.setColumnCount( 0 );
    m_statusModel.setColumnCount( 0 );

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
      
      m_statusModel.insertRow( index, new Object[m_columns.size()] );

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
      m_statusModel.removeRow( index );
      m_sharedModel.remove( row.get( 0 ) );

      fireTableDataChanged();

      return row;
    }
  }

  /**
   * Creates an ITuppleModel with the values of the columns of the given theme.
   * 
   * @param theme
   * @return new model
   */
  public ITuppleModel getValues( final ITableViewTheme theme )
  {
    return getValues( theme.getColumns(), null );
  }

  /**
   * Creates an ITuppleModel with the values of all columns and the given rows.
   * 
   * @param rows
   * @return new model
   */
  public ITuppleModel getValues( final int[] rows )
  {
    return getValues( m_columns, rows );
  }

  /**
   * Creates an ITuppleModel with the values for the given columns and rows.
   * 
   * @param cols
   *          list of columns for which to create a tupple model.
   * @param rows
   *          indices of rows to export
   * 
   * @return new model
   */
  protected ITuppleModel getValues( final List cols, final int[] rows )
  {
    final List allAxes = new ArrayList();
    final Map statusAxes = new HashMap();

    final Iterator it = cols.iterator();
    while( it.hasNext() )
    {
      final ITableViewColumn col = (ITableViewColumn) it.next();
      allAxes.add( col.getAxis() );
      
      final IAxis statusAxis = getStatusAxis( col );
      if( statusAxis != null )
      {
        allAxes.add( statusAxis );
        statusAxes.put( col, statusAxis );
      }

      // for the last column, add also the key axis
      if( !it.hasNext() )
        allAxes.add( col.getKeyAxis() );
    }

    final SimpleTuppleModel model = new SimpleTuppleModel( allAxes );

    int rowIndex = 0;
    for( final Iterator ite = m_sharedModel.iterator(); ite.hasNext(); )
    {
      final Object keyObj = ite.next();

      if( rows == null
          || (rows != null && Arrays.binarySearch( rows, rowIndex ) >= 0) )
      {
        final Object[] tupple = new Object[ allAxes.size() ];

        for( final Iterator ita = cols.iterator(); ita.hasNext(); )
        {
          final ITableViewColumn col = (ITableViewColumn) ita.next();

          // the col index is the same for the value model and the status model
          final int colIndex = m_valuesModel.findColumn( col.getName() );

          tupple[ col.getAxis().getPosition() ] = m_valuesModel.getValueAt( rowIndex, colIndex );
          
          final IAxis statusAxis = (IAxis) statusAxes.get( col );
          if( statusAxis != null )
            tupple[ statusAxis.getPosition() ] = m_statusModel.getValueAt( rowIndex, colIndex );
          
          // and the key value
          if( !ita.hasNext() )
            tupple[ col.getKeyAxis().getPosition() ] = keyObj;
        }

        model.addTupple( tupple );
      }

      rowIndex++;
    }

    return model;
  }

  /**
   * Removes the columns associated to the given theme
   * 
   * @param theme
   * @throws SensorException
   */
  public void removeTableViewColumns( final ITableViewTheme theme ) throws SensorException
  {
    m_columns.removeAll( theme.getColumns() );
    
    final ArrayList cols = new ArrayList( m_columns );
    clearColumns();
    
    for( Iterator it = cols.iterator(); it.hasNext(); )
      addTableViewColumn( (ITableViewColumn) it.next() );
  }

  /**
   * Removes one column
   * 
   * @param col
   * @throws SensorException
   */
  public void removeTableViewColumn( final ITableViewColumn col ) throws SensorException
  {
    m_columns.remove( col );
    
    final ArrayList cols = new ArrayList( m_columns );
    clearColumns();
    
    for( Iterator it = cols.iterator(); it.hasNext(); )
      addTableViewColumn( (ITableViewColumn) it.next() );
  }
}