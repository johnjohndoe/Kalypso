package org.kalypso.ogc.sensor.tableview.swing;

import java.util.Arrays;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.table.AbstractTableModel;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.DefaultAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable
 * benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel implements ITemplateEventListener
{
  private final static ITableViewColumn[] EMPTY_COLS = new ITableViewColumn[0];

  private ITableViewColumn[] m_columns = EMPTY_COLS;

  private IVariableArguments m_args = null;

  /** common column */
  private Set m_cc = null;

  /** common fake axis */
  private DefaultAxis m_ccAxis = null;

  private ITableViewRules m_rules = null;

  private final Object m_lock = new Object();

  public ObservationTableModel()
  {
  // nix
  }

  /**
   * Constructor with columns. Calls setColumns( ITableViewColumn[] ).
   * 
   * @throws SensorException
   */
  public ObservationTableModel( ITableViewColumn[] columns ) throws SensorException
  {
    setColumns( columns, null );
  }

  /**
   * Adds a column.
   * 
   * @throws SensorException
   */
  public synchronized void addColumn( final ITableViewColumn column ) throws SensorException
  {
    if( m_columns != EMPTY_COLS )
    {
      final Set cols = new HashSet( Arrays.asList( m_columns ) );
      cols.add( column );
      setColumns( (ITableViewColumn[])cols.toArray( new ITableViewColumn[0] ), m_args );
    }
    else
      setColumns( new ITableViewColumn[]
      { column }, m_args );
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
  public void setColumns( final ITableViewColumn[] columns, final IVariableArguments args )
      throws SensorException
  {
    if( columns == null )
      throw new IllegalArgumentException(
          "columns null not allowed. (If you want to clear the table: use clearColumns() instead)" );

    synchronized( m_lock )
    {
      m_columns = columns;
      m_args = args;

      // reset
      m_cc = null;
      m_ccAxis = null;

      if( m_columns != EMPTY_COLS )
      {
        // the common column, merges all the values from the common axes
        m_cc = new TreeSet();

        for( int col = 0; col < m_columns.length; col++ )
        {
          IAxis sharedAxis = m_columns[col].getSharedAxis();

          // create common axis (fake)
          if( m_ccAxis == null )
            m_ccAxis = new DefaultAxis( sharedAxis );

          // verify compatibility of the axes
          if( m_ccAxis.getDataClass() != sharedAxis.getDataClass()
              || !m_ccAxis.getUnit().equals( sharedAxis.getUnit() ) )
            throw new SensorException( m_ccAxis + " ist nicht mit " + sharedAxis + " kompatibel." );

          ITuppleModel sharedModel = m_columns[col].getObservation().getValues( m_args );

          for( int row = 0; row < sharedModel.getCount(); row++ )
            m_cc.add( sharedModel.getElement( row, sharedAxis.getPosition() ) );
        }
      }

      fireTableStructureChanged();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnClass(int)
   */
  public synchronized Class getColumnClass( int columnIndex )
  {
    synchronized( m_lock )
    {
      if( columnIndex == 0 )
        return m_ccAxis.getDataClass();

      return m_columns[columnIndex - 1].getValueAxis().getDataClass();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#getColumnName(int)
   */
  public synchronized String getColumnName( int column )
  {
    synchronized( m_lock )
    {
      if( column == 0 )
        return m_ccAxis.getLabel();

      return m_columns[column - 1].getName();
    }
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public synchronized int getColumnCount()
  {
    synchronized( m_lock )
    {
      if( m_columns == EMPTY_COLS )
        return 0;

      return m_columns.length + 1;
    }
  }

  /**
   * @see javax.swing.table.TableModel#getRowCount()
   */
  public synchronized int getRowCount()
  {
    synchronized( m_lock )
    {
      if( m_cc == null )
        return 0;

      return m_cc.size();
    }
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
    synchronized( m_lock )
    {
      if( columnIndex == 0 )
        return m_cc.toArray()[rowIndex];

      try
      {
        // Retrieve object of common column at given position
        Object obj = m_cc.toArray()[rowIndex];

        // Use this object to retrieve real index from tupple (with shared axis)
        ITuppleModel values = m_columns[columnIndex - 1].getObservation().getValues( m_args );
        int index = values.indexOf( obj, m_columns[columnIndex - 1].getSharedAxis() );

        if( index < 0 )
          return null;

        // Now we can retrieve the element using value axis
        return values.getElement( index, m_columns[columnIndex - 1].getValueAxis().getPosition() );
      }
      catch( SensorException e )
      {
        // TODO: handling
        throw new RuntimeException( e );
      }
    }
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
    try
    {
      // Retrieve object of common column at given position
      Object obj = m_cc.toArray()[rowIndex];

      // Use this object to retrieve real index from tupple (with shared axis)
      ITuppleModel values = m_columns[columnIndex - 1].getObservation().getValues( m_args );
      int index = values.indexOf( obj, m_columns[columnIndex - 1].getSharedAxis() );

      // Now we can retrieve the element using value axis
      values.setElement( index, aValue, m_columns[columnIndex - 1].getValueAxis().getPosition() );
    }
    catch( SensorException e )
    {
      // TODO: handling
      e.printStackTrace();
    }
  }

  /**
   * Sets the rules. Important: you should call this method if you want the
   * rules rendering to be enabled.
   */
  public void setRules( final ITableViewRules rules )
  {
    m_rules = rules;
  }

  /**
   *  
   */
  public RenderingRule[] findRules( int row, int column ) throws NoSuchElementException
  {
    synchronized( m_lock )
    {
      final String kStatusCol = KalypsoStatusUtils.getStatusAxisLabelFor( m_columns[column - 1]
          .getValueAxis() );

      final IAxis valueAxis = findValueAxis( kStatusCol );

      return m_rules.findRules( ( (Integer)getValueAt( row, valueAxis.getPosition() + 1 ) )
          .intValue() );
    }
  }

  /**
   * Finds a value axis with the given name.
   * 
   * @throws NoSuchElementException
   *           when axis could not be found
   */
  private IAxis findValueAxis( final String name ) throws NoSuchElementException
  {
    synchronized( m_lock )
    {
      for( int i = 0; i < m_columns.length; i++ )
      {
        if( m_columns[i].getValueAxis().getLabel().equals( name ) )
          return m_columns[i].getValueAxis();
      }

      throw new NoSuchElementException();
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    try
    {
      if( evt.getType() == TemplateEvent.TYPE_ADD && evt.getObject() instanceof ITableViewColumn )
        addColumn( (ITableViewColumn)evt.getObject() );

      if( evt.getType() == TemplateEvent.TYPE_REMOVE_ALL )
        clearColumns();
    }
    catch( SensorException e )
    {
      throw new RuntimeException( e );
    }
  }

  /**
   * Clears the columns of the model.
   */
  public void clearColumns()
  {
    m_columns = EMPTY_COLS;
    fireTableStructureChanged();
  }
}