/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview.swing;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.logging.Logger;

import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableColumn;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.TableViewTheme;
import org.kalypso.ogc.sensor.tableview.rules.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable
 * benutzt werden.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private static final RenderingRule[] EMPTY_RENDERING_RULES = new RenderingRule[0];

  /** status used when user modifies the values */
  private static final Integer STATUS_USERMOD = new Integer(
      KalypsoStati.BIT_USER_MODIFIED );

  /** rendering rules, defaults to some standard rules */
  private ITableViewRules m_rules = RulesFactory.getDefaultRules();

  /** contains the values */
  private final DefaultTableModel m_valuesModel = new DefaultTableModel();

  /** contains the status information */
  private final DefaultTableModel m_statusModel = new DefaultTableModel();

  /** the shared model is sorted, it contains all key values */
  private final TreeSet m_sharedModel = new TreeSet();

  /** it contains all the "value" columns */
  private final List m_columns = Collections.synchronizedList( new ArrayList() );

  /** the shared axis is the axis which is common to all "value" columns */
  private IAxis m_sharedAxis = null;

  /**
   * when this flag is true, the underlying observations are synchronized with
   * the content of this table model
   */
  private boolean m_syncObservation = true;

  /**
   * TRICKY: this map is used to store the modified themes and the observations
   * values in order to perform the calls to IObservation.setValues() after
   * having proceeded the themes using the iterator.
   * <p>
   * Moreover, it is used in refreshColumns() to check if the theme to refresh
   * is a saved theme. When this is true, the theme is ignored because this
   * model was the originator of the event, so it knows the theme is up to date.
   */
  private final Map m_savedThemes = Collections.synchronizedMap( new HashMap() );

  private final Logger m_logger = Logger.getLogger( getClass().getName() );

  private ObservationTable m_table;

  protected void setTable( final ObservationTable table )
  {
    m_table = table;
  }

  /**
   * Sets the flag for synchronizing the observations with the values in this
   * table model. When true, the observations are synchronized in this call, and
   * for each change, they are synchronized (see setValueAt())
   * 
   * @param b
   */
  public void setSynchronizeObservations( final boolean b )
  {
    m_syncObservation = b;

    if( m_syncObservation )
      try
      {
        saveToObservations();
      }
      catch( SensorException e )
      {
        e.printStackTrace();
      }
  }

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
  public void addColumn( final TableViewColumn col ) throws SensorException
  {
    synchronized( m_columns )
    {
      if( col == null )
        return;

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
      if( obs == null )
        return;

      if( !m_columns.contains( col ) )
      {
        m_columns.add( col );

        // add tablecolumn to tablemodel
        m_valuesModel.addColumn( col.getName() );

        // adapt width of column
        // TODO: listen for column width changes (initiated by the user) and
        // store it in the template when saving it
        final int colIx = m_valuesModel.findColumn( col.getName() );
        final TableColumn tableColumn = m_table.getColumnModel().getColumn(
            colIx );
        tableColumn.setPreferredWidth( col.getWidth() );

        // add tablecolumn to status model
        final IAxis statusAxis = getStatusAxis( col );
        if( statusAxis != null )
          m_statusModel.addColumn( statusAxis.getName() );
        else
          m_statusModel.addColumn( "no-status" );
      }

      final ITuppleModel tupModel = obs.getValues( col.getTheme()
          .getArguments() );

      // fill shared column values
      for( int r = 0; r < tupModel.getCount(); r++ )
      {
        final Object elt = tupModel.getElement( r, keyAxis );
        m_sharedModel.add( elt );
      }

      if( m_sharedModel.size() > m_valuesModel.getRowCount() )
      {
        m_valuesModel.setRowCount( m_sharedModel.size() );
        m_statusModel.setRowCount( m_sharedModel.size() );
      }

      fillValues();

      fireTableStructureChanged();
    }
  }

  /**
   * Helper for filling the underlying table model with the values from the
   * observations
   * 
   * @throws SensorException
   */
  private void fillValues( ) throws SensorException
  {
    for( final Iterator itCol = m_columns.iterator(); itCol.hasNext(); )
    {
      final TableViewColumn tCol = (TableViewColumn) itCol.next();
      final IObservation obs = tCol.getTheme().getObservation();

      if( obs == null )
        continue;

      final ITuppleModel tupModel = obs.getValues( tCol.getTheme()
          .getArguments() );

      final int colIndex = m_valuesModel.findColumn( tCol.getName() );
      if( colIndex == -1 )
      {
        m_logger.warning( "Could not find column: " + tCol );
        continue;
      }

      final IAxis statusAxis = getStatusAxis( tCol );
      final IAxis keyAxis = tCol.getKeyAxis();

      // fill valued column values
      int r = 0;
      for( final Iterator it = m_sharedModel.iterator(); it.hasNext(); )
      {
        final Object sharedElement = it.next();

        final int index = tupModel.indexOf( sharedElement, keyAxis );
        if( index != -1 )
        {
          final Object element = tupModel.getElement( index, tCol.getAxis() );
          m_valuesModel.setValueAt( element, r, colIndex );

          if( statusAxis != null )
            m_statusModel.setValueAt( tupModel.getElement( index, statusAxis ),
                r, colIndex );
        }
        else
          m_valuesModel.setValueAt( null, r, colIndex );

        r++;
      }
    }
  }

  /**
   * Adds all columns of a theme
   * 
   * @param theme
   * @throws SensorException
   */
  public void addColumnsFor( final TableViewTheme theme )
      throws SensorException
  {
    final Iterator it = theme.getColumns().iterator();
    while( it.hasNext() )
    {
      final TableViewColumn col = (TableViewColumn) it.next();
      addColumn( col );
    }
  }

  /**
   * Refresh columns
   * 
   * @param theme
   * @throws SensorException
   */
  public void refreshColumns( final TableViewTheme theme )
      throws SensorException
  {
    synchronized( m_savedThemes )
    {

      if( m_savedThemes.containsKey( theme ) )
      {
        m_savedThemes.remove( theme );
        return;
      }

      final List cols = theme.getColumns();

      for( final Iterator it = cols.iterator(); it.hasNext(); )
      {
        final TableViewColumn col = (TableViewColumn) it.next();

        if( m_columns.contains( col ) )
          removeColumn( col );

        addColumn( col );
      }
    }
  }

  /**
   * Returns the status axis for the 'normal' axis found in the given column.
   * 
   * @param col
   * @return status axis or null if not found
   */
  private IAxis getStatusAxis( final TableViewColumn col )
  {
    final IAxis[] obsAxes = col.getTheme().getObservation().getAxisList();
    final String statusAxisLabel = KalypsoStatusUtils
        .getStatusAxisLabelFor( col.getAxis() );
    final IAxis statusAxis = ObservationUtilities.findAxisByNameNoEx( obsAxes,
        statusAxisLabel );

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

      return ((TableViewColumn) m_columns.get( columnIndex - 1 ))
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

      try
      {
        return m_valuesModel.getValueAt( rowIndex, columnIndex - 1 );
      }
      catch( ArrayIndexOutOfBoundsException e )
      {
        // TRICKY: Ich weiss nicht warum, diese Exception taucht auf wenn die
        // Berechnung im Wizard gestartet wird. Ich denke, es liegt daran, dass
        // im Hintergrund die Zeitreihen aktualisiert werden, der Pool schickt
        // das Event, und die Tabelle versucht sich zu aktualisieren: dabei
        // ist der Model nicht mehr genau in Sync mit die Daten
        // WORKAROUND: wir ignorieren die Exception ganz leise und geben
        // einfach null zur�ck.
        return null;
      }
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

      return ((TableViewColumn) m_columns.get( columnIndex - 1 )).isEditable();
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
      final TableViewColumn col = (TableViewColumn) m_columns
          .get( columnIndex - 1 );
      col.setDirty( true );

      m_statusModel.setValueAt( STATUS_USERMOD, rowIndex, columnIndex - 1 );
      m_valuesModel.setValueAt( aValue, rowIndex, columnIndex - 1 );

      if( m_syncObservation )
        try
        {
          saveToObservations();
        }
        catch( SensorException e )
        {
          e.printStackTrace();
        }
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
    synchronized( m_columns )
    {
      m_columns.clear();
      m_sharedModel.clear();
      m_sharedAxis = null;
      m_valuesModel.setColumnCount( 0 );
      m_statusModel.setColumnCount( 0 );

      fireTableStructureChanged();
    }
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
        final TableViewColumn col = (TableViewColumn) it.next();

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
   * Saves the underlying observations with the content of this table model, as
   * long as the columns have the dirty flag set to true. Note that this does
   * not save to the files, but rather to the observation models.
   * 
   * @throws SensorException
   */
  public void saveToObservations( ) throws SensorException
  {
    synchronized( m_savedThemes )
    {

      for( final Iterator it = m_columns.iterator(); it.hasNext(); )
      {
        final TableViewColumn column = (TableViewColumn) it.next();

        if( column.isDirty() )
        {
          final TableViewTheme theme = column.getTheme();

          for( final Iterator itcol = theme.getColumns().iterator(); itcol
              .hasNext(); )
            ((TableViewColumn) itcol.next()).setDirty( false );

          final ITuppleModel values = getValues( theme );

          m_savedThemes.put( theme, values );
        }
      }

      // iterate over the modified themes and save the observations
      for( final Iterator it = m_savedThemes.keySet().iterator(); it.hasNext(); )
      {
        final TableViewTheme theme = (TableViewTheme) it.next();
        final ITuppleModel values = (ITuppleModel) m_savedThemes.get( theme );

        theme.getObservation().setValues( values );
      }
    }
  }

  /**
   * Creates an ITuppleModel with the values of the columns of the given theme.
   * 
   * @param theme
   * @return new model
   * @throws SensorException
   */
  public ITuppleModel getValues( final TableViewTheme theme )
      throws SensorException
  {
    return getValues( theme.getColumns(), null );
  }

  /**
   * Creates an ITuppleModel with the values of all columns and the given rows.
   * 
   * @param rows
   * @return new model
   * @throws SensorException
   */
  public ITuppleModel getValues( final int[] rows ) throws SensorException
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
   * @throws SensorException
   */
  protected ITuppleModel getValues( final List cols, final int[] rows )
      throws SensorException
  {
    final List allAxes = new ArrayList();
    final Map statusAxes = new HashMap();

    final Iterator it = cols.iterator();
    while( it.hasNext() )
    {
      final TableViewColumn col = (TableViewColumn) it.next();
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
        final Object[] tupple = new Object[allAxes.size()];

        for( final Iterator ita = cols.iterator(); ita.hasNext(); )
        {
          final TableViewColumn col = (TableViewColumn) ita.next();

          // the col index is the same for the value model and the status model
          final int colIndex = m_valuesModel.findColumn( col.getName() );

          // value
          // TODO: do something if the value is null else
          // it is a problem when loading the zml back
          final Object value = m_valuesModel.getValueAt( rowIndex, colIndex );
          tupple[model.getPositionFor( col.getAxis() )] = value;

          // status
          final IAxis statusAxis = (IAxis) statusAxes.get( col );
          if( statusAxis != null )
          {
            final Object status = m_statusModel.getValueAt( rowIndex, colIndex );
            tupple[model.getPositionFor( statusAxis )] = status;
          }

          // and the key value
          if( !ita.hasNext() )
            tupple[model.getPositionFor( col.getKeyAxis() )] = keyObj;
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
  public void removeColumnsFor( final TableViewTheme theme )
      throws SensorException
  {
    m_columns.removeAll( theme.getColumns() );

    final ArrayList cols = new ArrayList( m_columns );
    clearColumns();

    for( Iterator it = cols.iterator(); it.hasNext(); )
      addColumn( (TableViewColumn) it.next() );
  }

  /**
   * Removes one column
   * 
   * @param col
   * @return position of the column that has just been removed
   * @throws SensorException
   */
  public int removeColumn( final TableViewColumn col ) throws SensorException
  {
    int pos = m_columns.indexOf( col );

    m_columns.remove( col );

    final ArrayList cols = new ArrayList( m_columns );
    clearColumns();

    for( Iterator it = cols.iterator(); it.hasNext(); )
      addColumn( (TableViewColumn) it.next() );

    return pos;
  }

  /**
   * Returns the corresponding NumberFormat for the given column. The
   * NumberFormat is type dependent. For instance, Water-level is displayed
   * differently than Rainfall.
   * 
   * @param column
   * @return adequate instance of NumberFormat
   * @see org.kalypso.ogc.sensor.timeseries.TimeserieUtils#getNumberFormatFor(String)
   */
  public NumberFormat getNumberFormat( int column )
  {
    final TableViewColumn col = (TableViewColumn) m_columns.get( column - 1 );
    return TimeserieUtils.getNumberFormatFor( col.getAxis().getType() );
  }
}