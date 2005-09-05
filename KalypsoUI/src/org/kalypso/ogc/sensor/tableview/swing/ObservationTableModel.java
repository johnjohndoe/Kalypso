/*--------------- Kalypso-Header --------------------------------------

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
 
 -----------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.tableview.swing;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.Map.Entry;
import java.util.logging.Logger;

import javax.swing.table.AbstractTableModel;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.MultiStatus;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.TableViewColumn;
import org.kalypso.ogc.sensor.tableview.rules.ITableViewRules;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.tableview.rules.RulesFactory;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * TableModel das mit IObservation benutzt werden kann. Kann in eine JTable benutzt werden.
 * <p>
 * Changes made in the table are directly reflected into the observations models.
 * 
 * @author schlienger
 */
public class ObservationTableModel extends AbstractTableModel
{
  private static final RenderingRule[] EMPTY_RENDERING_RULES = new RenderingRule[0];

  /** rendering rules, defaults to some standard rules */
  private ITableViewRules m_rules = RulesFactory.getDefaultRules();

  /** the shared model is sorted, it contains all key values */
  private final TreeSet m_sharedModel = new TreeSet();

  /** it contains all the "value" columns */
  private final List m_columns = new ArrayList();

  /** the shared axis is the axis which is common to all "value" columns */
  private IAxis m_sharedAxis = null;

  private final static Logger m_logger = Logger.getLogger( ObservationTableModel.class.getName() );

  /**
   * Adds a column at the end of the table.
   */
  public void addColumn( final TableViewColumn col ) throws SensorException
  {
    // columns should be in ascending order (since we always add them in that order)
    final Object[] cols = m_columns.toArray();

    // find good position for column (alphabetically sorted)
    int i = Arrays.binarySearch( cols, col, ColOrderComparator.INSTANCE );
    if( i < 0 )
      i = -i - 1;

    addColumn( col, i );
  }

  /**
   * Adds a column to this tablemodel at the given position
   */
  private void addColumn( final TableViewColumn col, int pos ) throws SensorException
  {
    synchronized( m_columns )
    {
      if( col == null || !col.isShown() )
        return;

      final IAxis keyAxis = col.getKeyAxis();

      if( m_sharedAxis == null )
        m_sharedAxis = keyAxis;
      else
      {
        // verify compatibility of the axes. We do not use IAxis.equals()
        // since the position is not relevant here
        if( m_sharedAxis.getDataClass() != keyAxis.getDataClass() || !m_sharedAxis.getUnit().equals( keyAxis.getUnit() )
            || !m_sharedAxis.getType().equals( keyAxis.getType() ) )
        {
          throw new SensorException( m_sharedAxis + " ist nicht mit " + keyAxis + " kompatibel." );
        }
      }

      // values of observation of the column
      final IObservation obs = col.getObservation();
      final IAxis axis = col.getAxis();
      if( obs == null || axis == null )
        return;

      if( !m_columns.contains( col ) )
      {
        m_columns.add( pos, col );

        // adapt width of column
        // TODO: listen for column width changes (initiated by the user) and
        // store it in the template when saving it
        //        final int colIx = m_valuesModel.findColumn( col.getName() );
        //        final int colIx = findColumn( col.getName() );
        //        final TableColumn tableColumn = m_table.getColumnModel().getColumn(
        //            colIx );
        //        tableColumn.setPreferredWidth( col.getWidth() );
      }

      final ITuppleModel tupModel = obs.getValues( col.getArguments() );

      // fill shared column values
      for( int r = 0; r < tupModel.getCount(); r++ )
      {
        final Object elt = tupModel.getElement( r, keyAxis );
        m_sharedModel.add( elt );
      }
    }

    // !WARNING! never fire within synchronized block
    // -> dead-lock!
    fireTableStructureChanged();
  }

  public void refreshColumn( final TableViewColumn column ) throws SensorException
  {
    final int pos = removeColumn( column );
    addColumn( column, pos );
  }

  /**
   * Returns the status axis for the 'normal' axis found in the given column.
   * 
   * @return status axis or null if not found
   */
  private IAxis getStatusAxis( final IObservation obs, final IAxis axis )
  {
    final IAxis[] obsAxes = obs.getAxisList();
    final String statusAxisLabel = KalypsoStatusUtils.getStatusAxisLabelFor( axis );
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

      return ( (TableViewColumn)m_columns.get( columnIndex - 1 ) ).getColumnClass();
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

      return ( (TableViewColumn)m_columns.get( columnIndex - 1 ) ).getName();
    }
  }

  /**
   * @see javax.swing.table.TableModel#getColumnCount()
   */
  public int getColumnCount()
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
  public int getRowCount()
  {
    return m_sharedModel.size();
  }

  /**
   * @see javax.swing.table.TableModel#getValueAt(int, int)
   */
  public Object getValueAt( int rowIndex, int columnIndex )
  {
    try
    {
      final Object key = m_sharedModel.toArray()[rowIndex];

      if( columnIndex == 0 )
        return key;

      try
      {
        final TableViewColumn col = (TableViewColumn)m_columns.get( columnIndex - 1 );
        final ITuppleModel values = col.getObservation().getValues( col.getArguments() );
        final int ix = values.indexOf( key, col.getKeyAxis() );
        if( ix != -1 )
          return values.getElement( ix, col.getAxis() );

        return null;
      }
      catch( final SensorException e )
      {
        e.printStackTrace();
        throw new IllegalStateException( e.getLocalizedMessage() );
      }
    }
    catch( final ArrayIndexOutOfBoundsException e )
    {
      // TRICKY: Ich weiss nicht warum, diese Exception taucht auf wenn die
      // Berechnung im Wizard gestartet wird. Ich denke, es liegt daran, dass
      // im Hintergrund die Zeitreihen aktualisiert werden, der Pool schickt
      // das Event, und die Tabelle versucht sich zu aktualisieren: dabei
      // ist der Model nicht mehr genau in Sync mit die Daten
      // WORKAROUND: wir ignorieren die Exception ganz leise und geben
      // einfach null zurück.
      return null;
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

      return ( (TableViewColumn)m_columns.get( columnIndex - 1 ) ).isEditable();
    }
  }

  /**
   * @see javax.swing.table.AbstractTableModel#setValueAt(java.lang.Object, int, int)
   */
  public void setValueAt( Object aValue, int rowIndex, int columnIndex )
  {
    // TRICKY: if value is null, do nothing (NOTE: the DoubleCellEditor used
    // within our ObservationTable returns null when editing has been cancelled
    // and in our case this means: don't modify the status)
    if( aValue == null )
      return;

    // date column is not editable!
    if( columnIndex == 0 )
      return;

    final TableViewColumn col = (TableViewColumn)m_columns.get( columnIndex - 1 );

    try
    {
      final ITuppleModel values = col.getObservation().getValues( col.getArguments() );
      final Object key = m_sharedModel.toArray()[rowIndex];
      final int ix = values.indexOf( key, col.getKeyAxis() );
      if( ix != -1 )
      {
        // first set status (may be overriden)
        final IAxis statusAxis = getStatusAxis( col.getObservation(), col.getAxis() );
        if( statusAxis != null )
          values.setElement( ix, KalypsoStati.STATUS_USERMOD, statusAxis );

        // then set value
        values.setElement( ix, aValue, col.getAxis() );

        col.setDirty( true );
      }
      else
        m_logger.info( "Cannot setValue because key not found" );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
  }

  /**
   * Sets the rules. Important: you should call this method if you want the rules rendering to be enabled.
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
   * @return rendering rules or empty array if no rules found.
   */
  public RenderingRule[] findRules( int row, int column )
  {
    if( column == 0 )
      return EMPTY_RENDERING_RULES;

    final TableViewColumn col = (TableViewColumn)m_columns.get( column - 1 );
    try
    {
      final ITuppleModel values = col.getObservation().getValues( col.getArguments() );
      final Object key = m_sharedModel.toArray()[row];
      final int ix = values.indexOf( key, col.getKeyAxis() );
      if( ix != -1 )
      {
        final IAxis statusAxis = getStatusAxis( col.getObservation(), col.getAxis() );
        if( statusAxis != null )
        {
          final Number status = (Number)values.getElement( ix, statusAxis );

          if( status != null )
            return m_rules.findRules( status );
        }
      }

      return EMPTY_RENDERING_RULES;
    }
    catch( final SensorException e )
    {
      e.printStackTrace();
      throw new IllegalStateException( e.getLocalizedMessage() );
    }
  }

  /**
   * Clears the columns of the model.
   */
  public void clearColumns()
  {
    synchronized( m_columns )
    {
      m_columns.clear();
      m_sharedModel.clear();
      m_sharedAxis = null;
    }

    fireTableStructureChanged();
  }

  /**
   * Removes one column
   * 
   * @return position of the column that has just been removed
   */
  public int removeColumn( final TableViewColumn col ) throws SensorException
  {
    synchronized( m_columns )
    {
      int pos = m_columns.indexOf( col );

      m_columns.remove( col );

      final ArrayList cols = new ArrayList( m_columns );
      clearColumns();

      for( Iterator it = cols.iterator(); it.hasNext(); )
        addColumn( (TableViewColumn)it.next() );

      return pos;
    }
  }

  /**
   * Returns the corresponding NumberFormat for the given column. The NumberFormat is type dependent. For instance,
   * Water-level is displayed differently than Rainfall.
   * 
   * @return adequate instance of NumberFormat
   * @see org.kalypso.ogc.sensor.timeseries.TimeserieUtils#getNumberFormatFor(String)
   */
  public NumberFormat getNumberFormat( int column )
  {
    if( column == 0 )
      return TimeserieUtils.getNumberFormatFor( m_sharedAxis.getType() );

    synchronized( m_columns )
    {
      final TableViewColumn col = (TableViewColumn)m_columns.get( column - 1 );
      return TimeserieUtils.getNumberFormat( col.getFormat() );
    }
  }

  public IStatus saveDirtyObservations( final IProgressMonitor monitor )
  {
    final MultiStatus status = new MultiStatus( IStatus.OK, KalypsoGisPlugin.getId(), 0, "Zeitreihen speichern" );

    final Map map = getMappedColumns();

    monitor.beginTask( "Zeitreihen speichern", map.size() );

    try
    {
      for( final Iterator it = map.entrySet().iterator(); it.hasNext(); )
      {
        final Map.Entry entry = (Entry)it.next();
        final IObservation obs = (IObservation)entry.getKey();
        final List cols = (List)entry.getValue();

        boolean obsSaved = false;

        for( final Iterator itCols = cols.iterator(); itCols.hasNext(); )
        {
          final TableViewColumn col = (TableViewColumn)itCols.next();

          if( col.isDirty() && !obsSaved )
          {
            try
            {
              KalypsoGisPlugin.getDefault().getPool().saveObject( obs, monitor );

              obsSaved = true;
            }
            catch( final LoaderException e )
            {
              e.printStackTrace();
              status.addMessage( "Fehler beim speichern von " + obs, e );
            }
          }

          col.setDirty( false );
        }

        monitor.worked( 1 );
      }

      return status;
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * Return a map from IObservation to TableViewColumn. Each IObservation is mapped to a list of TableViewColumns which
   * are based on it.
   */
  public Map getMappedColumns()
  {
    final Map map = new HashMap();

    for( final Iterator it = m_columns.iterator(); it.hasNext(); )
    {
      final TableViewColumn col = (TableViewColumn)it.next();
      final IObservation obs = col.getObservation();

      if( !map.containsKey( obs ) )
        map.put( obs, new ArrayList() );

      ( (ArrayList)map.get( obs ) ).add( col );
    }

    return map;
  }

  /**
   * Simple comparator used for ordering the columns in ascending order according to their names.
   * 
   * @author schlienger
   */
  private static class ColOrderComparator implements Comparator
  {
    public final static ColOrderComparator INSTANCE = new ColOrderComparator();

    public int compare( final Object o1, final Object o2 )
    {
      final TableViewColumn col1 = (TableViewColumn)o1;
      final TableViewColumn col2 = (TableViewColumn)o2;

      return col1.getName().compareTo( col2.getName() );
    }
  }
}