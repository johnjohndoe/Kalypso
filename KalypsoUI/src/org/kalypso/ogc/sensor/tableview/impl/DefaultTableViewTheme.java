package org.kalypso.ogc.sensor.tableview.impl;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTheme;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * DefaultTableViewTheme
 * 
 * @author schlienger
 */
public class DefaultTableViewTheme implements ITableViewTheme
{
  private IObservation m_obs = null;
  private IVariableArguments m_args = null;

  private final List m_columns = new ArrayList();
  
  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getObservation()
   */
  public IObservation getObservation( )
  {
    return m_obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getArguments()
   */
  public IVariableArguments getArguments( )
  {
    return m_args;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#getColumns()
   */
  public List getColumns( )
  {
    return m_columns;
  }

  /**
   * Adds a column.
   * 
   * @param column
   */
  public void addColumn( final ITableViewColumn column )
  {
    m_columns.add( column );
  }
  
  /**
   * Removes a column
   * 
   * @param column
   */
  public void removeColumn( final ITableViewColumn column )
  {
    m_columns.remove( column );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTheme#dispose()
   */
  public void dispose( )
  {
    m_columns.clear();
  }

  /**
   * @param obs
   */
  public void setObservation( IObservation obs )
  {
    m_obs = obs;
  }

  /**
   * @param args
   */
  public void setArguments( IVariableArguments args )
  {
    m_args = args;
  }
}
