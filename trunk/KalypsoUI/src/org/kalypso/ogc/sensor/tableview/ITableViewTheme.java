package org.kalypso.ogc.sensor.tableview;

import java.util.List;

import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * ITableViewTheme
 * 
 * @author schlienger
 */
public interface ITableViewTheme
{
  /**
   * @return name of the theme
   */
  public String getName();
  
  /**
   * @return observation on which this theme is based
   */
  public IObservation getObservation();
  
  /**
   * @return [optional] variable arguments that can be used when values are fetched from
   * the observation
   */
  public IVariableArguments getArguments( );
  
  /**
   * @return list of <code>ITableViewColumn</code>
   */
  public List getColumns();

  /**
   * disposes this theme
   */
  public void dispose( );
}
