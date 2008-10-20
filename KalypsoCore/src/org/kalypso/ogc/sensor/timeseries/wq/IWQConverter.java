package org.kalypso.ogc.sensor.timeseries.wq;

import java.util.Date;

/**
 * IWQConversion
 * 
 * @author schlienger
 */
// TODO: 'Q' and 'W' is hardcoded here, changes this to 'from' and 'to'
public interface IWQConverter
{
  /** Converts from 'from' to 'to'  */
  public double computeW( final Date date, final double Q ) throws WQException;

  /** Converts from 'to' to 'from'  */
  public double computeQ( final Date date, final double W ) throws WQException;
  
  public String getFromType();
  
  public String getToType();
}
