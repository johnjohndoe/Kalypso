package org.kalypso.ogc.sensor.timeseries.wq;

import java.util.Date;

/**
 * IWQConversion
 * 
 * @author schlienger
 */
public interface IWQConverter
{
  public double computeW( final Date date, final double Q ) throws WQException;

  public double computeQ( final Date date, final double W ) throws WQException;
}
