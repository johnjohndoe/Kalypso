package org.kalypso.optimize.errorfunctions;

import java.util.Date;
import java.util.Iterator;
import java.util.TreeMap;

/**
 * @author doemming
 */
public class FunctionVolumeError extends IErrorFunktion
{

  public FunctionVolumeError( TreeMap measuredTS, Date startCompare, Date endCompare,
      double errorDivisor )
  {
    super( measuredTS, startCompare, endCompare, errorDivisor );
  }

  public double calculateError( TreeMap calced )
  {
    double error = 0;
    double c = 0;
    Iterator it_all = calced.keySet().iterator();
    while( it_all.hasNext() )
    {
      Date dateKey = (Date)it_all.next();
      if( m_startCompare.before( dateKey ) && m_endCompare.after( dateKey ) )
      {
        try
        {
          final double valueCalced = ( (Double)calced.get( dateKey ) ).doubleValue();
          final double valueMeasured = ( (Double)m_measuredTS.get( dateKey ) ).doubleValue();
          error += valueCalced - valueMeasured;
          c++;
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }
    return Math.abs( error / c / m_errorDivisor);
  }
}