package org.kalypso.optimize.errorfunctions;

import java.util.Date;
import java.util.Iterator;
import java.util.TreeMap;

/**
 * @author doemming
 */
public class FunctionRMSError extends IErrorFunktion
{
  
  public FunctionRMSError( TreeMap measuredTS, Date startCompare, Date endCompare, double errorDivisor )
  {
    super( measuredTS, startCompare, endCompare, errorDivisor );
    
  }
  public double calculateError( TreeMap calced)
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
          error += Math.pow( valueCalced - valueMeasured, 2d );
          c++;
        }
        catch( Exception e )
        {
          e.printStackTrace();  
        }
      }
    }
    return Math.sqrt( error / c )/m_errorDivisor;
  }
}