package org.kalypso.optimize.errorfunctions;

import java.util.Date;
import java.util.Iterator;
import java.util.TreeMap;

/**
 * @author doemming
 */
public class FunctionRMSEPeakAndLowFlow extends IErrorFunktion
{
  public FunctionRMSEPeakAndLowFlow( TreeMap measuredTS, Date startCompare, Date endCompare, double errorDivisor,double min, double max  )
  {
    super( measuredTS, startCompare, endCompare, errorDivisor );
    m_min = min;
    m_max = max;
    
  }

  public final static double UNBOUND = -1;

  private final double m_min;

  private final double m_max;

  private static final int STATUS_SEARCH_INTERVALL = 1;

  private static final int STATUS_INSIDE_INTERVALL = 2;

  private static final int STATUS_OUTSIDE_INTERVALL = 3;

  public double calculateError( TreeMap calced )
  {
    int status = STATUS_SEARCH_INTERVALL;
    double error = 0;
    int c = 0;

    double errorAll = 0;
    int cAll = 0;

    final Iterator it_all = calced.keySet().iterator();
    while( it_all.hasNext() )
    {
      final Date dateKey = (Date)it_all.next();
      if( m_startCompare.before( dateKey ) && m_endCompare.after( dateKey ) )
      {
        final double valueCalced = ( (Double)calced.get( dateKey ) ).doubleValue();
        final double valueMeasured = ( (Double)m_measuredTS.get( dateKey ) ).doubleValue();
        try
        {
          if( ( m_min == UNBOUND && valueMeasured <= m_max )
              || ( m_max == UNBOUND && valueMeasured >= m_min )
              || ( m_max != UNBOUND && m_min == UNBOUND && m_min <= valueMeasured && valueMeasured <= m_max ) )
          {
            // valid intervall
            if( status == STATUS_OUTSIDE_INTERVALL )
            {
              // reentering it
              errorAll += Math.sqrt( error / c );
              cAll++;
              error = 0;
              c = 0;
            }
            error += Math.pow( valueCalced - valueMeasured, 2 );
            c++;
            status = STATUS_INSIDE_INTERVALL;
          }
          else
            status = STATUS_OUTSIDE_INTERVALL;
        }
        catch( Exception e )
        {
          status = STATUS_OUTSIDE_INTERVALL;
          e.printStackTrace();
        }
      }
    }
    errorAll += Math.sqrt( error / c );
    cAll++;
    return errorAll /cAll/m_errorDivisor;
  }
}