package org.kalypso.optimize.errorfunctions;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

/**
 * @author doemming
 */
public class FunctionMultiError extends IErrorFunktion
{
  private final List m_functions;

  public FunctionMultiError( TreeMap measuredTS, Date startCompare, Date endCompare,
      double errorDivisor )
  {
    super( measuredTS, startCompare, endCompare, errorDivisor );
    m_functions = new ArrayList();
  }

  public double calculateError( TreeMap calcedTS )
  {
    return calculateError( calcedTS, false );
  }

  public double normalizeWeights( TreeMap calcedTS )
  {
    return calculateError( calcedTS, true );
  }

  private double calculateError( TreeMap calcedTS, boolean normalize )
  {
    int c = 0;
    int error = 0;
    final Iterator iter = m_functions.iterator();
    while( iter.hasNext() )
    {
      final IErrorFunktion function = (IErrorFunktion)iter.next();
      try
      {
        final double calculateError;
        if( normalize )
          calculateError = function.updateErrorDivisor( calcedTS );
        else
          calculateError = function.calculateError( calcedTS );
        if( !Double.isInfinite( calculateError ) && !Double.isNaN( calculateError ) )
        {
          error += Math.pow( calculateError, 2d );
          c++;
        }
        else
          System.out.println("errorfunction "+function.toString()+" is invalid");
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    return Math.sqrt( error / c );
  }

  public void addFunction( IErrorFunktion function )
  {
    m_functions.add( function );
  }
}