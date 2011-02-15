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
package org.kalypso.optimize.errorfunctions;

import java.util.Arrays;
import java.util.Date;
import java.util.SortedMap;

import org.eclipse.core.runtime.Assert;

/**
 * @author doemming
 */
public class FunctionMultiError extends IErrorFunktion
{
  private final IErrorFunktion[] m_functions;

  private boolean m_normalized = false;

  public FunctionMultiError( final SortedMap<Date, Double> measuredTS, final Date startCompare, final Date endCompare, final IErrorFunktion[] errorFunktions )
  {
    super( measuredTS, startCompare, endCompare );
    m_functions = errorFunktions;

    Assert.isTrue( errorFunktions.length > 0 );
  }

  @Override
  public double calculateError( final SortedMap<Date, Double> calcedTS )
  {
    // first time normalize offsets
    if( !m_normalized )
    {
      normalizeOffset( calcedTS );
      m_normalized = true;
    }
    return calculateInnerError( calcedTS );
  }

  public void normalizeOffset( final SortedMap<Date, Double> calcedTS )
  {
    final double error[] = new double[m_functions.length];
    final boolean valid[] = new boolean[m_functions.length];
    Arrays.fill( valid, true );
    for( int i = 0; i < m_functions.length; i++ )
    {
      final IErrorFunktion function = m_functions[i]; // FIXME: calculateError is called twice per function, is this really necessary? Normalization should happen in this implementation 
      final double calculateError = function.calculateError( calcedTS );
      if( !Double.isInfinite( calculateError ) && !Double.isNaN( calculateError ) )
        error[i] = calculateError;
      else
      {
        error[i] = 0;
        valid[i] = false;
      }
    }

    final double maxError = org.kalypso.contribs.java.util.Arrays.findMax( error );
    for( int i = 0; i < m_functions.length; i++ )
    {
      final IErrorFunktion function = m_functions[i];
      if( valid[i] )
        function.setNormalizeOffset( maxError - error[i] );
      else
        function.setNormalizeOffset( 0 );
    }
  }

  private double calculateInnerError( final SortedMap<Date, Double> calcedTS )
  {
    double c = 0;
    double error = 0;

    for( final IErrorFunktion function : m_functions )
    {
      try
      {
        final double calculateError = function.calculateError( calcedTS );
        if( !Double.isInfinite( calculateError ) && !Double.isNaN( calculateError ) )
        {
          error += Math.pow( calculateError, 2d );
          c++;
        }
        else
          System.out.println( "errorfunction " + function.toString() + " is invalid" );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
    return Math.sqrt( error / c ) + getNormalizeOffset();
  }
}