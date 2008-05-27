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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;

/**
 * @author doemming
 */
public class FunctionMultiError extends IErrorFunktion
{
  private final List <IErrorFunktion> m_functions;

  private boolean m_normalized = false;

  public FunctionMultiError( TreeMap measuredTS, Date startCompare, Date endCompare )
  {
    super( measuredTS, startCompare, endCompare );
    m_functions = new ArrayList<IErrorFunktion>();
  }

  @Override
  public double calculateError( TreeMap calcedTS )
  {
    // first time normalize offsets
    if( !m_normalized )
    {
      normalizeOffset( calcedTS );
      m_normalized = true;
    }
    return calculateInnerError( calcedTS );
  }

  public void normalizeOffset( TreeMap calcedTS )
  {
    double error[] = new double[m_functions.size()];
    boolean valid[] = new boolean[m_functions.size()];
    Arrays.fill( valid, true );
    for( int i = 0; i < m_functions.size(); i++ )
    {
      final IErrorFunktion function = m_functions.get( i );
      final double calculateError = function.calculateError( calcedTS );
      if( !Double.isInfinite( calculateError ) && !Double.isNaN( calculateError ) )
        error[i] = calculateError;
      else
      {
        error[i] = 0;
        valid[i] = false;
      }
    }
    // find max
    // TODO: what about using Arrays.sort() and error[error.length - 1] ?
    // ask Marc for more information. I believe it must be more performant.
    final double maxError = org.kalypso.contribs.java.util.Arrays.findMax( error );
    for( int i = 0; i < m_functions.size(); i++ )
    {
      final IErrorFunktion function = m_functions.get( i );
      if( valid[i] )
        function.setNormalizeOffset( maxError - error[i] );
      else
        function.setNormalizeOffset( 0 );
    }
  }

  private double calculateInnerError( TreeMap calcedTS )
  {
    double c = 0;
    double error = 0;
    final Iterator iter = m_functions.iterator();
    while( iter.hasNext() )
    {
      final IErrorFunktion function = (IErrorFunktion)iter.next();
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
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
    return Math.sqrt( error / c ) + m_normalizeOffset;
  }

  public void addFunction( IErrorFunktion function )
  {
    m_functions.add( function );
  }
}