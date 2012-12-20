/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 *
 *  and
 *
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  Contact:
 *
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *
 *  ---------------------------------------------------------------------------*/
package org.kalypso.risk.test;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.risk.eval.ExpressionParser;

/**
 * @author antanas
 *
 */
public final class TestFunctionParser
{

  public static void main( final String[] args ) throws Exception
  {

    final List<String> functionsList = new ArrayList<>();
    functionsList.add( "2*x^3 - 8*x^2 + 13*x" ); //$NON-NLS-1$
    functionsList.add( "2*x*x*x-8*x*x+13*x" ); //$NON-NLS-1$
    functionsList.add( "(2*x*x*x) - (8*x*x) + (13*x)" ); //$NON-NLS-1$
    functionsList.add( "2*x^3 - (8*x*x - 13*x)" ); //$NON-NLS-1$

    int cnt = 1;
    System.out.println( "\n\n*** SYNTAX TEST ***" ); //$NON-NLS-1$
    for( final String entry : functionsList )
    {
      final long tStart = System.currentTimeMillis();
      final ExpressionParser ep = new ExpressionParser( entry );
      final double value1 = ep.evaluate( 1.0 );
      final double value2 = ep.evaluate( 2.0 );
      final double value3 = ep.evaluate( 3.0 );
      final long tEnd = System.currentTimeMillis();
      System.out.println( "\n--- Test Nr. " + cnt++ + " -----------" ); //$NON-NLS-1$ //$NON-NLS-2$
      System.out.println( "Function: " + entry ); //$NON-NLS-1$
      System.out.println( "Postfix notation: " + ep.getPostfixExpression() ); //$NON-NLS-1$
      System.out.println( "Result for x=1.0: " + value1 ); //$NON-NLS-1$
      System.out.println( "Result for x=2.0: " + value2 ); //$NON-NLS-1$
      System.out.println( "Result for x=3.0: " + value3 ); //$NON-NLS-1$
      System.out.println( "Elapsed time [ms]: " + (tEnd - tStart) ); //$NON-NLS-1$
    }

    cnt = 1;
    System.out.println( "\n\n*** PERFORMANCE TEST ***" ); //$NON-NLS-1$
    final int NUMBER_OF_CALCULATIONS = 1000000;
    final double[] randoms = new double[NUMBER_OF_CALCULATIONS];
    for( int i = 0; i < NUMBER_OF_CALCULATIONS; i++ )
      randoms[i] = (-0.5 + Math.random()) * 5.0; // Values between -2.5 and 2.5

    for( final String entry : functionsList )
    {
      final ExpressionParser ep = new ExpressionParser( entry );
      System.out.println( "\n--- Test Nr. " + cnt++ + " -----------" ); //$NON-NLS-1$ //$NON-NLS-2$
      System.out.println( "Function: " + entry ); //$NON-NLS-1$
      System.out.println( String.format( "Number of calculations: %d", NUMBER_OF_CALCULATIONS )); //$NON-NLS-1$
      final long tStart = System.currentTimeMillis();
      for( int i = 0; i < NUMBER_OF_CALCULATIONS; i++ )
        ep.evaluate( randoms[i] );
      final long tEnd = System.currentTimeMillis();
      System.out.println( "Elapsed time [ms]: " + (tEnd - tStart) ); //$NON-NLS-1$
    }
    System.out.println( "\n\n*** FINISHED ***\n" ); //$NON-NLS-1$
  }

}
