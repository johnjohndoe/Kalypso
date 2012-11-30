/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.wspwin.core;

import java.io.IOException;
import java.io.LineNumberReader;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.Locale;

/**
 * @author Gernot Belger
 */
public abstract class AbstractCalculationContentBean implements ICalculationContentBean
{
  private final CalculationBean m_bean;

  public AbstractCalculationContentBean( final CalculationBean bean )
  {
    m_bean = bean;
  }

  @Override
  public CalculationBean getCalculationBean( )
  {
    return m_bean;
  }

  static BigDecimal readBigDecimal( final LineNumberReader lnr ) throws IOException
  {
    final String line = expectNextLine( lnr );
    return line.length() == 0 ? null : new BigDecimal( line );
  }

  static boolean readBoolean( final LineNumberReader lnr ) throws IOException
  {
    return readInt( lnr ) == 1 ? true : false;
  }

  static int readInt( final LineNumberReader lnr ) throws IOException
  {
    final String line = expectNextLine( lnr );
    return line.length() == 0 ? 0 : Integer.parseInt( line );
  }

  /** Returns the empty the, if no more lines are available. */
  static String expectNextLine( final LineNumberReader lnr ) throws IOException
  {
    final String line = lnr.ready() ? lnr.readLine() : null;
    if( line == null )
// // throw new ParseException( "More lines expected", lnr.getLineNumber() );
      return ""; //$NON-NLS-1$

    return line;
  }

  static void printBigDecimal( final PrintWriter pw, final BigDecimal value )
  {
    if( value == null )
      pw.println();
    else
      pw.format( Locale.US, "%s%n", value ); //$NON-NLS-1$
  }
}