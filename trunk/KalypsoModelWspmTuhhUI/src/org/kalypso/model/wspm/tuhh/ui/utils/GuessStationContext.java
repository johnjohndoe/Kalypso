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
package org.kalypso.model.wspm.tuhh.ui.utils;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;

/**
 * @author Gernot Belger
 *
 */
public class GuessStationContext
{
  public static final GuessStationContext[] DEFAULT_SEARCH_CONTEXTS = new GuessStationContext[] {
    //
    new GuessStationContext( null, '+', 3 ), //
    new GuessStationContext( null, '-', 3 ), //
    new GuessStationContext( null, '.', 0 ), //
    new GuessStationContext( null, ',', 0 ) //
  };

  // Max. UTF, so this separator will not be used
  private static final char NO_SEPARATOR = '\uffff'; //$NON-NLS-1$

  private static final String DIGITS = "\\d"; //$NON-NLS-1$

  private final Character m_thousandSeparator;

  private final Character m_decimalSeparator;

  private final String m_pattern;

  private final DecimalFormat m_decimalFormat;

  private final int m_factor;

  public GuessStationContext( final Character thousandSeparator, final Character decimalSeparator, final int factor )
  {
    m_thousandSeparator = thousandSeparator;
    m_decimalSeparator = decimalSeparator;
    m_factor = factor;
    m_pattern = buildPattern();
    m_decimalFormat = buildFormat();
  }

  private DecimalFormat buildFormat( )
  {
    final DecimalFormat df = new DecimalFormat( "###,###.#" ); //$NON-NLS-1$
    final DecimalFormatSymbols symbols = new DecimalFormatSymbols();

    if( m_thousandSeparator != null )
      symbols.setGroupingSeparator( m_thousandSeparator );
    else
      symbols.setGroupingSeparator( NO_SEPARATOR );

    if( m_decimalSeparator != null )
      symbols.setDecimalSeparator( m_decimalSeparator );
    else
      symbols.setDecimalSeparator( '\uffff' );

    df.setDecimalFormatSymbols( symbols );

    df.setParseBigDecimal( true );
    return df;
  }

  public String getStationPattern( )
  {
    return m_pattern;
  }

  public String buildPattern( )
  {
    final StringBuilder builder = new StringBuilder();
    builder.append( '(' );

    if( m_thousandSeparator != null )
    {
      builder.append( DIGITS ).append( "+" ); //$NON-NLS-1$
      builder.append( "\\" ).append( m_thousandSeparator ); //$NON-NLS-1$
    }

    builder.append( DIGITS ).append( "+" ); //$NON-NLS-1$

    if( m_decimalSeparator == null )
      builder.append( "+" ); //$NON-NLS-1$
    else
    {
      builder.append( "\\" ).append( m_decimalSeparator ); //$NON-NLS-1$
      builder.append( DIGITS ).append( '+' );
    }

    builder.append( ')' );

    return builder.toString();
  }

  public BigDecimal parseStation( final String input )
  {
    try
    {
      final BigDecimal decimal = (BigDecimal) m_decimalFormat.parse( input );
      final BigDecimal decimalAdjusted = decimal.movePointRight( m_factor );
      return decimalAdjusted.setScale( 1, BigDecimal.ROUND_HALF_UP );
    }
    catch( final ParseException e )
    {
      e.printStackTrace();
      return null;
    }
  }
}