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
package org.kalypso.wspwin.core.prf.datablock;

import java.io.BufferedReader;
import java.io.EOFException;
import java.io.IOException;
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.kalypso.wspwin.core.i18n.Messages;

/**
 * @author belger
 */
public class CoordDataBlock extends AbstractDataBlock
{
  private static NumberFormat DF = NumberFormat.getNumberInstance( Locale.US );

  static
  {
    DF.setMaximumIntegerDigits( 8 );
    DF.setMinimumIntegerDigits( 8 );
    DF.setMaximumFractionDigits( 4 );
    DF.setMinimumFractionDigits( 4 );
  }

  private final static Logger m_logger = Logger.getLogger( CoordDataBlock.class.getName() );

  private Double[] m_xs = new Double[0];

  private Double[] m_ys = new Double[0];

  public CoordDataBlock( final DataBlockHeader dbh )
  {
    super( dbh );
  }

  /**
   * Muss von allen Implementatoren zuerst aufgerufen werden
   */
  @Override
  public final void readFromReader( final int count, final BufferedReader reader )
  {
    setCoords( readDoubleBlock( count, reader ), readDoubleBlock( count, reader ) );
  }

  @Override
  public Double[] getX( )
  {
    return m_xs;
  }

  @Override
  public Double[] getY( )
  {
    return m_ys;
  }

  private static Double[] readDoubleBlock( final int count, final BufferedReader reader )
  {
    final Double[] coords = new Double[count];

    int counter = 0;

    while( true )
    {
      final StringTokenizer sT;
      try
      {
        final String readLine = reader.readLine();
        if( readLine == null )
          throw new EOFException( Messages.getString( "org.kalypso.wspwin.core.prf.datablock.CoordDataBlock.0" ) ); //$NON-NLS-1$

        sT = new StringTokenizer( readLine );
      }
      catch( final IOException e )
      {
        // TODO: ist das gut, vielleicht doch lieber ne exception raus werfen und das ganze profil
        // verwerfen??
        // aber es hilft beim Einlesen von Geokoordinaten mit z.B. Schrottrauheiten(nur der Datanblock wird verworfen)
        m_logger.log( Level.SEVERE, e.getLocalizedMessage() );
        return coords;
      }

      if( count < 1 )
        return coords;

      String dblStr = ""; //$NON-NLS-1$
      final int ci = (sT.countTokens() / 2);

      if( ci < 4 && counter + ci < count )
      {
        m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.wspwin.core.prf.datablock.CoordDataBlock.1" ) ); //$NON-NLS-1$
        return coords;
      }

      for( int i = 0; i < ci; i++ )
      {
        try
        {
          sT.nextToken();// Zero vor dem Double ¸berspringen
          dblStr = sT.nextToken();

          coords[counter] = Double.parseDouble( dblStr );
        }
        catch( final NoSuchElementException e )
        {
          coords[counter] = 0.0;
          m_logger.log( Level.SEVERE, Messages.getString( "org.kalypso.wspwin.core.prf.datablock.CoordDataBlock.2", dblStr, counter + 1, e.getLocalizedMessage() ) ); //$NON-NLS-1$
        }
        counter++;
        if( counter == count )
          return coords;
      }
    }
  }

  @Override
  public void printToPrinter( final PrintWriter pw )
  {
    getDataBlockHeader().printToPrinter( pw );
    writeDoubleBlock( m_xs, pw );
    writeDoubleBlock( m_ys, pw );
  }

  /**
   * Schreibt einen Koordinatenblock raus
   * 
   * @param dbls
   *          -
   * @param pw
   */
  private void writeDoubleBlock( final Double[] dbls, final PrintWriter pw )
  {
    for( int i = 0; i < dbls.length; i++ )
    {
      pw.write( formatDouble( dbls[i] == null ? Double.NaN : dbls[i] ) );

      if( (i + 1) % 8 == 0 & i != dbls.length - 1 )
        pw.println();
    }

    pw.println();
  }

  @Override
  public int getCoordCount( )
  {
    return m_xs.length;
  }

  public final void setCoords( final Double[] xs, final Double[] ys )
  {
    if( xs == null | ys == null | xs.length != ys.length )
      throw new IllegalArgumentException();

    m_xs = xs;
    m_ys = ys;
  }
  
  @Override
  public String[] getText( )
  {
    return new String[] { m_xs.toString(), m_ys.toString() };
  }
}