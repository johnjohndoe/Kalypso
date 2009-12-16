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
import java.io.PrintWriter;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Locale;

/**
 * @author belger
 */
public class LengthSectionDataBlock extends AbstractDataBlock
{
  private static NumberFormat DF = NumberFormat.getNumberInstance( Locale.US );

  static
  {
    DF.setMaximumIntegerDigits( 8 );
    DF.setMinimumIntegerDigits( 8 );
    DF.setMaximumFractionDigits( 4 );
    DF.setMinimumFractionDigits( 4 );
  }

  private Double[] m_xs = new Double[0];

  private Object[] m_ys = new Object[0];

  public LengthSectionDataBlock( DataBlockHeader dbh )
  {
    super( dbh );
  }

  @Override
  public final void readFromReader( final int count, final BufferedReader reader )
  {
    // nothing
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.ICoordDataBlock#getX()
   */
  public Double[] getX( )
  {
    return m_xs;
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.ICoordDataBlock#getY()
   */
  public Double[] getY( )
  {
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.AbstractDataBlock#printToPrinterInternal(java.io.PrintWriter)
   */
  @Override
  public void printToPrinter( final PrintWriter pw )
  {
    m_dataBlockHeader.printToPrinter( pw );
    if( m_dataBlockHeader.getSpecification( 8 ) == 12 )
    {
      pw.write( "1 " + getCoordCount() + " 0.0000" );
      pw.println();
      writeTextBlock( pw );
    }
    else
    {
      writeDoubleBlock( m_xs, pw );
      writeDoubleBlock( m_ys, pw );
    }
  }

  /**
   * Schreibt einen Koordinatenblock raus
   * 
   * @param dbls
   *          -
   * @param pw
   *          -
   */
  private void writeDoubleBlock( final Object[] dbls, final PrintWriter pw )

  {
    for( int i = 0; i < dbls.length; i++ )
    {
      if( dbls[i] instanceof Double )
        pw.write( formatDouble( (Double) dbls[i] ) );
      else
        pw.write( dbls[i].toString() );

      if( (i + 1) % 8 == 0 & i != dbls.length - 1 )
        pw.println();
    }

    pw.println();
  }

  private void writeTextBlock( final PrintWriter pw )

  {
    for( int i = 0; i < m_xs.length; i++ )
    {
      pw.write( String.format( "1 0 2 2 %.0f 2", m_xs[i] ) );
      pw.println();
      pw.print( m_ys[i].toString() );
      pw.println();
    }
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.IDataBlock#getCoordCount()
   */
  public int getCoordCount( )
  {
    return m_xs.length;
  }

  public final void setCoords( final Double[] xs, final Object[] ys )
  {

    if( xs == null | ys == null | xs.length != ys.length )
      throw new IllegalArgumentException();

    final ArrayList<Object> target = new ArrayList<Object>();
    final ArrayList<Double> basis = new ArrayList<Double>();
    for( int i = 0; i < xs.length; i++ )
    {
      if( ys[i] != null && "".equalsIgnoreCase( ys[i].toString() ) == false )
      {
        target.add( ys[i] );
        basis.add( m_dataBlockHeader.getSpecification( 8 ) == 12 ? new Double( i ) : xs[i] );
      }
    }
    m_xs = basis.toArray( new Double[] {} );
    m_ys = target.toArray();
  }

  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getText()
   */
  public String[] getText( )
  {
    return new String[] { m_xs.toString(), m_ys.toString() };
  }
}