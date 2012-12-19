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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;

/**
 * FIXME: separate between blocks with double values and text blocks.
 *
 * @author Gernot Belger
 */
public class LengthSectionTextBlock extends AbstractDataBlock
{
  private Double[] m_xs = null;

  private String[] m_texts = null;

  public LengthSectionTextBlock( final DataBlockHeader dbh )
  {
    super( dbh );

    Assert.isTrue( dbh.getSpecification( 8 ) == 12 );
  }

  @Override
  public final void readFromReader( final int count, final BufferedReader reader )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.wspwin.core.prf.datablock.IDataBlock#getX()
   */
  @Override
  public Double[] getX( )
  {
    return m_xs;
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.ICoordDataBlock#getY()
   */
  @Override
  public Double[] getY( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.AbstractDataBlock#printToPrinterInternal(java.io.PrintWriter)
   */
  @Override
  public void printToPrinter( final PrintWriter pw )
  {
    getDataBlockHeader().printToPrinter( pw );

    pw.write( "1 " + getCoordCount() + " 0.0000" ); //$NON-NLS-1$ //$NON-NLS-2$
    pw.println();
    writeTextBlock( pw );
  }

  private void writeTextBlock( final PrintWriter pw )
  {
    for( int i = 0; i < m_xs.length; i++ )
    {
      pw.write( String.format( "1 0 2 2 %.0f 2", m_xs[i] ) ); //$NON-NLS-1$
      pw.println();
      pw.print( m_texts[i].toString() );
      pw.println();
    }
  }

  /**
   * @see org.kalypso.model.wspm.profileeditor.serializer.datablock.IDataBlock#getCoordCount()
   */
  @Override
  public int getCoordCount( )
  {
    return m_xs.length;
  }

  public final void setCoords( final String[] ys )
  {
    Assert.isNotNull( ys );

    final List<Double> basis = new ArrayList<>();
    final List<String> target = new ArrayList<>();
    for( int i = 0; i < ys.length; i++ )
    {
      if( !StringUtils.isEmpty( ys[i] ) )
      {
        target.add( ys[i] );
        basis.add( new Double( i ) );
      }
    }

    m_xs = basis.toArray( new Double[basis.size()] );
    m_texts = target.toArray( new String[target.size()] );
  }

  @Override
  public String[] getText( )
  {
    throw new UnsupportedOperationException();
  }
}