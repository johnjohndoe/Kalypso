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
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;

/**
 * @author kimwerner
 */
public class TextDataBlock extends AbstractDataBlock
{
  private final Collection<String> m_lines = new ArrayList<>();

  public TextDataBlock( final DataBlockHeader dbh )
  {
    super( dbh );
  }

  @Override
  public void printToPrinter( final PrintWriter pw )
  {
    getDataBlockHeader().printToPrinter( pw );
    for( final String s : m_lines )
      pw.println( s );
  }

  @Override
  public void readFromReader( final int count, final BufferedReader reader ) throws IOException
  {
    m_lines.add( reader.readLine() );
    for( int i = 1; i < count; i++ )
    {
      m_lines.add( reader.readLine() );
    }

  }

  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getCoordCount()
   */
  @Override
  public int getCoordCount( )
  {
    return m_lines.size();
  }

  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getX()
   */
  @Override
  public Double[] getX( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getY()
   */
  @Override
  public Double[] getY( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getText()
   */
  @Override
  public String[] getText( )
  {
    return m_lines.toArray(new String[0]);
  }

  public void addLine( final String line )
  {
    m_lines.add( line );
  }

}
