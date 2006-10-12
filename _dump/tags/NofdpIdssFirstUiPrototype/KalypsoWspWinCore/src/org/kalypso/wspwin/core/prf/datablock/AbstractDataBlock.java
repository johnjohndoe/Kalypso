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
package org.kalypso.wspwin.core.prf.datablock;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Locale;

/**
 * @author KimWerner
 */
public abstract class AbstractDataBlock implements IDataBlock
{
  protected final DataBlockHeader m_dataBlockHeader;



  public AbstractDataBlock( final DataBlockHeader dbh )
  {
    m_dataBlockHeader = dbh;
  }
  public DataBlockHeader getDataBlockHeader( )
  {
    return m_dataBlockHeader;
  }
  public abstract void readFromReader( final int count, final BufferedReader reader ) throws IOException;

  public abstract void printToPrinter( final PrintWriter pw ) throws IOException;
  public static final String formatDouble( final double d )
  {
    return String.format( Locale.US, " 0 %12.4f", new Object[] { new Double( d ) } );
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getFirstLine()
   */
  public String getFirstLine( )
  {
        return m_dataBlockHeader.getFirstLine();
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getSecondLine()
   */
  public String getSecondLine( )
  {
    return m_dataBlockHeader.getSecondLine();
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#getThirdLine()
   */
  public String getThirdLine( )
  {
    return m_dataBlockHeader.getThirdLine();
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#setSecondLine(java.lang.String)
   */
  public void setSecondLine( String text )
  {
    m_dataBlockHeader.setSecondLine(text);
    
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#setThirdLine(java.lang.String)
   */
  public void setThirdLine( String text )
  {
    m_dataBlockHeader.setThirdLine(text);
  }
  /**
   * @see org.bce.wspm.core.prf.datablock.IDataBlock#setFirstLine(java.lang.String)
   */
  public void setFirstLine( String text )
  {
    m_dataBlockHeader.setFirstLine(text);
    
  }

  
}