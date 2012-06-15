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
package org.kalypso.wspwin.core.prf;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.wspwin.core.i18n.Messages;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

public class DataBlockWriter
{
  private final List<IDataBlock> m_dbs = new ArrayList<IDataBlock>();

  private final Map<Integer, String[]> m_metaMap = new HashMap<Integer, String[]>();

  public void addKeyValue( final int lineNr, final String[] data )
  {
    m_metaMap.put( lineNr, data );
  }

  public void addDataBlock( final IDataBlock dataBlock )
  {
    m_dbs.add( dataBlock );
  }

  public void addDataBlock( final int index, final IDataBlock dataBlock )
  {
    m_dbs.add( index, dataBlock );
  }

  public void store( final PrintWriter pw ) throws IOException
  {
    writeMetadata( pw );
    writeZeile14( pw );
    pw.println( "0.0000  0.0000  0.0000  0.0000  0.0000  0.0000   0 0 0" );// Plotvorgaben //$NON-NLS-1$

    for( final IDataBlock db : m_dbs )
    {
      try
      {
        db.printToPrinter( pw );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
        throw new IOException( Messages.getString("DataBlockWriter_0") + db.getFirstLine(), e ); //$NON-NLS-1$
      }
    }
  }

  /**
   * Schreibt die Anzahlen der Datenblöcke als Zeile 14 raus
   */
  private void writeZeile14( final PrintWriter pw )
  {
    pw.print( m_dbs.size() );
    for( final IDataBlock dataBlock : m_dbs )
    {
      pw.print( ' ' );
      final int i = dataBlock.getCoordCount();
      pw.print( i );
    }
    pw.println();
  }

  private void writeMetadata( final PrintWriter pw )
  {
    for( int i = 1; i < 14; i++ )
    {
      final String[] line = m_metaMap.get( i ) == null ? new String[] { "", "" } : m_metaMap.get( i ); //$NON-NLS-1$ //$NON-NLS-2$
      final StringBuffer buffer = new StringBuffer( "#" ); //$NON-NLS-1$
      buffer.append( line.length > 0 ? line[0] : "" ); //$NON-NLS-1$

      if( buffer.length() > 41 )
        buffer.setLength( 41 );
      else
      {
        final int l = 41 - buffer.length();
        final char[] space = new char[l];
        Arrays.fill( space, ' ' );

        buffer.append( space );
      }
      buffer.append( line.length > 1 ? line[1] : "" ); //$NON-NLS-1$
      pw.println( buffer.toString().trim().substring( 1 ) );
    }
  }
}