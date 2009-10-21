/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.tuhh.core.wspwin.strang;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;

import org.apache.commons.lang.NotImplementedException;
import org.kalypso.contribs.java.lang.NumberUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.serializer.IStrangSink;
import org.kalypso.wspwin.core.prf.PrfWriter;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;

import au.com.bytecode.opencsv.CSVReader;

/**
 * @author kimwerner
 */
public class LngSink implements IStrangSink
{

  @SuppressWarnings("unchecked")
  private void extractDataBlocks( final PrfWriter pw, final Reader reader, final int colStation ) throws IOException
  {

    final CSVReader tableReader = new CSVReader( reader, ';' );
    final String[] cols = tableReader.readNext();
    final Object[] table = new Object[cols.length];
    for( int i = 0; i < cols.length; i++ )
    {
      table[i] = new ArrayList<Double>();
    }
    String[] values = tableReader.readNext();
    while( values != null )
    {
      if( values.length == cols.length )
      {
        for( int i = 0; i < cols.length; i++ )
        {
          ((ArrayList<Double>) table[i]).add( NumberUtils.parseQuietDouble( values[i] ) );
        }
        values = tableReader.readNext();
      }
    }
    for( int i = 0; i < cols.length; i++ )
    {
      if( i != colStation )
      {
        final DataBlockHeader dbh = PrfWriter.createHeader( cols[i] ); //$NON-NLS-1$
        final CoordDataBlock block = new CoordDataBlock( dbh );
        block.setCoords( ((ArrayList<Double>) table[colStation]).toArray( new Double[] {} ), ((ArrayList<Double>) table[i]).toArray( new Double[] {} ) );
        pw.addDataBlock( block );
      }
    }
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil)
   */
  public void write( final String source, final String destination )
  {
    PrintWriter pw = null;
    try
    {
      pw = new PrintWriter( new FileOutputStream( new File( destination ) ) );
    }
    catch( final FileNotFoundException e1 )
    {
      // TODO Auto-generated catch block
      e1.printStackTrace();
    }

    final PrfWriter prfwriter = new PrfWriter();
    try
    {
      extractDataBlocks( prfwriter, new FileReader( source ), 0 );
    }
    catch( final FileNotFoundException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    prfwriter.store( pw );
  }

  // TODO Kim: kann das nicht weg?

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IStrangSink#write(org.kalypso.model.wspm.core.profil.IProfil[],
   *      java.io.Writer)
   */
  public void write( final IProfil[] profiles, final Writer writer )
  {
    throw new NotImplementedException();
  }

  /**
   * @see org.kalypso.model.wspm.core.profil.serializer.IProfilSink#write(org.kalypso.model.wspm.core.profil.IProfil,
   *      java.io.Writer)
   */
  public void write( final IProfil profil, final Writer writer )
  {
    throw new NotImplementedException();
  }

}
