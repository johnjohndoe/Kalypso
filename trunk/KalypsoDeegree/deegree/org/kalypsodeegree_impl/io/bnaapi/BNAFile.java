/*----------------    FILE HEADER  ------------------------------------------

 This file has been provided to deegree by
 Emanuele Tajariol e.tajariol@libero.it
 
 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.io.bnaapi;

import java.io.EOFException;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;

/**
 * Class representing a BNA File. Provides methods to load and parse a file in
 * BNA format. Data are returned as a {@link BNAFeatureSet}
 * <p>
 * 
 * @version 2003.08.04
 * @author Emanuele Tajariol
 */
public class BNAFile
{
  /** The featureSet collected from the file. */
  private BNAFeatureSet _fs;

  /**
   * Construct a MainFile from a file name.
   */
  public BNAFile( String url ) throws IOException
  {
    System.out.println( "OPENING BNA FILE " + url );
    _fs = new BNAFeatureSet();

    File file = new File( url );
    RandomAccessFile raf = new RandomAccessFile( file, "r" );
    loadFile( raf );
    raf.close();
  }

  /**
   * Parses a BNA file. TODO: improve parser.
   * 
   * @param raf
   *          a RandomAccessFile
   *  
   */
  private void loadFile( RandomAccessFile raf ) throws IOException
  {
    long len = raf.length();

    while( true )
    {
      long pos = raf.getFilePointer();

      if( pos == len )
      {
        //				System.out.println("EOF - OK - SKIP OUT");
        break;
      }

      //			if(len-pos < 3)
      //			{
      //				System.out.println("TRIMMING OUT FILE TAIL ("+(len-pos)+" bytes
      // left)");
      //				break;
      //			}
      // READ HEADER
      String header = raf.readLine();

      String sPoints; // number of points
      String[] headers; // headers' names
      String[] heads; // temp parsed header

      heads = getHeaderByQuote( header );

      if( heads == null )
      {
        heads = getHeaderByTab( header );
      }
      if( heads == null )
      {
        heads = getHeaderBySpace( header );
      }

      // TODO: try other delimiters
      //if(heads == null)
      // head = getHeaderByXXXX(header);
      //...
      // End of delimiters check. Cant' parse any further. Maybe there is
      // only garbage left.
      if( heads == null )
      {
        // Check to see if remaining chars can be safely skipped
        raf.seek( pos );

        if( !skippable( raf ) )
        {
          System.out
              .println( "Cannot continue parsing file. " + ( len - pos ) + " bytes left out." );
        }

        break;
        // throw new IOException("Header not scanned ("+header+")");
      }

      // unpack heads[]
      headers = new String[heads.length - 1];

      for( int i = 0; i < headers.length; i++ )
        headers[i] = heads[i];

      sPoints = heads[heads.length - 1];

      int vertNum = Integer.parseInt( sPoints.trim() ); // may be negative

      // Build the related feature
      BNAGeometry geom = readGeometry( raf, vertNum );
      BNAFeature feat = new BNAFeature( headers );
      feat.addGeometry( geom );

      _fs.addFeature( feat );
    }
  }

  /**
   * Search for quote-delimited headers We are looking for a header with this
   * format: "name1", "name2", ... , numpoints The split array will be something
   * like this: 0 1 2 3 4 len-1 "A","B",...,n => null / A / , / B / , / ... / , /
   * n
   * 
   * @param header
   *          The header to be parsed
   * 
   * @return a String[] with the parsed headers (last element in array will be
   *         the numpoints) or null if the header is not parsable by quotes.
   *  
   */
  private String[] getHeaderByQuote( String header ) throws IOException
  {
    String[] ret = null;
    String[] heads = header.split( "\"" );

    //for (int i = 0; i < heads.length; i++)
    //	System.out.print(i+") "+heads[i] + " --- ");
    //System.out.println();
    if( ( heads.length >= 5 ) && // we need at least 2 header fields +
        // numpoint
        ( ( heads.length % 2 ) == 1 ) ) // it must be an odd number: i.e:
    // 1"2"3"4"5
    {
      int hnum = ( heads.length - 1 ) / 2;
      ret = new String[hnum + 1];

      for( int i = 0; i < hnum; i++ )
        ret[i] = heads[( i * 2 ) + 1];

      // Parse the numpoint
      String[] next = heads[4].split( "," );

      if( next.length != 2 )
      {
        throw new IOException( "Bad header (" + header + ")" ); // FIXME
      }

      ret[hnum] = next[1];
    }

    return ret;
  }

  /**
   * Search for tab-delimited headers We are looking for a header with this
   * format: name1 TAB name2 TAB ... TAB numpoints
   * 
   * The split array will be something like this: 0 1 len-1 A / B / ... / n
   * 
   * @param header
   *          The header to be parsed
   * 
   * @return a String[] with the parsed headers (last element in array will be
   *         the numpoints) or null if the header is not parsable by tabs.
   */
  private String[] getHeaderByTab( String header ) throws IOException
  {
    String[] heads = header.split( "\t" );

    if( heads.length >= 2 ) // we need at least 2 header fields + numpoint
    {
      return heads; // FIXME Maybe we have to remove bounding quotes
    }
    else
    {
      return null;
    }
  }

  /**
   * Search for space-delimited headers We are looking for a header with this
   * format: name1 TAB name2 TAB ... TAB numpoints
   * 
   * The split array will be something like this: 0 1 len-1 A / B / ... / n
   * 
   * @param header
   *          The header to be parsed
   * 
   * @return a String[] with the parsed headers (last element in array will be
   *         the numpoints) or null if the header is not parsable by tabs.
   */
  private String[] getHeaderBySpace( String header ) throws IOException
  {
    String[] heads = header.split( " " );

    if( heads.length >= 2 ) // we need at least 2 header fields + numpoint
    {
      return heads; // FIXME Maybe we have to remove bounding quotes
    }
    else
    {
      return null;
    }
  }

  /**
   * Checks if remaining chars can be skipped.
   * 
   * @param raf
   *          a RandomAccessFile
   * 
   * @return a boolean
   */
  private boolean skippable( RandomAccessFile raf ) throws IOException
  {
    try
    {
      while( true )
      {
        char ch = (char)raf.readByte();

        if( !Character.isWhitespace( ch ) )
        {
          System.out.println( "Char " + (int)ch + " @" + raf.getFilePointer()
              + " is not a whitespace." );
          return false;
        }
      }
    }
    catch( EOFException e )
    {
      return true;
    }
  }

  /**
   * Reads n points from a file. This methods reads <I>n </I> line from file
   * <I>raf </I>. In each line there's a pair of coords, comma- or
   * tab-separated.
   * 
   * @param raf
   *          The input file
   * @param n
   *          Number of points (file lines) to be read
   * 
   * @return a BNAGeometry built from the points read from the file.
   * 
   * @exception IOException
   *  
   */
  private BNAGeometry readGeometry( RandomAccessFile raf, int n ) throws IOException
  {
    BNAGeometry ret = new BNAGeometry( n < -1 );

    if( n < 0 )
    {
      n = -n;
    }

    for( int i = 0; i < n; i++ )
    {
      String line = raf.readLine();
      String[] coords = line.split( ",|\t" );

      if( coords.length != 2 )
      {
        throw new IOException( "Bad coords (" + line + ")" );
      }

      GM_Position point = GeometryFactory.createGM_Position( Double.parseDouble( coords[0] ),
          Double.parseDouble( coords[1] ) );
      ret.addPoint( point );
    }

    return ret;
  }

  /**
   * 
   * 
   * @return
   */
  public BNAFeatureSet getFeatureSet()
  {
    return _fs;
  }

  /**
   * Just for some debug
   */
  public static void main( String[] args ) throws IOException
  {
    String filename = "/home/fao/project/deegree/runningstuff/deegreewms/WEB-INF/data/bna/Worad1.bna";

    BNAFile bf = new BNAFile( filename );

    for( int i = 0; i < bf.getFeatureSet().size(); i++ )
    {
      System.out.println( "FEAT " + i + ": " + bf.getFeatureSet().getFeature( i ) );
    }
  }
}