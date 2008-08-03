/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */

package org.kalypsodeegree_impl.io.shpapi;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Locale;

/**
 * Class representing a record of the data section of a dBase III/IV file <BR>
 * at the moment only the daata types character ("C") and numeric ("N") are supported
 * <P>
 * <B>Last changes <B>: <BR>
 * 28.04.00 ap: constructor declared and implemented <BR>
 * 28.04.00 ap: method setRecord(ArrayList recData) declared and implemented <BR>
 * 28.04.00 ap: method getDataSection() declared and implemented <BR>
 * 03.05.00 ap: method setRecord(ArrayList recData) modified <BR>
 * 03.05.00 ap: method setRecord(int index, ArrayList recData) declared and implemented <BR>
 * 03.05.00 ap: method getDataSection() modified <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 03.05.2000
 * @author Andreas Poth
 */

public class DBFDataSection
{
  // length of one record in bytes
  private int recordlength = 0;

  private final FieldDescriptor[] m_fieldDesc;

  private final ArrayList<ByteContainer> data = new ArrayList<ByteContainer>();

  private final String m_charset;

  /**
   * @param charset
   *          name of the charset string value will be encoded with.
   * @see String#getBytes(java.lang.String)
   */
  public DBFDataSection( final FieldDescriptor[] fieldDesc, final String charset )
  {
    m_fieldDesc = fieldDesc;
    m_charset = charset;

    // calculate length of the data section
    recordlength = 0;
    for( int i = 0; i < this.m_fieldDesc.length; i++ )
    {

      byte[] fddata = this.m_fieldDesc[i].getFieldDescriptor();

      recordlength += fddata[16];

      fddata = null;

    }

    recordlength++;

  }

  /**
   * method: public setRecord(ArrayList recData) writes a data record to byte array representing the data section of the
   * dBase file. The method gets the data type of each field in recData from fieldDesc wich has been set at the
   * constructor.
   */
  public void setRecord( ArrayList recData ) throws DBaseException
  {
    setRecord( data.size(), recData );
  }

  /**
   * method: public setRecord(int index, ArrayList recData) writes a data record to byte array representing the data
   * section of the dBase file. The method gets the data type of each field in recData from fieldDesc wich has been set
   * at the constructor. index specifies the location of the retrieved record in the datasection. if an invalid index is
   * used an exception will be thrown
   */
  public void setRecord( int index, ArrayList recData ) throws DBaseException
  {
    ByteContainer datasec = new ByteContainer( recordlength );

    if( (index < 0) || (index > data.size()) )
      throw new DBaseException( "invalid index: " + index );

    if( recData.size() != this.m_fieldDesc.length )
      throw new DBaseException( "invalid size of recData" );

    int offset = 0;

    datasec.data[offset] = 0x20;

    offset++;

    // write every field on the ArrayList to the data byte array
    for( int i = 0; i < recData.size(); i++ )
    {
      final byte[] fddata = this.m_fieldDesc[i].getFieldDescriptor();
      final Object recdata = recData.get( i );
      switch( fddata[11] )
      {
        case (byte) 'C':
        {
          if( recdata != null && !(recdata instanceof String) )
            throw new DBaseException( "invalid data type at field: " + i );

          final byte[] b;
          if( recdata == null )
            b = new byte[0];
          else if( m_charset == null )
            b = ((String) recdata).getBytes();
          else
          {
            try
            {
              b = ((String) recdata).getBytes( m_charset );
            }
            catch( final UnsupportedEncodingException e )
            {
              throw new DBaseException( "Unsupported encoding: " + e.getLocalizedMessage() );
            }
          }

          writeEntry( datasec, offset, fddata[16], b, (byte) 0x20 );
        }
          break;

        case (byte) 'N':
        {
          if( recdata != null && !(recdata instanceof Number) )
            throw new DBaseException( "invalid data type at field: " + i );

          final int fieldLength = fddata[16];
          final int decimalcount = fddata[17];

          final byte[] b;
          if( recdata == null )
          {
            b = new byte[fieldLength];
            for( int j = 0; j < fieldLength; j++ )
              b[j] = ' ';
          }
          else
          {
            // todo: performance: would probably be better to create the format-string only once

            final StringBuffer pattern = new StringBuffer( );
            pattern.append( '%' );

            pattern.append( fieldLength );

            // format specifier must correspond to real type of recdata
            if( recdata instanceof Double || recdata instanceof Float )
            {
              // always append decimalcount for float/double value
              pattern.append( '.' );
              pattern.append( decimalcount );
              pattern.append( 'f' );
            }
            else
              pattern.append( 'd' );

            final String format = String.format( Locale.US, pattern.toString(), recdata );
            b = format.getBytes();
          }

          writeEntry( datasec, offset, fieldLength, b, (byte) 0x0 );
        }
          break;

        case (byte) 'L':
        {
          if( recdata != null && !(recdata instanceof Boolean) )
            throw new DBaseException( "invalid data type at field: " + i );

          final Boolean logical = (Boolean) recdata;
          final byte[] b = new byte[1];
          if( logical == null || !logical.booleanValue() )
            b[0] = 'F';
          else
            b[0] = 'T';

          writeEntry( datasec, offset, fddata[16], b, (byte) 0x00 );
        }
          break;
        default:
          throw new DBaseException( "data type not supported" );
      }

      offset += fddata[16];

    }

    // puts the record to the ArrayList (container)
    data.add( index, datasec );

  }

  private void writeEntry( final ByteContainer datasec, final int offset, final int length, final byte[] b, final byte fillByte ) throws DBaseException
  {
    if( b.length > length )
      throw new DBaseException( "Entry contains too many characters " + new String( b ) );

    for( int j = 0; j < b.length; j++ )
      datasec.data[offset + j] = b[j];
    for( int j = b.length; j < length; j++ )
      datasec.data[offset + j] = fillByte;
  }

  /**
   * method: public byte[] getDataSection() returns the data section as a byte array.
   */
  public byte[] getDataSection( )
  {

    // allocate memory for all datarecords on one array + 1 byte
    byte[] outdata = new byte[data.size() * recordlength + 1];

    // set the file terminating byte
    outdata[outdata.length - 1] = 0x1A;

    // get all records from the ArrayList and put it
    // on a single array
    int j = 0;
    for( int i = 0; i < data.size(); i++ )
    {

      ByteContainer bc = data.get( i );

      for( int k = 0; k < recordlength; k++ )
      {
        outdata[j++] = bc.data[k];
      }

    }

    return outdata;

  }

  /**
   * method: public int getNoOfRecords() returns the number of records within the container
   */
  public int getNoOfRecords( )
  {

    return data.size();

  }

}

class ByteContainer
{

  public byte[] data = null;

  public ByteContainer( int size )
  {

    data = new byte[size];

  }

}