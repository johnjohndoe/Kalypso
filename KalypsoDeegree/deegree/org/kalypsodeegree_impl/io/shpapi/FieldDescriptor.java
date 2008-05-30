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

/**
 * Class representing a field descriptor of a dBase III/IV file
 * <P>
 * <B>Last changes <B>: <BR>
 * 28.04.00 ap: constructor declared and implemented <BR>
 * 28.04.00 ap: method getFieldDescriptor() declared and implemented <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 28.04.2000
 * @author Andreas Poth
 */

public class FieldDescriptor
{
  /**
   * fieldinformation as byte array
   */
  private byte[] data = null;

  /**
   * constructor recieves name and type of the field, the length of the field in bytes and the decimalcount. the
   * decimalcount is only considered if type id "N" or "F", it's maxvalue if fieldlength - 2!
   */
  public FieldDescriptor( final String name, final String type, final byte fieldlength, final byte decimalcount ) throws DBaseException
  {
    if( (!type.equalsIgnoreCase( "C" )) && (!type.equalsIgnoreCase( "D" )) && (!type.equalsIgnoreCase( "F" )) && (!type.equalsIgnoreCase( "N" )) && (!type.equalsIgnoreCase( "M" ))
        && (!type.equalsIgnoreCase( "L" )) )
      throw new DBaseException( "data type is not supported" );

    data = new byte[32];

    // fill first 11 bytes with ASCII zero
    for( int i = 0; i <= 10; i++ )
      data[i] = 0x0;

    // copy name into the first 11 bytes
    final byte[] dum = name.getBytes();

    int cnt = dum.length;

    if( cnt > 11 )
      cnt = 11;

    for( int i = 0; i < cnt; i++ )
      data[i] = dum[i];

    final byte[] b = type.getBytes();

    data[11] = b[0];

    // set fieldlength
    data[16] = fieldlength;

    // set decimalcount
    if( type.equalsIgnoreCase( "N" ) || type.equalsIgnoreCase( "F" ) )
      data[17] = decimalcount;
    else
      data[17] = 0;

    // throw DBaseException if the decimalcount is larger then the
    // number off fields required for plotting a float number
    // as string
    if( data[17] > data[16] - 2 )
      throw new DBaseException( "invalid fieldlength and/or decimalcount" );

    // work area id (don't know if it should be 1)
    data[20] = 1;

    // has no index tag in a MDX file
    data[31] = 0x00;

    // all other fields are reserved!

  }

  /**
   * method: public byte[] getFieldDescriptor() returns the field descriptor as byte array
   */
  public byte[] getFieldDescriptor( )
  {

    return data;

  }

}