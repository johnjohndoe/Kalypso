/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.deegree_impl.tools;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import javax.media.jai.JAI;
import javax.media.jai.RenderedOp;

import com.sun.media.jai.codec.ByteArraySeekableStream;
import com.sun.media.jai.codec.SeekableStream;

/**
 * This class provides encode/decode for RFC 2045 Base64 as defined by RFC 2045,
 * N. Freed and N. Borenstein. <a href="http://www.ietf.org/rfc/rfc2045.txt">RFC
 * 2045 </a>: Multipurpose Internet Mail Extensions (MIME) Part One: Format of
 * Internet Message Bodies. Reference 1996
 * 
 * @author Jeffrey Rodriguez
 * @version $Id$
 */
public final class Base64
{
  static private final int BASELENGTH = 255;

  static private final int LOOKUPLENGTH = 64;

  static private final int TWENTYFOURBITGROUP = 24;

  static private final int EIGHTBIT = 8;

  static private final int SIXTEENBIT = 16;

//  static private final int SIXBIT = 6;

  static private final int FOURBYTE = 4;

  static private final int SIGN = -128;

  static private final byte PAD = (byte)'=';

  static private byte[] base64Alphabet = new byte[BASELENGTH];

  static private byte[] lookUpBase64Alphabet = new byte[LOOKUPLENGTH];

  //static private final Log log =
  // LogSource.getInstance("org.apache.commons.util.Base64");
  static
  {
    for( int i = 0; i < BASELENGTH; i++ )
    {
      base64Alphabet[i] = -1;
    }

    for( int i = 'Z'; i >= 'A'; i-- )
    {
      base64Alphabet[i] = (byte)( i - 'A' );
    }

    for( int i = 'z'; i >= 'a'; i-- )
    {
      base64Alphabet[i] = (byte)( i - 'a' + 26 );
    }

    for( int i = '9'; i >= '0'; i-- )
    {
      base64Alphabet[i] = (byte)( i - '0' + 52 );
    }

    base64Alphabet['+'] = 62;
    base64Alphabet['/'] = 63;

    for( int i = 0; i <= 25; i++ )
      lookUpBase64Alphabet[i] = (byte)( 'A' + i );

    for( int i = 26, j = 0; i <= 51; i++, j++ )
      lookUpBase64Alphabet[i] = (byte)( 'a' + j );

    for( int i = 52, j = 0; i <= 61; i++, j++ )
      lookUpBase64Alphabet[i] = (byte)( '0' + j );

    lookUpBase64Alphabet[62] = (byte)'+';
    lookUpBase64Alphabet[63] = (byte)'/';
  }

  /**
   * 
   * 
   * @param isValidString
   * 
   * @return
   */
  public static boolean isBase64( String isValidString )
  {
    return isArrayByteBase64( isValidString.getBytes() );
  }

  /**
   * 
   * 
   * @param octect
   * 
   * @return
   */
  public static boolean isBase64( byte octect )
  {
    //shall we ignore white space? JEFF??
    return ( octect == PAD || base64Alphabet[octect] != -1 );
  }

  /**
   * 
   * 
   * @param arrayOctect
   * 
   * @return
   */
  public static boolean isArrayByteBase64( byte[] arrayOctect )
  {
    int length = arrayOctect.length;

    if( length == 0 )
    {
      // shouldn't a 0 length array be valid base64 data?
      // return false;
      return true;
    }

    for( int i = 0; i < length; i++ )
    {
      if( !Base64.isBase64( arrayOctect[i] ) )
      {
        return false;
      }
    }

    return true;
  }

  /**
   * Encodes hex octects into Base64.
   * 
   * @param binaryData
   *          Array containing binary data to encode.
   * @return Base64-encoded data.
   */
  public static byte[] encode( byte[] binaryData )
  {
    int lengthDataBits = binaryData.length * EIGHTBIT;
    int fewerThan24bits = lengthDataBits % TWENTYFOURBITGROUP;
    int numberTriplets = lengthDataBits / TWENTYFOURBITGROUP;
    byte[] encodedData = null;

    if( fewerThan24bits != 0 )
    {
      //data not divisible by 24 bit
      encodedData = new byte[( numberTriplets + 1 ) * 4];
    }
    else
    {
      // 16 or 8 bit
      encodedData = new byte[numberTriplets * 4];
    }

    byte k = 0;
    byte l = 0;
    byte b1 = 0;
    byte b2 = 0;
    byte b3 = 0;

    int encodedIndex = 0;
    int dataIndex = 0;
    int i = 0;

    //log.debug("number of triplets = " + numberTriplets);
    for( i = 0; i < numberTriplets; i++ )
    {
      dataIndex = i * 3;
      b1 = binaryData[dataIndex];
      b2 = binaryData[dataIndex + 1];
      b3 = binaryData[dataIndex + 2];

      //log.debug("b1= " + b1 +", b2= " + b2 + ", b3= " + b3);
      l = (byte)( b2 & 0x0f );
      k = (byte)( b1 & 0x03 );

      encodedIndex = i * 4;

      byte val1 = ( ( b1 & SIGN ) == 0 ) ? (byte)( b1 >> 2 ) : (byte)( b1 >> 2 ^ 0xc0 );
      byte val2 = ( ( b2 & SIGN ) == 0 ) ? (byte)( b2 >> 4 ) : (byte)( b2 >> 4 ^ 0xf0 );
      byte val3 = ( ( b3 & SIGN ) == 0 ) ? (byte)( b3 >> 6 ) : (byte)( b3 >> 6 ^ 0xfc );

      encodedData[encodedIndex] = lookUpBase64Alphabet[val1];

      //log.debug( "val2 = " + val2 );
      //log.debug( "k4 = " + (k<<4) );
      //log.debug( "vak = " + (val2 | (k<<4)) );
      encodedData[encodedIndex + 1] = lookUpBase64Alphabet[val2 | ( k << 4 )];
      encodedData[encodedIndex + 2] = lookUpBase64Alphabet[( l << 2 ) | val3];
      encodedData[encodedIndex + 3] = lookUpBase64Alphabet[b3 & 0x3f];
    }

    // form integral number of 6-bit groups
    dataIndex = i * 3;
    encodedIndex = i * 4;

    if( fewerThan24bits == EIGHTBIT )
    {
      b1 = binaryData[dataIndex];
      k = (byte)( b1 & 0x03 );

      //log.debug("b1=" + b1);
      //log.debug("b1<<2 = " + (b1>>2) );
      byte val1 = ( ( b1 & SIGN ) == 0 ) ? (byte)( b1 >> 2 ) : (byte)( b1 >> 2 ^ 0xc0 );
      encodedData[encodedIndex] = lookUpBase64Alphabet[val1];
      encodedData[encodedIndex + 1] = lookUpBase64Alphabet[k << 4];
      encodedData[encodedIndex + 2] = PAD;
      encodedData[encodedIndex + 3] = PAD;
    }
    else if( fewerThan24bits == SIXTEENBIT )
    {
      b1 = binaryData[dataIndex];
      b2 = binaryData[dataIndex + 1];
      l = (byte)( b2 & 0x0f );
      k = (byte)( b1 & 0x03 );

      byte val1 = ( ( b1 & SIGN ) == 0 ) ? (byte)( b1 >> 2 ) : (byte)( b1 >> 2 ^ 0xc0 );
      byte val2 = ( ( b2 & SIGN ) == 0 ) ? (byte)( b2 >> 4 ) : (byte)( b2 >> 4 ^ 0xf0 );

      encodedData[encodedIndex] = lookUpBase64Alphabet[val1];
      encodedData[encodedIndex + 1] = lookUpBase64Alphabet[val2 | ( k << 4 )];
      encodedData[encodedIndex + 2] = lookUpBase64Alphabet[l << 2];
      encodedData[encodedIndex + 3] = PAD;
    }

    return encodedData;
  }

  /**
   * Encodes a <tt>String</tt> into Base64.
   * 
   * @param s
   * @return Base64-encoded data.
   */
  public static byte[] encode( String s )
  {
    return encode( s.getBytes() );
  }

  /**
   * Decodes Base64 data into octects
   * 
   * @param base64Data
   *          Byte array containing Base64 data
   * @return Array containing decoded data.
   */
  public static byte[] decodeByteArray( byte[] base64Data )
  {
    // handle the edge case, so we don't have to worry about it later
    if( base64Data.length == 0 )
    {
      return new byte[0];
    }

    int numberQuadruple = base64Data.length / FOURBYTE;
    byte[] decodedData = null;
    byte b1 = 0;
    byte b2 = 0;
    byte b3 = 0;
    byte b4 = 0;
    byte marker0 = 0;
    byte marker1 = 0;

    // Throw away anything not in base64Data
    int encodedIndex = 0;
    int dataIndex = 0;

    // this sizes the output array properly - rlw
    int lastData = base64Data.length;

    // ignore the '=' padding
    while( base64Data[lastData - 1] == PAD )
    {
      if( ( --lastData ) == 0 )
      {
        return new byte[0];
      }
    }

    decodedData = new byte[lastData - numberQuadruple];

    for( int i = 0; i < numberQuadruple; i++ )
    {
      dataIndex = i * 4;
      marker0 = base64Data[dataIndex + 2];
      marker1 = base64Data[dataIndex + 3];

      b1 = base64Alphabet[base64Data[dataIndex]];
      b2 = base64Alphabet[base64Data[dataIndex + 1]];

      if( ( marker0 != PAD ) && ( marker1 != PAD ) )
      {
        //No PAD e.g 3cQl
        b3 = base64Alphabet[marker0];
        b4 = base64Alphabet[marker1];

        decodedData[encodedIndex] = (byte)( b1 << 2 | b2 >> 4 );
        decodedData[encodedIndex + 1] = (byte)( ( ( b2 & 0xf ) << 4 ) | ( ( b3 >> 2 ) & 0xf ) );
        decodedData[encodedIndex + 2] = (byte)( b3 << 6 | b4 );
      }
      else if( marker0 == PAD )
      {
        //Two PAD e.g. 3c[Pad][Pad]
        decodedData[encodedIndex] = (byte)( b1 << 2 | b2 >> 4 );
      }
      else if( marker1 == PAD )
      {
        //One PAD e.g. 3cQ[Pad]
        b3 = base64Alphabet[marker0];

        decodedData[encodedIndex] = (byte)( b1 << 2 | b2 >> 4 );
        decodedData[encodedIndex + 1] = (byte)( ( ( b2 & 0xf ) << 4 ) | ( ( b3 >> 2 ) & 0xf ) );
      }

      encodedIndex += 3;
    }

    return decodedData;
  }

  /**
   * Decodes Base64 data into a <tt>String</tt>
   * 
   * @param base64Data
   * @return Array containing decoded data.
   */
  public static String decodeString( byte[] base64Data )
  {
    return new String( decodeByteArray( base64Data ) );
  }

  /**
   * Decodes Base64 data into a <tt>InputStream</tt>
   * 
   * @param base64Data
   *          Byte array containing Base64 data
   * @return Array containing decoded data.
   */
  public static InputStream decodeInputStream( byte[] base64Data )
  {
    return new ByteArrayInputStream( decodeByteArray( base64Data ) );
  }

  /**
   * Decodes Base64 data into a <tt>OutputStream</tt>
   * 
   * @param base64Data
   *          Byte array containing Base64 data
   * @return Array containing decoded data.
   */
  public static OutputStream decodeOutputStream( byte[] base64Data )
  {
    byte[] b = decodeByteArray( base64Data );
    ByteArrayOutputStream bos = new ByteArrayOutputStream( b.length );
    bos.write( b, 0, b.length );
    return bos;
  }

  /**
   * Decodes Base64 data into a <tt>BufferedImage</tt>
   * 
   * @param base64Data
   *          Byte array containing Base64 data
   * @return Array containing decoded data.
   */
  public static BufferedImage decodeImage( byte[] base64Data ) throws IOException
  {
    byte[] b = decodeByteArray( base64Data );
    SeekableStream fss = new ByteArraySeekableStream( b );
    RenderedOp ro = JAI.create( "stream", fss );
    BufferedImage img = ro.getAsBufferedImage();
    fss.close();
    return img;
  }
}