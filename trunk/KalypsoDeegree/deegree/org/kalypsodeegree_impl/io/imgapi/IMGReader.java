/*----------------    FILE HEADER  ------------------------------------------
 
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

package org.deegree_impl.io.imgapi;

import java.io.IOException;
import java.io.RandomAccessFile;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.ByteUtils;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.graphics.transformation.WorldToScreenTransform;
import org.deegree_impl.tools.Debug;

/**
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class IMGReader
{

  public static final int BYTE = 1;

  public static final int SMALLINT = 2;

  public static final int INT = 3;

  public static final int FLOAT = 4;

  public static final int DOUBLE = 8;

  private String fileName = null;

  private int width = 0;

  private int height = 0;

  private int type = 0;

  private GeoTransform trans = null;

  /** Creates a new instance of IMGReader */
  public IMGReader( String fileName, int width, int height, GM_Envelope bbox, int type )
  {

    this.fileName = fileName;
    this.width = width;
    this.height = height;
    this.type = type;
    trans = new WorldToScreenTransform( bbox.getMin().getX(), bbox.getMin().getY(), bbox.getMax()
        .getX(), bbox.getMax().getY(), 0, 0, width - 1, height - 1 );
  }

  /**
   * reads a rectangular subset from the image
   */
  public float[][] read( GM_Envelope bbox ) throws IOException
  {
    Debug.debugMethodBegin( this, "read(GM_Envelope)" );

    float[][] data = null;
    switch( type )
    {
    case BYTE:
      data = readByte( bbox );
      break;
    case SMALLINT:
      data = readSmallInt( bbox );
      break;
    case INT:
      data = readInt( bbox );
      break;
    case FLOAT:
      data = readFloat( bbox );
      break;
    case DOUBLE:
      data = readDouble( bbox );
      break;
    default:
      throw new IOException( "not supported file format!" );
    }

    Debug.debugMethodEnd();
    return data;
  }

  private float[][] readByte( GM_Envelope bbox ) throws IOException
  {
    Debug.debugMethodBegin( this, "read(GM_Envelope)" );

    int x1 = (int)trans.getDestX( bbox.getMin().getX() );
    if( x1 < 0 )
      x1 = 0;
    int y1 = (int)trans.getDestY( bbox.getMin().getY() );
    if( y1 >= height )
      y1 = height - 1;
    int x2 = (int)trans.getDestX( bbox.getMax().getX() );
    if( x2 >= width )
      x1 = width - 1;
    int y2 = (int)trans.getDestY( bbox.getMax().getY() );
    if( y2 < 0 )
      y2 = 0;

    int w = Math.abs( x2 - x1 );
    int h = Math.abs( y1 - y2 );

    RandomAccessFile raf = new RandomAccessFile( fileName, "r" );

    byte[] b = new byte[w];
    float[][] data = new float[h][w];
    for( int i = 0; i < h; i++ )
    {
      raf.seek( width * ( y2 + i ) + x1 );
      raf.read( b );
      for( int j = 0; j < w; j++ )
      {
        data[i][j] = b[j] + 127f;
      }
    }
    raf.close();

    Debug.debugMethodEnd();
    return data;
  }

  private float[][] readSmallInt( GM_Envelope bbox ) throws IOException
  {
    Debug.debugMethodBegin( this, "read(GM_Envelope)" );

    new NoSuchMethodError( "readSmallInt" );

    Debug.debugMethodEnd();
    return null;
  }

  private float[][] readInt( GM_Envelope bbox ) throws IOException
  {
    Debug.debugMethodBegin( this, "read(GM_Envelope)" );

    int x1 = (int)trans.getDestX( bbox.getMin().getX() );
    if( x1 < 0 )
      x1 = 0;
    int y1 = (int)trans.getDestY( bbox.getMin().getY() );
    if( y1 >= height )
      y1 = height - 1;
    int x2 = (int)trans.getDestX( bbox.getMax().getX() );
    if( x2 >= width )
      x1 = width - 1;
    int y2 = (int)trans.getDestY( bbox.getMax().getY() );
    if( y2 < 0 )
      y2 = 0;

    int w = Math.abs( x2 - x1 );
    int h = Math.abs( y1 - y2 );
    RandomAccessFile raf = new RandomAccessFile( fileName, "r" );

    byte[] b = new byte[w * 4];
    float[][] data = new float[h][w];
    for( int i = 0; i < h; i++ )
    {
      raf.seek( ( width * ( y2 + i ) + x1 ) * 4 );
      raf.read( b );
      int k = 0;
      for( int j = 0; j < w; j++ )
      {
        data[i][j] = ByteUtils.readBEInt( b, k );
        k += 4;
      }
    }
    raf.close();

    Debug.debugMethodEnd();
    return data;
  }

  private float[][] readFloat( GM_Envelope bbox ) throws IOException
  {
    Debug.debugMethodBegin( this, "read(GM_Envelope)" );

    int x1 = (int)trans.getDestX( bbox.getMin().getX() );
    int y1 = (int)trans.getDestY( bbox.getMin().getY() );
    int x2 = (int)trans.getDestX( bbox.getMax().getX() );
    int y2 = (int)trans.getDestY( bbox.getMax().getY() );

    int w = Math.abs( x2 - x1 );
    int h = Math.abs( y1 - y2 );

    if( w <= 0 || h <= 0 )
      return new float[0][0];

    RandomAccessFile raf = new RandomAccessFile( fileName, "r" );

    byte[] b = new byte[width * FLOAT];
    float[][] data = new float[h][w];
    for( int i = y2; i < y1; i++ )
    {
      if( i >= 0 && i < height )
      {
        raf.seek( width * i * FLOAT );
        raf.readFully( b );
        int k = 0;
        if( x1 > 0 )
          k = x1 * FLOAT;
        for( int j = x1; j < x2; j++ )
        {
          if( j >= 0 && j < width )
          {
            data[i - y2][j - x1] = ByteUtils.readBEFloat( b, k );
            k += FLOAT;
          }
        }
      }
    }
    raf.close();

    Debug.debugMethodEnd();
    return data;
  }

  private float[][] readDouble( GM_Envelope bbox ) throws IOException
  {
    Debug.debugMethodBegin( this, "read(GM_Envelope)" );

    int x1 = (int)trans.getDestX( bbox.getMin().getX() );
    if( x1 < 0 )
      x1 = 0;
    int y1 = (int)trans.getDestY( bbox.getMin().getY() );
    if( y1 >= height )
      y1 = height - 1;
    int x2 = (int)trans.getDestX( bbox.getMax().getX() );
    if( x2 >= width )
      x1 = width - 1;
    int y2 = (int)trans.getDestY( bbox.getMax().getY() );
    if( y2 < 0 )
      y2 = 0;

    int w = Math.abs( x2 - x1 );
    int h = Math.abs( y1 - y2 );

    RandomAccessFile raf = new RandomAccessFile( fileName, "r" );

    byte[] b = new byte[w * DOUBLE];
    float[][] data = new float[h][w];
    for( int i = 0; i < h; i++ )
    {
      raf.seek( ( width * ( y2 + i ) + x1 ) * DOUBLE );
      raf.read( b );
      int k = 0;
      for( int j = 0; j < w; j++ )
      {
        data[i][j] = (float)ByteUtils.readBEDouble( b, k );
        k += DOUBLE;
      }
    }
    raf.close();

    Debug.debugMethodEnd();
    return data;
  }

}