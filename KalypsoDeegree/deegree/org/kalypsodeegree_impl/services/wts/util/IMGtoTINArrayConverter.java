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
package org.deegree_impl.services.wts.util;

import javax.media.j3d.GeometryArray;
import javax.media.j3d.TriangleArray;

import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;

import com.sun.j3d.utils.geometry.GeometryInfo;
import com.sun.j3d.utils.geometry.NormalGenerator;

/**
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class IMGtoTINArrayConverter
{

  /**
   * Convert heightmap values to triangle array, with textures, normals and
   * everything.. The TriangleArray object draws the array of vertices as
   * individual triangles. Each group of three vertices defines a triangle to be
   * drawn.
   * 
   * @param values
   *          heightmap values
   * @return created array
   */
  public static GeometryArray convertToArray( float[][] values, boolean enableTexture,
      GM_Surface ring, float scale )
  {

    Debug.debugMethodBegin();

    if( values == null )
      return null;
    float w = values[0].length - 1;
    float h = values.length - 1;

    TriangleArray T = null;
    if( enableTexture )
    {
      T = new TriangleArray( (int)( w * h * 6 ), GeometryArray.COORDINATES | GeometryArray.COLOR_4
          | GeometryArray.TEXTURE_COORDINATE_2 | GeometryArray.NORMALS );
    }
    else
    {
      T = new TriangleArray( (int)( w * h * 6 ), GeometryArray.COORDINATES | GeometryArray.COLOR_4
          | GeometryArray.NORMALS );
    }

    float coord[] = new float[3];
    float texCoord[] = new float[2];
    int coordNr = 0;

    float xmin = (float)ring.getEnvelope().getMin().getX();
    float ymin = (float)ring.getEnvelope().getMin().getY();
    float xmax = (float)ring.getEnvelope().getMax().getX();
    float ymax = (float)ring.getEnvelope().getMax().getY();
    float qx = ( xmax - xmin ) / w;
    float qy = ( ymax - ymin ) / h;

    int cc = 0;
    for( int j = 0; j < h; j++ )
    {
      for( int i = 0; i < w; i++ )
      {

        /*
         * 3 -- 2 6 | / / | | / / | 1 4 -- 5
         */
        // (1)
        coord[0] = -( i * qx + xmin );
        coord[2] = ( j + 1 ) * qy + ymin;
        GM_Position pos1 = GeometryFactory.createGM_Position( new double[]
        { -coord[0], coord[2] } );
        GM_Position pos2 = GeometryFactory.createGM_Position( new double[]
        { ( i * qx + xmin ), j * qy + ymin } );
        GM_Position pos3 = GeometryFactory.createGM_Position( new double[]
        { ( i + 1 ) * qx + xmin, j * qy + ymin } );
        boolean in1 = ring.contains( pos1 );
        boolean in2 = false;
        if( !in1 )
          in2 = ring.contains( pos2 );
        in2 = ring.contains( pos2 );
        boolean in3 = false;
        if( !in1 && !in2 )
          in3 = ring.contains( pos3 );
        in3 = ring.contains( pos3 );
        if( in1 || in2 || in3 )
        {
          cc += 3;
          coord[1] = values[(int)h - ( j + 1 )][i] * scale;
          T.setCoordinate( coordNr, coord );
          if( enableTexture )
          {
            texCoord[0] = i / w;
            texCoord[1] = ( j + 1 ) / h;
            T.setTextureCoordinate( 0, coordNr++, texCoord );
          }

          // (3)
          coord[0] = (float)-pos2.getX();
          coord[2] = (float)pos2.getY();
          coord[1] = values[(int)h - j][i] * scale;
          T.setCoordinate( coordNr, coord );
          if( enableTexture )
          {
            texCoord[0] = i / w;
            texCoord[1] = j / h;
            T.setTextureCoordinate( 0, coordNr++, texCoord );
          }

          // (2)
          coord[0] = (float)-pos3.getX();
          coord[2] = (float)pos3.getY();
          coord[1] = values[(int)h - j][( i + 1 )] * scale;
          T.setCoordinate( coordNr, coord );
          if( enableTexture )
          {
            texCoord[0] = ( i + 1 ) / w;
            texCoord[1] = j / h;
            T.setTextureCoordinate( 0, coordNr++, texCoord );
          }

        }

        // (5)
        coord[0] = -( ( i + 1 ) * qx + xmin );
        coord[2] = ( j + 1 ) * qy + ymin;
        pos1 = GeometryFactory.createGM_Position( new double[]
        { -coord[0], coord[2] } );
        pos2 = GeometryFactory.createGM_Position( new double[]
        { ( i * qx + xmin ), ( ( j + 1 ) * qy + ymin ) } );
        pos3 = GeometryFactory.createGM_Position( new double[]
        { ( ( i + 1 ) * qx + xmin ), ( j * qy + ymin ) } );
        in1 = ring.contains( pos2 );
        if( !in1 )
          in2 = ring.contains( pos1 );
        if( !in1 && !in2 )
          in3 = ring.contains( pos3 );
        if( in1 || in2 || in3 )
        {
          cc += 3;
          coord[1] = values[(int)h - ( j + 1 )][( i + 1 )] * scale;
          T.setCoordinate( coordNr, coord );
          if( enableTexture )
          {
            texCoord[0] = ( i + 1 ) / w;
            texCoord[1] = ( j + 1 ) / h;
            T.setTextureCoordinate( 0, coordNr++, texCoord );
          }

          // (4)
          coord[0] = -( i * qx + xmin );
          coord[2] = ( ( j + 1 ) * qy + ymin );
          coord[1] = values[(int)h - ( j + 1 )][i] * scale;
          T.setCoordinate( coordNr, coord );
          if( enableTexture )
          {
            texCoord[0] = i / w;
            texCoord[1] = ( j + 1 ) / h;
            T.setTextureCoordinate( 0, coordNr++, texCoord );
          }

          // (6)
          coord[0] = -( ( i + 1 ) * qx + xmin );
          coord[2] = ( j * qy + ymin );
          coord[1] = values[(int)h - j][( i + 1 )] * scale;
          T.setCoordinate( coordNr, coord );
          if( enableTexture )
          {
            texCoord[0] = ( i + 1 ) / w;
            texCoord[1] = j / h;
            T.setTextureCoordinate( 0, coordNr++, texCoord );
          }
        }
      }
    }

    T.setValidVertexCount( cc );

    GeometryInfo gi = new GeometryInfo( T );
    NormalGenerator ng = new NormalGenerator();
    //        ng.setCreaseAngle(3);
    ng.generateNormals( gi );
    GeometryArray result = gi.getGeometryArray();
    result.setValidVertexCount( cc );

    Debug.debugMethodEnd();

    return result;
  }

}