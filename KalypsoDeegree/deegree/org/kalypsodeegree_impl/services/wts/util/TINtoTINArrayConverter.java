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

import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Position;
import org.deegree.model.geometry.GM_Surface;
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
public class TINtoTINArrayConverter
{

  /**
   * Converts the geometries of the feature of a feature collection to TINs
   * stored in a javax.media.j3d.TriangleArray
   * 
   * @param fc
   *          feature collection
   * @param enableTexture
   *          true if the TINs shalled textured
   * @param scale
   *          scale factor ( 1 = original height )
   * @return
   */
  public static GeometryArray convertToArray( FeatureCollection fc, boolean enableTexture,
      float scale ) throws GM_Exception
  {

    Debug.debugMethodBegin();

    GM_Envelope envelope = fc.getEnvelope();

    if( fc == null || envelope == null )
    {
      return null;
    }

    // get envelope of the feature collection to calculate the texture
    // coordinates
    double width = envelope.getWidth();
    double height = envelope.getHeight();
    double minx = envelope.getMin().getX();
    double miny = envelope.getMin().getY();

    // allocate memory for TriangleArray considering texture option or not
    TriangleArray T = null;
    if( enableTexture )
    {
      T = new TriangleArray( fc.getSize() * 3, GeometryArray.COORDINATES | GeometryArray.COLOR_4
          | GeometryArray.TEXTURE_COORDINATE_2 | GeometryArray.NORMALS );
    }
    else
    {
      T = new TriangleArray( fc.getSize() * 3, GeometryArray.COORDINATES | GeometryArray.COLOR_4
          | GeometryArray.NORMALS );
    }

    float coord[] = new float[3];
    float texCoord[] = new float[2];
    int coordNr = 0;

    for( int i = 0; i < fc.getSize(); i++ )
    {
      GM_Surface surface = (GM_Surface)fc.getFeature( i ).getProperty( "FIN15.GEOM" );
      GM_Position[] pos = surface.getSurfacePatchAt( 0 ).getExteriorRing();
      pos = ccw( pos );
      for( int k = 0; k < 3; k++ )
      {
        coord[0] = -(float)pos[k].getX();
        coord[1] = (float)pos[k].getZ();
        coord[2] = (float)pos[k].getY();
        //System.out.println(coord[0]);
        //System.out.println(coord[1]);
        //System.out.println(coord[2]);
        //System.out.println();
        T.setCoordinate( coordNr, coord );
        if( enableTexture )
        {
          texCoord[0] = (float)( ( pos[k].getX() - minx ) / width );
          texCoord[1] = (float)( ( pos[k].getY() - miny ) / height );
          T.setTextureCoordinate( 0, coordNr++, texCoord );
        }
        else
        {
          coordNr++;
        }
      }
    }

    T.setValidVertexCount( fc.getSize() * 3 );

    GeometryInfo gi = new GeometryInfo( T );
    NormalGenerator ng = new NormalGenerator();
    //        ng.setCreaseAngle(3);
    ng.generateNormals( gi );
    GeometryArray result = gi.getGeometryArray();
    result.setValidVertexCount( fc.getSize() );

    Debug.debugMethodEnd();

    return result;
  }

  private static GM_Position[] ccw( GM_Position[] ring )
  {

    int p0;
    int p1;
    int p2;
    double dx1;
    double dx2;
    double dy1;
    double dy2;
    int[] pts = new int[4];
    boolean loop = false;

    p1 = pts[0];

    do
    {
      p0 = p1 - 1;

      if( p0 < 0 )
      {
        p0 = ring.length - 2;
      }

      p2 = p1 + 1;

      dx1 = ring[p1].getX() - ring[p0].getX();
      dy1 = ring[p1].getY() - ring[p0].getY();
      dx2 = ring[p2].getX() - ring[p0].getX();
      dy2 = ring[p2].getY() - ring[p0].getY();

      if( ( ( dx1 == 0 ) && ( dx2 == 0 ) ) || ( ( dy1 == 0 ) && ( dy2 == 0 ) ) )
      {
        p1++;

        if( p1 > ( ring.length - 2 ) )
        {
          p1 = 0;
        }
      }
      else
      {
        loop = true;
      }
    }
    while( loop == false );

    boolean tmp = false;
    if( ( dx1 * dy2 ) > ( dy1 * dx2 ) )
    {
      tmp = false;
    }

    if( ( dx1 * dy2 ) < ( dy1 * dx2 ) )
    {
      tmp = true;
    }

    if( ( dx1 * dx2 < 0 ) || ( dy1 * dy2 < 0 ) )
    {
      tmp = true;
    }

    if( ( dx1 * dx1 + dy1 * dy1 ) < ( dx2 * dx2 + dy2 * dy2 ) )
    {
      tmp = false;
    }

    if( tmp )
    {
      GM_Position[] pos = new GM_Position[3];
      int g = 2;
      for( int k = 0; k < 3; k++ )
      {
        pos[g--] = ring[k];
      }
      ring = pos;
    }

    return ring;
  }

}