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

package org.kalypsodeegree_impl.io.shpapi;

import org.kalypsodeegree.model.geometry.ByteUtils;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * Class representig a two dimensional ESRI Polygon <BR>
 * <B>Last changes <B>: <BR>
 * 12.01.2000 ap: constructor re-declared <BR>
 * 25.01.2000 ap: constructor modified; 25.01.2000 ap: public variables numRings and numPoints declared <BR>
 * 21.03.2000 ap: parameter list of the second constructor modified <BR>
 * 14.08.2000 ap: constructor SHPPolygon (GM_Point[][] gm_points) added <BR>
 * 14.08.2000 ap: method writeSHPPolygon(..) added <BR>
 * 14.08.2000 ap: import clauses added <BR>
 * 14.08.2000 ap: method size() added <BR>
 * 16.08.2000 ap: constructor SHPPolygon (byte[] recBuf) modified <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 16.08.2000
 * @author Andreas Poth
 */

public class SHPPolygon implements ISHPGeometry
{

  public int numRings = 0;

  public int numPoints = 0;

  public SHPPolyLine m_rings = null;

  private SHPEnvelope m_envelope;

  /**
   * constructor: recieves a stream <BR>
   */
  public SHPPolygon( byte[] recBuf )
  {
    m_envelope = ShapeUtils.readBox( recBuf, 4 );

    m_rings = new SHPPolyLine( recBuf );

    numPoints = m_rings.numPoints;
    numRings = m_rings.numParts;
  }

  /**
   * constructor: recieves an array of arrays of GM_Points <BR>
   */
  @SuppressWarnings("unchecked")
  public SHPPolygon( GM_Surface[] surface )
  {

    Debug.debugMethodBegin( this, "SHPPolygon" );

    try
    {
      int count = 0;

      for( int i = 0; i < surface.length; i++ )
      {
        // increment for exterior ring
        count++;
        // increment for inner rings
        GM_Ring[] rings = surface[i].getSurfaceBoundary().getInteriorRings();
        if( rings != null )
        {
          count += rings.length;
        }
      }

      GM_Curve[] curves = new GM_Curve[count];

      count = 0;
      for( int i = 0; i < surface.length; i++ )
      {

        GM_CurveSegment cs = surface[i].getSurfaceBoundary().getExteriorRing().getAsCurveSegment();
        curves[count++] = GeometryFactory.createGM_Curve( cs );

        GM_Ring[] rings = surface[i].getSurfaceBoundary().getInteriorRings();
        if( rings != null )
        {
          for( int j = 0; j < rings.length; j++ )
          {
            cs = rings[j].getAsCurveSegment();
            curves[count++] = GeometryFactory.createGM_Curve( cs );
          }
        }
      }

      m_rings = new SHPPolyLine( curves );

      m_envelope = m_rings.getEnvelope();

      numPoints = m_rings.numPoints;
      numRings = m_rings.numParts;

    }
    catch( Exception e )
    {
      System.out.println( "SHPPolygon::" + e );
    }

    Debug.debugMethodEnd();
  }

  /**
   * method: writeSHPPolygon(byte[] bytearray, int start) <BR>
   */
  public byte[] writeShape( )
  {
    int offset = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH;
    final byte[] byteArray = new byte[offset + size()];

    double xmin = m_rings.points[0][0].getX();
    double xmax = m_rings.points[0][0].getX();
    double ymin = m_rings.points[0][0].getY();
    double ymax = m_rings.points[0][0].getY();

    // write shape type identifier
    ByteUtils.writeLEInt( byteArray, offset, ShapeConst.SHAPE_TYPE_POLYGON );

    offset += 4;
    // save offset of the bounding box
    int tmp1 = offset;

    // increment offset with size of the bounding box
    offset += (4 * 8);

    // write numRings
    ByteUtils.writeLEInt( byteArray, offset, numRings );
    offset += 4;
    // write numpoints
    ByteUtils.writeLEInt( byteArray, offset, numPoints );
    offset += 4;

    // save offset of the list of offsets for each polyline
    int tmp2 = offset;

    // increment offset with numRings
    offset += (4 * numRings);

    int count = 0;
    for( int i = 0; i < m_rings.points.length; i++ )
    {

      // stores the index of the i'th part
      ByteUtils.writeLEInt( byteArray, tmp2, count );
      tmp2 += 4;

      // write the points of the i'th part and calculate bounding box
      for( int j = 0; j < m_rings.points[i].length; j++ )
      {
        // number of the current point
        count++;

        // calculate bounding box
        if( m_rings.points[i][j].getX() > xmax )
        {
          xmax = m_rings.points[i][j].getX();
        }
        else if( m_rings.points[i][j].getX() < xmin )
        {
          xmin = m_rings.points[i][j].getX();
        }

        if( m_rings.points[i][j].getY() > ymax )
        {
          ymax = m_rings.points[i][j].getY();
        }
        else if( m_rings.points[i][j].getY() < ymin )
        {
          ymin = m_rings.points[i][j].getY();
        }

        // write x-coordinate
        ByteUtils.writeLEDouble( byteArray, offset, m_rings.points[i][j].getX() );
        offset += 8;

        // write y-coordinate
        ByteUtils.writeLEDouble( byteArray, offset, m_rings.points[i][j].getY() );
        offset += 8;
      }
    }

    // jump back to the offset of the bounding box
    offset = tmp1;

    // write bounding box to the byte array
    ByteUtils.writeLEDouble( byteArray, offset, xmin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, ymin );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, xmax );
    offset += 8;
    ByteUtils.writeLEDouble( byteArray, offset, ymax );

    return byteArray;
  }

  /**
   * returns the polygon shape size in bytes <BR>
   */
  public int size( )
  {
    return 44 + numRings * 4 + numPoints * 16;
  }

  @Override
  public String toString( )
  {

    return "WKBPOLYGON" + " numRings: " + numRings;

  }

  public SHPEnvelope getEnvelope( )
  {
    return m_envelope;
  }

}