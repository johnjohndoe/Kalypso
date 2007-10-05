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
 * Class representing a three dimensional ESRI Polygonz <BR>
 * <!---------------------------------------------------------------------------->
 * 
 * @version 26.01.2007
 * @author Thomas Jung
 */

public class SHPPolygonz implements ISHPGeometry
{

  public int numRings;

  public int numPoints;

  public SHPPointz[][] pointsz = null;

  public SHPPolyLinez m_rings = null;

  public SHPZRange m_zrange;

  private SHPEnvelope m_envelope = null;

  /**
   * constructor: recieves a stream <BR>
   */
  public SHPPolygonz( byte[] recBuf )
  {
    m_envelope = ShapeUtils.readBox( recBuf, 4 );

    m_rings = new SHPPolyLinez( recBuf );

    numPoints = m_rings.numPoints;
    numRings = m_rings.numParts;

  }

  /**
   * constructor: recieves an array of arrays of GM_Points <BR>
   */
  public SHPPolygonz( GM_Surface[] surface )
  {

    Debug.debugMethodBegin( this, "SHPPolygonz" );

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

      m_rings = new SHPPolyLinez( curves );

      m_envelope = m_rings.getEnvelope();
      m_zrange = m_rings.getZRange();
      numPoints = m_rings.numPoints;
      numRings = m_rings.numParts;

    }
    catch( Exception e )
    {
      System.out.println( "SHPPolygonz::" + e );
    }

    Debug.debugMethodEnd();
  }

  /**
   * method: writeSHPPolygonz(byte[] bytearray, int start) <BR>
   */
  public byte[] writeShape( )
  {
    int offset = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH;
    final byte[] bytearray = new byte[offset + size()];

    int byteposition;

    double xmin = m_rings.pointsz[0][0].getX();
    double xmax = m_rings.pointsz[0][0].getX();
    double ymin = m_rings.pointsz[0][0].getY();
    double ymax = m_rings.pointsz[0][0].getY();
    double zmin = m_rings.pointsz[0][0].getZ();
    double zmax = m_rings.pointsz[0][0].getZ();
    // write shape type identifier
    ByteUtils.writeLEInt( bytearray, offset, ShapeConst.SHAPE_TYPE_POLYGONZ );

    offset += 4;
    // save offset of the bounding box
    int tmp1 = offset;

    // increment offset with size of the bounding box
    offset += (4 * 8);

    // write numRings
    ByteUtils.writeLEInt( bytearray, offset, numRings );
    offset += 4;
    // write numpoints
    ByteUtils.writeLEInt( bytearray, offset, numPoints );
    offset += 4;

    // save offset of the list of offsets for each polyline
    int tmp2 = offset;

    // increment offset with numRings
    offset += (4 * numRings);

    int count = 0;
    for( int i = 0; i < m_rings.pointsz.length; i++ )
    {

      // stores the index of the i'th part
      ByteUtils.writeLEInt( bytearray, tmp2, count );
      tmp2 += 4;

      // write the points of the i'th part and calculate bounding box
      for( int j = 0; j < m_rings.pointsz[i].length; j++ )
      {
        // number of the current point
        count++;

        // calculate bounding box
        if( m_rings.pointsz[i][j].getX() > xmax )
        {
          xmax = m_rings.pointsz[i][j].getX();
        }
        else if( m_rings.pointsz[i][j].getX() < xmin )
        {
          xmin = m_rings.pointsz[i][j].getX();
        }

        if( m_rings.pointsz[i][j].getY() > ymax )
        {
          ymax = m_rings.pointsz[i][j].getY();
        }
        else if( m_rings.pointsz[i][j].getY() < ymin )
        {
          ymin = m_rings.pointsz[i][j].getY();
        }

        if( m_rings.pointsz[i][j].getZ() > zmax )
        {
          zmax = m_rings.pointsz[i][j].getZ();
        }
        else if( m_rings.pointsz[i][j].getZ() < zmin )
        {
          zmin = m_rings.pointsz[i][j].getZ();
        }

        // write x-coordinate
        ByteUtils.writeLEDouble( bytearray, offset, m_rings.pointsz[i][j].getX() );
        offset += 8;

        // write y-coordinate
        ByteUtils.writeLEDouble( bytearray, offset, m_rings.pointsz[i][j].getY() );
        offset += 8;

        // write z-coordinate
        // jump to the z-values
        byteposition = 44 + (4 * numRings) + (count * 16);
        ByteUtils.writeLEDouble( bytearray, byteposition, pointsz[i][j].getZ() );
      }
    }
    // jump back to the offset of the bounding box
    offset = tmp1;

    // write bounding box to the byte array
    ByteUtils.writeLEDouble( bytearray, offset, xmin );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, ymin );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, xmax );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, offset, ymax );

    // write z-range
    // jump to the z-range byte postition
    byteposition = 44 + (4 * numRings) + (numPoints * 16);
    // write z-range to the byte array
    ByteUtils.writeLEDouble( bytearray, byteposition, zmin );
    offset += 8;
    ByteUtils.writeLEDouble( bytearray, byteposition, zmax );

    return bytearray;
  }

  /**
   * returns the polygonz shape size in bytes <BR>
   */
  public int size( )
  {
    return 44 + numRings * 4 + numPoints * 16 + 16 + (8 * numPoints);
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