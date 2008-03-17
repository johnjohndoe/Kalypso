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

import java.util.LinkedList;
import java.util.List;

import org.kalypsodeegree.model.geometry.ByteUtils;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_CurveSegment;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GM_PositionOrientation;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.GM_PositionOrientation.TYPE;
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

  private int m_numRings;

  private int m_numPoints;

  public final SHPPointz[][] pointsZ = null;

  private SHPPolyLinez m_rings = null;

  private SHPZRange m_zrange;

  private SHPEnvelope m_envelope = null;

  /**
   * constructor: recieves a stream <BR>
   */
  public SHPPolygonz( byte[] recBuf )
  {
    m_envelope = ShapeUtils.readBox( recBuf, 4 );

    m_rings = new SHPPolyLinez( recBuf );

    m_numPoints = m_rings.getNumPoints();
    m_numRings = m_rings.getNumParts();

  }

  /**
   * constructor: recieves an array of arrays of GM_Points <BR>
   */
  public SHPPolygonz( GM_SurfacePatch[] surfacePatch )
  {

    Debug.debugMethodBegin( this, "SHPPolygonz" );

    try
    {
      final List<GM_Curve> curveList = new LinkedList<GM_Curve>();
      String crs = surfacePatch[0].getCoordinateSystem();

      for( int i = 0; i < surfacePatch.length; i++ )
      {
        final GM_Position[] exteriorRing = surfacePatch[i].getExteriorRing();

        GM_CurveSegment cs = GeometryFactory.createGM_CurveSegment( exteriorRing, crs );

        final GM_Position[] positions = GM_PositionOrientation.orient( cs.getPositions(), TYPE.NEGATIV );
        cs = GeometryFactory.createGM_CurveSegment( positions, crs );
        if( cs != null )
          curveList.add( GeometryFactory.createGM_Curve( cs ) );

        final GM_Position[][] interiorRings = surfacePatch[i].getInteriorRings();

        if( interiorRings != null )
        {
          final GM_Curve[] rings = GeometryFactory.createGM_Curve( interiorRings, crs );
          if( rings != null )
          {
            for( int j = 0; j < rings.length; j++ )
            {
              curveList.add( rings[i] );
            }
          }
        }
      }

      m_rings = new SHPPolyLinez( curveList.toArray( new GM_Curve[curveList.size()] ) );

      m_numPoints = m_rings.getNumPoints();
      m_numRings = m_rings.getNumParts();

      m_envelope = m_rings.getEnvelope();
      m_zrange = m_rings.getZRange();
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
    if( m_rings == null )
      return null;

    int offset = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH;
    final byte[] bytearray = new byte[offset + size()];

    int byteposition;

    final SHPPointz[][] pointsZ = m_rings.getPointsz();

    final SHPPointz point = pointsZ[0][0];

    double xmin = point.getX();
    double xmax = point.getX();
    double ymin = point.getY();
    double ymax = point.getY();
    double zmin = point.getZ();
    double zmax = point.getZ();
    // write shape type identifier
    ByteUtils.writeLEInt( bytearray, offset, ShapeConst.SHAPE_TYPE_POLYGONZ );

    offset += 4;
    // save offset of the bounding box
    int tmp1 = offset;

    // increment offset with size of the bounding box
    offset += (4 * 8);

    // write numRings
    ByteUtils.writeLEInt( bytearray, offset, m_numRings );
    offset += 4;
    // write numpoints
    ByteUtils.writeLEInt( bytearray, offset, m_numPoints );
    offset += 4;

    // save offset of the list of offsets for each polyline
    int tmp2 = offset;

    // increment offset with numRings
    offset += (4 * m_numRings);

    int count = 0;
    for( int i = 0; i < pointsZ.length; i++ )
    {

      // stores the index of the i'th part
      ByteUtils.writeLEInt( bytearray, tmp2, count );
      tmp2 += 4;

      // write the points of the i'th part and calculate bounding box
      for( int j = 0; j < pointsZ[i].length; j++ )
      {
        // number of the current point
        count++;

        // calculate bounding box
        if( pointsZ[i][j].getX() > xmax )
        {
          xmax = pointsZ[i][j].getX();
        }
        else if( pointsZ[i][j].getX() < xmin )
        {
          xmin = pointsZ[i][j].getX();
        }

        if( pointsZ[i][j].getY() > ymax )
        {
          ymax = pointsZ[i][j].getY();
        }
        else if( pointsZ[i][j].getY() < ymin )
        {
          ymin = pointsZ[i][j].getY();
        }

        if( pointsZ[i][j].getZ() > zmax )
        {
          zmax = pointsZ[i][j].getZ();
        }
        else if( pointsZ[i][j].getZ() < zmin )
        {
          zmin = pointsZ[i][j].getZ();
        }

        // write x-coordinate
        ByteUtils.writeLEDouble( bytearray, offset, pointsZ[i][j].getX() );
        offset += 8;

        // write y-coordinate
        ByteUtils.writeLEDouble( bytearray, offset, pointsZ[i][j].getY() );
        offset += 8;

        // write z-coordinate
        // jump to the z-values
        byteposition = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH + 44 + (4 * m_numRings) + (m_numPoints * 16) + 16 + ((count - 1) * 8);
        ByteUtils.writeLEDouble( bytearray, byteposition, pointsZ[i][j].getZ() );
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
    byteposition = ShapeConst.SHAPE_FILE_RECORD_HEADER_LENGTH + 44 + (4 * m_numRings) + (m_numPoints * 16);
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
    return 44 + m_numRings * 4 + m_numPoints * 16 + 16 + (8 * m_numPoints);
  }

  @Override
  public String toString( )
  {

    return "WKBPOLYGON" + " numRings: " + m_numRings;

  }

  public SHPEnvelope getEnvelope( )
  {
    return m_envelope;
  }

  public int getNumRings( )
  {
    return m_numRings;
  }

  public int getNumPoints( )
  {
    return m_numPoints;
  }

  public SHPPointz[][] getPointsz( )
  {
    return pointsZ;
  }

  public SHPPolyLinez getRings( )
  {
    return m_rings;
  }

  public SHPZRange getZrange( )
  {
    return m_zrange;
  }

}