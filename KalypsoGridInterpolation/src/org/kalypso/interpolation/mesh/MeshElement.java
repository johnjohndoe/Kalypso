/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.interpolation.mesh;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.kalypso.interpolation.grid.Grid;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.cs.CS_CoordinateSystem;

import com.vividsolutions.jts.algorithm.RobustCGAlgorithms;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * MeshElement
 * <p>
 * 
 * created by
 * 
 * @author kuepfer (27.05.2005)
 */
public class MeshElement
{
  private final static String ns = "org.kalypso.risk";

  public static final int CONVEX_POLYGON = 1;

  public static final int CONCAVE_POLYGON = 0;

  private final static FeatureType m_featureType = FeatureFactory.createFeatureType( "MeshElement", ns,
      new FeatureTypeProperty[]
      { FeatureFactory.createFeatureTypeProperty( "GEOM", ns, GM_Surface.class.getName(), false, null ) }, new int[]
      {
          1,
          1 }, new int[]
      {
          1,
          1 }, null, null );

  private final Feature m_feature;

  private double[] m_values;

  public MeshElement( final String id, final GM_Position[] positions, final double[] values,
      final CS_CoordinateSystem crs ) throws GM_Exception
  {
    GM_Surface surface = GeometryFactory.createGM_Surface( positions, null, null, crs );
    Feature f = FeatureFactory.createFeature( id, m_featureType, false );
    FeatureProperty geoProperty = FeatureFactory.createFeatureProperty( "GEOM", surface );
    f.addProperty( geoProperty );
    m_feature = f;
    m_values = values;

  }//constructor}

  public void interpolateMeshElement( GM_Position[] cells, Grid grid ) throws Exception
  {
    for( int i = 0; i < cells.length; i++ )
    {
      GM_Position position = cells[i];
      if( getGeometry().contains( position ) == true )
      {
        double value = interpolatePointFEM( position );
        grid.writeGridValue( position, value );

      }//if cell contains
    }
  }

  public String getMeshElementID()
  {
    return m_feature.getId();

  }

  public GM_Envelope getEnvelope()
  {
    return m_feature.getEnvelope();
  }

  public String toString()
  {
    return m_feature.toString() + " values: " + m_values[0] + "\t" + m_values[1] + "\t" + m_values[2];
  }

  /**
   * <B>public double interpolatePointFEM(Point p, BufferedWriter logWriter) throws IOException </B>
   * <P>
   * Returns interpolated value of a given point using FEM method
   * 
   * @param p
   *          Point point to interpolate
   * @see Point
   * @return double interpolated value
   */
  private double interpolatePointFEM( GM_Position p ) throws Exception
  {
    //logWriter.newLine();
    //logWriter.write("Interpolation Method used: " + "Finite Element Method");
    //logWriter.close();
    double[] phi = new double[3];
    Double cellValue = new Double( 0 );
    Object[] factors = calculateWeightsFEM();
    GM_Position[] v = ( (GM_Surface)m_feature.getDefaultGeometryProperty() ).getSurfacePatchAt( 0 ).getExteriorRing();
    double area = ( (GM_Surface)m_feature.getDefaultGeometryProperty() ).getArea();
    //apply formula on each vertex
    for( int i = 0; i < v.length - 1; i++ )
    {
      //FEM-weights with phi
      phi[i] = ( 1 / ( 2 * ( area ) ) )
          * ( ( (double[])factors[0] )[i] + ( (double[])factors[1] )[i] * p.getX() + ( (double[])factors[2] )[i]
              * p.getY() );
    }
    //addup values from each vertex to calculate new interpolated value
    for( int i = 0; i < v.length - 1; i++ )
    {
      cellValue = new Double( cellValue.doubleValue() + phi[i] * m_values[i] );
    }
    //return new interpolated value
    return cellValue.doubleValue();
  }//interpolatePoint

  /**
   * <B>public Object[] calculateWeightsFEM() </B>
   * <P>
   * Calculates weight using Finite Element Method FEM
   * 
   * @return Object[]
   * @throws Exception
   */
  private Object[] calculateWeightsFEM() throws Exception
  {
    double alpha[] = new double[3];
    double beta[] = new double[3];
    double gamma[] = new double[3];
    GM_Position[] v = ( (GM_Surface)m_feature.getDefaultGeometryProperty() ).getSurfacePatchAt( 0 ).getExteriorRing();
    alpha[0] = ( ( v[1].getX() * v[2].getY() ) - ( v[2].getX() * v[1].getY() ) );
    beta[0] = v[1].getY() - v[2].getY();
    gamma[0] = v[2].getX() - v[1].getX();

    alpha[1] = ( v[2].getX() * v[0].getY() - ( v[0].getX() * v[2].getY() ) );
    beta[1] = v[2].getY() - v[0].getY();
    gamma[1] = v[0].getX() - v[2].getX();

    alpha[2] = ( v[0].getX() * v[1].getY() - ( v[1].getX() * v[0].getY() ) );
    beta[2] = v[0].getY() - v[1].getY();
    gamma[2] = v[1].getX() - v[0].getX();

    Object res[] =
    {
        alpha,
        beta,
        gamma };
    return res;
  }//setWeightsFEM

  public GM_Surface getGeometry()
  {
    return (GM_Surface)m_feature.getDefaultGeometryProperty();

  }

  public boolean isPointOnEdge( GM_Position p ) throws GM_Exception
  {
    GM_Position[] array = getGeometry().getSurfacePatchAt( 0 ).getExteriorRing();
    for( int i = 0; i < array.length - 1; i++ )
    {
      GM_Position vertex1 = array[i];
      GM_Position vertex2 = array[i + 1];
      double slope = 0.0;
      slope = ( ( vertex1.getX() - vertex2.getX() ) / ( vertex1.getY() - vertex2.getY() ) );

      if( slope == Double.NEGATIVE_INFINITY || slope == Double.POSITIVE_INFINITY )
        slope = 0;

      double b = vertex1.getY() - slope * vertex1.getX();
      double y = slope * p.getX() + b;
      if( y == p.getY() )
        return true;

    }//for i
    return false;
  }//isPointOnEdge

  public MeshElement[] splitElement() throws Exception
  {
    List res = new ArrayList();
    String eID1 = getMeshElementID() + ".1";
    String eID2 = getMeshElementID() + ".2";

    GM_Position[] positions = getGeometry().getSurfacePatchAt( 0 ).getExteriorRing();
    char orientation = getGeometry().getOrientation();

    //make split such that diagonal will have least
    // slope

    double m1 = Math
        .abs( ( positions[0].getY() - positions[2].getY() ) / ( positions[0].getX() - positions[2].getX() ) );
    double m2 = Math
        .abs( ( positions[1].getY() - positions[3].getY() ) / ( positions[1].getX() - positions[3].getX() ) );

    if( m1 < m2 && orientation == '+' )
    {

      MeshElement me1 = new MeshElement( eID1, new GM_Position[]
      {
          positions[0],
          positions[1],
          positions[2],
          positions[0] }, new double[]
      {
          m_values[0],
          m_values[1],
          m_values[2],
          m_values[0]

      }, getCoordinateSystem() );

      res.add( me1 );
      MeshElement me2 = new MeshElement( eID2, new GM_Position[]
      {
          positions[0],
          positions[2],
          positions[3],
          positions[0] }, new double[]
      {
          m_values[0],
          m_values[2],
          m_values[3],
          m_values[0] }, getCoordinateSystem() );
      res.add( me2 );
    }
    else
    {
      MeshElement me1 = new MeshElement( eID1, new GM_Position[]
      {
          positions[1],
          positions[2],
          positions[3],
          positions[1] }, new double[]
      {
          m_values[1],
          m_values[2],
          m_values[3],
          m_values[1]

      }, getCoordinateSystem() );

      res.add( me1 );
      MeshElement me2 = new MeshElement( eID2, new GM_Position[]
      {
          positions[1],
          positions[3],
          positions[0],
          positions[1] }, new double[]
      {
          m_values[1],
          m_values[3],
          m_values[0],
          m_values[1] }, getCoordinateSystem() );
      res.add( me2 );
    }

    //    if( ( ( positions[0].getY() - positions[1].getY() ) / (
    // positions[0].getX() - positions[1]
    //        .getX() ) ) < ( ( positions[1].getY() - positions[3].getY() ) / (
    // positions[1]
    //        .getX() - positions[3].getX() ) ) )
    //    {
    //
    //      MeshElement e1 = new MeshElement( eID1, new GM_Position[]
    //      {
    //          positions[0],
    //          positions[1],
    //          positions[2],
    //          positions[0] }, new double[]
    //      {
    //          m_values[0],
    //          m_values[1],
    //          m_values[2],
    //          m_values[0] }, getCoordinateSystem() );
    //      MeshElement e2 = new MeshElement( eID2, new GM_Position[]
    //      {
    //          positions[1],
    //          positions[2],
    //          positions[3],
    //          positions[1] }, new double[]
    //      {
    //          m_values[1],
    //          m_values[2],
    //          m_values[3],
    //          m_values[1] }, getCoordinateSystem() );
    //      res.add( e1 );
    //      res.add( e2 );
    //
    //    }
    //    else
    //    {
    //      MeshElement e1 = new MeshElement( eID1, new GM_Position[]
    //      {
    //          positions[0],
    //          positions[1],
    //          positions[3],
    //          positions[0] }, new double[]
    //      {
    //          m_values[0],
    //          m_values[1],
    //          m_values[3],
    //          m_values[0] }, getCoordinateSystem() );
    //      MeshElement e2 = new MeshElement( eID2, new GM_Position[]
    //      {
    //          positions[1],
    //          positions[2],
    //          positions[3],
    //          positions[1] }, new double[]
    //      {
    //          m_values[1],
    //          m_values[2],
    //          m_values[3],
    //          m_values[1] }, getCoordinateSystem() );
    //      res.add( e1 );
    //      res.add( e2 );
    //    }
    return (MeshElement[])res.toArray( new MeshElement[res.size()] );
  }

  /**
   * <B>private static int getPolygonType(Point[] tVertices) </B>
   * <P>
   * Returns type of polygon from given vertices in the form of Point[]
   * 
   * @return int CONVEX_POLYGON or CONCAVE_POLYGON based on type of it
   */
  public int getPolygonType() throws Exception
  {
    GM_Surface surface = getGeometry();
    GM_Position[] positions = surface.getSurfacePatchAt( 0 ).getExteriorRing();
    int nNumOfVertices = positions.length - 1;//-1 because first and last
    // point are the same (closed)
    // polygon
    boolean bSignChanged = false;
    int nCount = 0;
    int j = 0, k = 0;

    for( int i = 0; i < nNumOfVertices; i++ )
    { //to get 3 consecutive
      // points like 0,1,2 1,2,3
      // 2,3,4 3,4,0 4,0,1
      j = ( i + 1 ) % nNumOfVertices; //j:=i+1;
      k = ( i + 2 ) % nNumOfVertices; //k:=i+2;

      //((x1-x0)*(y2-y1))-((y1-y0)*(x2-x1))
      double crossProduct = ( positions[j].getX() - positions[i].getX() )
          * ( positions[k].getY() - positions[j].getY() );
      crossProduct = crossProduct
          - ( ( positions[j].getY() - positions[i].getY() ) * ( positions[k].getX() - positions[j].getX() ) );

      //change the value of nCount
      if( ( crossProduct > 0 ) && ( nCount == 0 ) )//if positive first time
        nCount = 1;
      else if( ( crossProduct < 0 ) && ( nCount == 0 ) )//if negative first
        // time
        nCount = -1;

      if( ( ( nCount == 1 ) && ( crossProduct < 0 ) )//if negative this time
          // and was positive first
          // time
          || ( ( nCount == -1 ) && ( crossProduct > 0 ) ) )//or if positive
        // this time and
        // was negative
        // first time
        bSignChanged = true;//so if second time sign conflicts or
      // changed
    }

    if( bSignChanged )
      return CONCAVE_POLYGON;//if sign differs second time: its not
    // supported

    return CONVEX_POLYGON;//if same sign both times: its supported

  }

  public CS_CoordinateSystem getCoordinateSystem()
  {
    return getGeometry().getCoordinateSystem();
  }

  public double[] getValues()
  {
    return m_values;
  }

  public String getId()
  {

    return m_feature.getId();
  }

  public int getOrientation() throws GM_Exception
  {
    Geometry geom1 = JTSAdapter.export( getGeometry() );
    Coordinate[] cor = geom1.getCoordinates();
    int k = RobustCGAlgorithms.orientationIndex( cor[0], cor[1], cor[2] );
    return k;
  }

  /**
   * @throws GM_Exception
   */
  public MeshElement invertOrientation() throws GM_Exception
  {
    GM_Position[] pos = getGeometry().getSurfaceBoundary().getExteriorRing().getPositions();
    final GM_Position[] newPos = new GM_Position[pos.length];
    final double[] newVal = new double[pos.length];
    System.arraycopy( m_values, 0, newVal, 0, pos.length );
    ArrayUtils.reverse( newVal );
    System.arraycopy( pos, 0, newPos, 0, pos.length );
    ArrayUtils.reverse( newPos );
    return new MeshElement( m_feature.getId(), newPos, newVal, getCoordinateSystem() );
  }
}//class
