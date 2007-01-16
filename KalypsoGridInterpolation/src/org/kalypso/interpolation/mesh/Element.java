/*
 * Created on 11.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer TODO To change the template for this generated type comment go to Window - Preferences - Java - Code
 *         Style - Code Templates
 */
public class Element
{
  public static final int CONVEX_POLYGON = 1;

  public static final int CONCAVE_POLYGON = 0;

  // name space of gml-schema
  private final static String ns = "org.kalypso.risk";

  private final IFeatureType m_featureType;

  {
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final ITypeHandler geomTH = registry.getTypeHandlerForClassName( GeometryUtilities.getPolygonClass() );
    final ITypeHandler stringTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final IPropertyType[] pts = new IPropertyType[]//
    { GMLSchemaFactory.createValuePropertyType( new QName( ns, "GEOM" ), geomTH.getTypeName(), geomTH, 1, 1, false ),//
        GMLSchemaFactory.createValuePropertyType( new QName( ns, "vertList" ), stringTH.getTypeName(), stringTH, 1, 1, false ) //
    };
    m_featureType = GMLSchemaFactory.createFeatureType( new QName( ns, "MeshElement" ), pts );
  }

  private Feature feature = null;

  public Element( String eleNo, String verticies, GM_Position[] positions, CS_CoordinateSystem crs ) throws GM_Exception
  {
    if( crs == null )
    {
      // default coordinate system is Gauß-Krüger

      CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance().getOGCCSByName( "EPSG:31467" );
      crs = cs;
    }
    GM_Surface surface = GeometryFactory.createGM_Surface( positions, null, null, crs );
    final Feature f = FeatureFactory.createFeature( null, null, eleNo, m_featureType, false );

    FeatureHelper.addProperty( f, m_featureType.getProperty( "GEOM" ), surface );
    FeatureHelper.addProperty( f, m_featureType.getProperty( "vertList" ), verticies );
    this.feature = f;
  }

  public Element( String eleNo, GM_Object geom, CS_CoordinateSystem crs ) throws GM_Exception
  {
    if( crs == null )
      throw new GM_Exception( "No coordinate system defined! Element can not be created" );
    final Feature f = FeatureFactory.createFeature( null, null, eleNo, m_featureType, false );
    FeatureHelper.addProperty( f, m_featureType.getProperty( "GEOM" ), geom );
    this.feature = f;
  }

  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  public Feature getFeature( )
  {
    return feature;
  }

  public GM_Surface getGeometry( )
  {
    return (GM_Surface) feature.getDefaultGeometryProperty();
  }

  public GM_Envelope getBBox( )
  {
    return feature.getEnvelope();
  }

  public boolean isPointVertex( String pointID )
  {
    StringTokenizer st = new StringTokenizer( (String) feature.getProperty( "vertList" ) );
    while( st.hasMoreTokens() )
    {
      if( st.nextToken().equals( pointID ) )
        return true;
    }// while
    return false;
  }// isPointVertex

  public Vector getVertList( )
  {
    final Vector<String> vect = new Vector<String>();
    StringTokenizer st = new StringTokenizer( (String) feature.getProperty( "vertList" ) );
    while( st.hasMoreTokens() )
    {
      vect.add( st.nextToken() );
    }// while
    return vect;
  }// getVertList

  public String getElementID( )
  {
    return feature.getId();
  }

  public int getNumberOfVerticies( )
  {
    return getGeometry().getNumberOfSurfacePatches();
  }

  public Map splitElement( ) throws Exception
  {
    final Map<String, Element> res = new HashMap<String, Element>();
    String eID1 = getElementID() + ".1";
    String eID2 = getElementID() + ".2";
    Vector vList = getVertList();
    GM_Surface surface = getGeometry();
    GM_Position[] positions = surface.getSurfacePatchAt( 0 ).getExteriorRing();

    // make split such that diagonal will have least
    // slope

    if( ((positions[0].getY() - positions[1].getY()) / (positions[0].getX() - positions[1].getX())) < ((positions[1].getY() - positions[3].getY()) / (positions[1].getX() - positions[3].getX())) )
    {

      Element e1 = new Element( eID1, (String) vList.elementAt( 0 ) + " " + (String) vList.elementAt( 1 ) + " " + (String) vList.elementAt( 2 ) + " " + (String) vList.elementAt( 0 ), new GM_Position[] {
          positions[0], positions[1], positions[2], positions[0] }, null );
      Element e2 = new Element( eID2, (String) vList.elementAt( 1 ) + " " + (String) vList.elementAt( 2 ) + " " + (String) vList.elementAt( 3 ) + " " + (String) vList.elementAt( 1 ), new GM_Position[] {
          positions[1], positions[2], positions[3], positions[1] }, null );
      res.put( eID1, e1 );
      res.put( eID2, e2 );

    }
    else
    {
      Element e1 = new Element( eID1, (String) vList.elementAt( 0 ) + " " + (String) vList.elementAt( 1 ) + " " + (String) vList.elementAt( 3 ) + " " + (String) vList.elementAt( 0 ), new GM_Position[] {
          positions[0], positions[1], positions[3], positions[0] }, null );
      Element e2 = new Element( eID2, (String) vList.elementAt( 1 ) + " " + (String) vList.elementAt( 2 ) + " " + (String) vList.elementAt( 3 ) + " " + (String) vList.elementAt( 1 ), new GM_Position[] {
          positions[1], positions[2], positions[3], positions[1] }, null );
      res.put( eID1, e1 );
      res.put( eID2, e2 );
    }
    return res;
  }

  /**
   * <B>private static int getPolygonType(Point[] tVertices) </B>
   * <P>
   * Returns type of polygon from given vertices in the form of Point[]
   * 
   * @return int CONVEX_POLYGON or CONCAVE_POLYGON based on type of it
   */
  public int getPolygonType( ) throws Exception
  {
    GM_Surface surface = getGeometry();
    GM_Position[] positions = surface.getSurfacePatchAt( 0 ).getExteriorRing();
    int nNumOfVertices = positions.length - 1;// -1 because first and last
    // point are the same (closed)
    // polygon
    boolean bSignChanged = false;
    int nCount = 0;
    int j = 0, k = 0;

    for( int i = 0; i < nNumOfVertices; i++ )
    { // to get 3 consecutive
      // points like 0,1,2 1,2,3
      // 2,3,4 3,4,0 4,0,1
      j = (i + 1) % nNumOfVertices; // j:=i+1;
      k = (i + 2) % nNumOfVertices; // k:=i+2;

      // ((x1-x0)*(y2-y1))-((y1-y0)*(x2-x1))
      double crossProduct = (positions[j].getX() - positions[i].getX()) * (positions[k].getY() - positions[j].getY());
      crossProduct = crossProduct - ((positions[j].getY() - positions[i].getY()) * (positions[k].getX() - positions[j].getX()));

      // change the value of nCount
      if( (crossProduct > 0) && (nCount == 0) )// if positive first time
        nCount = 1;
      else if( (crossProduct < 0) && (nCount == 0) )// if negative first
        // time
        nCount = -1;

      if( ((nCount == 1) && (crossProduct < 0))// if negative this time
          // and was positive first
          // time
          || ((nCount == -1) && (crossProduct > 0)) )// or if positive
        // this time and
        // was negative
        // first time
        bSignChanged = true;// so if second time sign conflicts or
      // changed
    }

    if( bSignChanged )
      return CONCAVE_POLYGON;// if sign differs second time: its not
    // supported

    return CONVEX_POLYGON;// if same sign both times: its supported

  }

  /**
   * <B>public Object[] calculateWeightsFEM() </B>
   * <P>
   * Calculates weight using Finite Element Method FEM
   * 
   * @return Object[]
   * @throws Exception
   */
  public Object[] calculateWeightsFEM( ) throws Exception
  {
    double alpha[] = new double[3];
    double beta[] = new double[3];
    double gamma[] = new double[3];
    GM_Position[] v = getGeometry().getSurfacePatchAt( 0 ).getExteriorRing();
    alpha[0] = ((v[1].getX() * v[2].getY()) - (v[2].getX() * v[1].getY()));
    beta[0] = v[1].getY() - v[2].getY();
    gamma[0] = v[2].getX() - v[1].getX();

    alpha[1] = (v[2].getX() * v[0].getY() - (v[0].getX() * v[2].getY()));
    beta[1] = v[2].getY() - v[0].getY();
    gamma[1] = v[0].getX() - v[2].getX();

    alpha[2] = (v[0].getX() * v[1].getY() - (v[1].getX() * v[0].getY()));
    beta[2] = v[0].getY() - v[1].getY();
    gamma[2] = v[1].getX() - v[0].getX();

    Object res[] = { alpha, beta, gamma };
    return res;
  }// setWeightsFEM

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
  public double interpolatePointFEM( GM_Position p, double[] points ) throws Exception
  {
    // logWriter.newLine();
    // logWriter.write("Interpolation Method used: " + "Finite Element Method");
    // logWriter.close();
    double[] phi = new double[3];
    Double cellValue = new Double( 0 );
    Object[] factors = calculateWeightsFEM();
    GM_Position[] v = getGeometry().getSurfacePatchAt( 0 ).getExteriorRing();
    double area = getGeometry().getArea();
    // apply formula on each vertex
    for( int i = 0; i < v.length - 1; i++ )
    {
      // FEM-weights with phi
      phi[i] = (1 / (2 * (area))) * (((double[]) factors[0])[i] + ((double[]) factors[1])[i] * p.getX() + ((double[]) factors[2])[i] * p.getY());
    }
    // addup values from each vertex to calculate new interpolated value
    for( int i = 0; i < v.length - 1; i++ )
    {
      cellValue = new Double( cellValue.doubleValue() + phi[i] * points[i] );
    }
    // return new interpolated value
    return cellValue.doubleValue();
  }// interpolatePoint

  public boolean isPointOnEdge( Point p ) throws GM_Exception
  {
    GM_Position[] array = getGeometry().getSurfacePatchAt( 0 ).getExteriorRing();
    for( int i = 0; i < array.length - 1; i++ )
    {
      GM_Position vertex1 = array[i];
      GM_Position vertex2 = array[i + 1];
      double slope = 0.0;
      slope = ((vertex1.getX() - vertex2.getX()) / (vertex1.getY() - vertex2.getY()));

      if( slope == Double.NEGATIVE_INFINITY || slope == Double.POSITIVE_INFINITY )
        slope = 0;

      double b = vertex1.getY() - slope * vertex1.getX();
      double y = slope * p.getPosition().getX() + b;
      if( y == p.getPosition().getY() )
        return true;

    }// for i
    return false;
  }// isPointOnEdge

  public boolean isPointOnEdge( GM_Position p ) throws GM_Exception
  {
    GM_Position[] array = getGeometry().getSurfacePatchAt( 0 ).getExteriorRing();
    for( int i = 0; i < array.length - 1; i++ )
    {
      GM_Position vertex1 = array[i];
      GM_Position vertex2 = array[i + 1];
      double slope = 0.0;
      slope = ((vertex1.getX() - vertex2.getX()) / (vertex1.getY() - vertex2.getY()));

      if( slope == Double.NEGATIVE_INFINITY || slope == Double.POSITIVE_INFINITY )
        slope = 0;

      double b = vertex1.getY() - slope * vertex1.getX();
      double y = slope * p.getX() + b;
      if( y == p.getY() )
        return true;

    }// for i
    return false;
  }// isPointOnEdge

  @Override
  public String toString( )
  {
    return "ID: " + feature.getId() + " vertecies: " + feature.getDefaultGeometryProperty().toString();
  }
}// class Element
