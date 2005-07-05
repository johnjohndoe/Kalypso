/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author kuepfer
 * 
 * TODO To change the template for this generated type comment go to Window -
 * Preferences - Java - Code Style - Code Templates
 */
public class Point
{

  //	min cardinality of the properties
  private final static int[] min =
  {
      1,
      1 };

  //max cardinality of the properties
  private final static int[] max =
  {
      1,
      1 };

  //name space of gml-schema
  private final static String ns = "org.kalypso.risk";

  private FeatureType featureType = FeatureFactory.createFeatureType(
      "FEMNode", ns, new FeatureTypeProperty[]
      {
          FeatureFactory.createFeatureTypeProperty( "GEOM", ns, GM_Point.class
              .getName(), false, null ),
          FeatureFactory.createFeatureTypeProperty( "value", ns,
              Double.class.getName(), false, null ) }, min, max, null, null );

  private Feature feature = null;

  public Point( String pointID, Double val, double x, double y,
      CS_CoordinateSystem crs )
  {
    GM_Point point = GeometryFactory.createGM_Point( x, y, crs );
    Feature f = FeatureFactory.createFeature( pointID, this.featureType );
    FeatureProperty geoProperty = FeatureFactory.createFeatureProperty( "GEOM",
        point );
    f.addProperty( geoProperty );
    FeatureProperty property = FeatureFactory.createFeatureProperty(
        "value", val );
    f.addProperty( property );
    this.feature = f;

  }//constructor}

  public Point( String pointID, double x, double y, CS_CoordinateSystem crs )
  {
    if( crs == null )
    {
      CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance()
          .getOGCCSByName( "EPSG:31467" );
      crs = cs;
    }
    GM_Point point = GeometryFactory.createGM_Point( x, y, crs );
    Feature f = FeatureFactory.createFeature( pointID, this.featureType );
    FeatureProperty geoProperty = FeatureFactory.createFeatureProperty( "GEOM",
        point );
    f.addProperty( geoProperty );
    this.feature = f;

  }//constructor}

  public Point( String pointID, GM_Position pos, CS_CoordinateSystem crs )
  {
    if( crs == null )
    {
      //default coordinate system is Gauß-Krüger
      CS_CoordinateSystem cs = ConvenienceCSFactory.getInstance()
          .getOGCCSByName( "EPSG:31467" );
      crs = cs;
    }
    GM_Point point = GeometryFactory.createGM_Point( pos.getX(), pos.getY(),
        crs );
    Feature f = FeatureFactory.createFeature( pointID, this.featureType );
    FeatureProperty geoProperty = FeatureFactory.createFeatureProperty( "GEOM",
        point );
    f.addProperty( geoProperty );
    this.feature = f;
  }//constructor}

  public Point( String id, GM_Object geom )
  {
    Feature f = FeatureFactory.createFeature( id, this.featureType );
    FeatureProperty geoProperty = FeatureFactory.createFeatureProperty( "GEOM",
        geom );
    f.addProperty( geoProperty );
    this.feature = f;
  }

  public Feature getFeature()
  {
    return feature;
  }

  public String getPointID()
  {
    return feature.getId();
  }

  public Double getAttribute()
  {
    Double val = (Double)getFeature().getProperty( "value" );
    return val;
  }

  public void addAttribute( Double v, boolean replace )
  {
    Double value = (Double)feature.getProperty( "value" );
    if( value != null && replace == true )
    {
      FeatureProperty property = FeatureFactory.createFeatureProperty(
          "value", v );
      feature.setProperty( property );
    }
    else if( value == null )
    {
      FeatureProperty property = FeatureFactory.createFeatureProperty(
          "value", v );
      feature.addProperty( property );
    }
  }

  public GM_Position getPosition()
  {
    return ( (GM_Point)feature.getProperty( "GEOM" ) ).getPosition();
  }//getPos()

  /**
   * <B>public double distance(Point p2, BufferedWriter logWriter) throws
   * IOException </B>
   * <P>
   * Returns the distance with respect to given point
   * 
   * @param p2 Point
   * @see Point
   * @param logWriter BufferedWriter
   * @return double
   * @throws IOException
   */
//  public double distance( Point p2, BufferedWriter logWriter )
//      throws IOException
//  {
//    double dx = this.distanceDx( p2 );
//    double dy = this.distanceDy( p2 );
//    double d = Math.pow( Math.pow( dx, 2 ) + Math.pow( dy, 2 ), 0.5 );
//
//    //logWriter.write(" [ sqrroot{ " +
//    //		"sqr(" + this.getX() + "-" + p2.getX() + ") + "
//    //		+ "sqr(" + this.getY() + "-" + p2.getY() + ") } ]" );
//
//    return d;
//  }//distance

  /**
   * <B>public double distanceDx(Point p2) </B>
   * <P>
   * Returns the distance of x value with given point
   * 
   * @param p2 Point
   * @return double
   */
//  public double distanceDx( Point p2 )
//  {
//    double dx = this.getPosition().getX() - p2.getPosition().getX();
//    return dx;
//  }//distanceDx
//
//  public double distanceDx( GM_Position p2 )
//  {
//    double dx = this.getPosition().getX() - p2.getX();
//    return dx;
//  }//distanceDx

  /**
   * <B>public double distanceDy(Point p2) </B>
   * <P>
   * Returns the distance of y value with given point
   */
//  public double distanceDy( Point p2 )
//  {
//    double dy = this.getPosition().getY() - p2.getPosition().getY();
//    return dy;
//  }//distanceDy
//
//  public double distanceDy( GM_Position p2 )
//  {
//    double dx = this.getPosition().getY() - p2.getY();
//    return dx;
//  }//distanceDx
  public void setAttribute( Double v )
  {
    FeatureProperty property = FeatureFactory
        .createFeatureProperty( "value", v );
    feature.setProperty( property );
  }
}