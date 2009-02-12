/*
 * Created on 13.12.2004
 * 
 * TODO To change the template for this generated file go to Window - Preferences - Java - Code Style - Code Templates
 */
package org.kalypso.interpolation.mesh;

import javax.xml.namespace.QName;

import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * @author kuepfer
 */
public class Point
{

  // name space of gml-schema
  private final static String ns = "org.kalypso.risk";

  private final IFeatureType m_featureType;
  {
    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler geomTH = registry.getTypeHandlerForClassName( GeometryUtilities.getPointClass() );
    final IMarshallingTypeHandler doubleTH = registry.getTypeHandlerForClassName( Double.class );

    final IValuePropertyType pt1 = GMLSchemaFactory.createValuePropertyType( new QName( ns, "GEOM" ), geomTH.getTypeName(), geomTH, 1, 1, false );
    final IValuePropertyType pt2 = GMLSchemaFactory.createValuePropertyType( new QName( ns, "value" ), doubleTH.getTypeName(), doubleTH, 1, 1, false );
    final IPropertyType[] pts = new IPropertyType[] { pt1, pt2 };
    m_featureType = GMLSchemaFactory.createFeatureType( new QName( "ns", "FEMNode" ), pts );

  }

  private Feature feature = null;

  public Point( final String pointID, final Double val, final double x, final double y, final String crs )
  {
    final GM_Point point = GeometryFactory.createGM_Point( x, y, crs );
    final Feature f = FeatureFactory.createFeature( null, null, pointID, m_featureType, false );
    f.setProperty( "GEOM", point );
    f.setProperty( "value", val );
    this.feature = f;

  }// constructor}

  public Point( final String pointID, final double x, final double y, String crs )
  {
    if( crs == null )
    {
      crs = "EPSG:31467";
    }

    final GM_Point point = GeometryFactory.createGM_Point( x, y, crs );
    final Feature f = FeatureFactory.createFeature( null, null, pointID, m_featureType, false );
    f.setProperty( "GEOM", point );
    this.feature = f;

  }// constructor}

  public Point( final String pointID, final GM_Position pos, String crs )
  {
    if( crs == null )
    {
      // default coordinate system is Gauß-Krüger
      crs = "EPSG:31467";
    }

    final GM_Point point = GeometryFactory.createGM_Point( pos.getX(), pos.getY(), crs );
    final Feature f = FeatureFactory.createFeature( null, null, pointID, m_featureType, false );
    f.setProperty( "GEOM", point );
    this.feature = f;
  }// constructor}

  public Point( final String id, final GM_Object geom )
  {
    final Feature f = FeatureFactory.createFeature( null, null, id, m_featureType, false );
    f.setProperty( "GEOM", geom );
    this.feature = f;
  }

  public Feature getFeature( )
  {
    return feature;
  }

  public String getPointID( )
  {
    return feature.getId();
  }

  public Double getAttribute( )
  {
    final Double val = (Double) getFeature().getProperty( "value" );
    return val;
  }

  public void addAttribute( final Double v, final boolean replace )
  {
    final Double value = (Double) feature.getProperty( "value" );
    if( value != null && replace == true )
    {
      feature.setProperty( "value", v );
    }
    else if( value == null )
    {
      feature.setProperty( "value", v );
    }
  }

  public GM_Position getPosition( )
  {
    return ((GM_Point) feature.getProperty( "GEOM" )).getPosition();
  }// getPos()

  /**
   * <B>public double distance(Point p2, BufferedWriter logWriter) throws IOException </B>
   * <P>
   * Returns the distance with respect to given point
   * 
   * @param p2
   *            Point
   * @see Point
   * @param logWriter
   *            BufferedWriter
   * @return double
   * @throws IOException
   */
  // public double distance( Point p2, BufferedWriter logWriter )
  // throws IOException
  // {
  // double dx = this.distanceDx( p2 );
  // double dy = this.distanceDy( p2 );
  // double d = Math.pow( Math.pow( dx, 2 ) + Math.pow( dy, 2 ), 0.5 );
  //
  // //logWriter.write(" [ sqrroot{ " +
  // // "sqr(" + this.getX() + "-" + p2.getX() + ") + "
  // // + "sqr(" + this.getY() + "-" + p2.getY() + ") } ]" );
  //
  // return d;
  // }//distance
  /**
   * <B>public double distanceDx(Point p2) </B>
   * <P>
   * Returns the distance of x value with given point
   * 
   * @param p2
   *            Point
   * @return double
   */
  // public double distanceDx( Point p2 )
  // {
  // double dx = this.getPosition().getX() - p2.getPosition().getX();
  // return dx;
  // }//distanceDx
  //
  // public double distanceDx( GM_Position p2 )
  // {
  // double dx = this.getPosition().getX() - p2.getX();
  // return dx;
  // }//distanceDx
  /**
   * <B>public double distanceDy(Point p2) </B>
   * <P>
   * Returns the distance of y value with given point
   */
  // public double distanceDy( Point p2 )
  // {
  // double dy = this.getPosition().getY() - p2.getPosition().getY();
  // return dy;
  // }//distanceDy
  //
  // public double distanceDy( GM_Position p2 )
  // {
  // double dx = this.getPosition().getY() - p2.getY();
  // return dx;
  // }//distanceDx
  public void setAttribute( final Double v )
  {
    feature.setProperty( "value", v );
  }
}