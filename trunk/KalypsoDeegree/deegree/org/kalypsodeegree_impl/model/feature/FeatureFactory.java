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
package org.deegree_impl.model.feature;

import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.gml.GMLGeometry;
import org.deegree.gml.GMLProperty;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.extension.ITypeHandler;
import org.deegree_impl.extension.TypeRegistryException;
import org.deegree_impl.extension.TypeRegistrySingleton;
import org.deegree_impl.model.feature.xlink.XLinkArc;
import org.deegree_impl.model.feature.xlink.XLinkResource;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.tools.Debug;

/**
 * This factory offers methods for creating Features, FeatureCollection and all
 * direct related classes/interfaces that are part of the
 * org.deegree.model.feature package.
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
public class FeatureFactory
{

  /**
   * creates an instance of a FeatureTypeProperty from its name and the data
   * type it describes
   * 
   * @param name
   *          name of the feature type property
   * @param type
   *          type represented by the feature type property
   * @param nullable
   *          true if the feature type property is allowed to be <CODE>null
   *          </CODE>
   * @return instance of a <CODE>FeatureTypeProperty</CODE>
   */
  public static FeatureTypeProperty createFeatureTypeProperty( String name, String type,
      boolean nullable )
  {
    return new FeatureTypeProperty_Impl( name, type, nullable );
  }

  /**
   * creates an instance of a FeatureType from an array of
   * FeatureTypeProperties, its parents and childs and its name.
   * 
   * @param parents
   *          parents of the <CODE>FeatureType</CODE>
   * @param children
   *          known children of the <CODE>FeatureType</CODE>
   * @param name
   *          name of the <CODE>FeatureType</CODE>
   * @param properties
   *          properties containing the <CODE>FeatureType</CODE> s content
   * @return instance of a <CODE>FeatureType</CODE>
   */
  public static FeatureType createFeatureType( FeatureType[] parents, FeatureType[] children,
      String name, FeatureTypeProperty[] properties )
  {
    return new FeatureType_Impl( parents, children, name, properties );
  }

  /**
   * creates an instance of a FeatureProperty from its name and the data (value)
   * it contains
   * 
   * @param name
   *          name of the <CODE>FeatureProperty</CODE>
   * @return an instance of a <CODE>FeatureProperty</CODE>
   * @param value
   *          value of the <CODE>FeatureProperty</CODE>
   */
  public static FeatureProperty createFeatureProperty( String name, Object value )
  {
    return new FeatureProperty_Impl( name, value );
  }

  /**
   * creates an instance of a Feature from its FeatureType and an array of
   * Objects that represents it properties. It is assumed that the order of the
   * properties is identical to the order of the FeatureTypeProperties of the
   * the FeatureType.
   * 
   * @param id
   *          unique id of the <CODE>Feature</CODE>
   * @param featureType
   *          <CODE>FeatureType</CODE> of the <CODE>Feature</CODE>
   * @param properties
   *          properties (content) of the <CODE>Feature</CODE>
   * @return instance of a <CODE>Feature</CODE>
   */
  public static Feature createFeature( String id, FeatureType featureType, Object[] properties )
  {
    return new Feature_Impl( id, featureType, properties );
  }

  /**
   * creates an instance of a Feature from its FeatureType and an array of
   * Objects that represents it properties. It is assumed that the order of the
   * properties is identical to the order of the FeatureTypeProperties of the
   * the FeatureType.
   * 
   * @param id
   *          unique id of the <CODE>Feature</CODE>
   * @param featureType
   *          <CODE>FeatureType</CODE> of the <CODE>Feature</CODE>
   * @param properties
   *          properties (content) of the <CODE>Feature</CODE>
   * @return instance of a <CODE>Feature</CODE>
   */
  public static Feature createFeature( String id, FeatureType featureType,
      FeatureProperty[] properties )
  {
    Debug.debugMethodBegin();//this, "createFeature(String , FeatureType ,
    // FeatureProperty[])" );

    Object[] o = new Object[properties.length];
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
    {
      String name = ftp[i].getName();
      for( int j = 0; j < properties.length; j++ )
      {
        if( properties[j].getName().equals( name ) )
        {
          o[i] = properties[j].getValue();
          break;
        }
      }
    }

    Debug.debugMethodEnd();
    //	return new Feature_Impl( id, featureType, properties );
    return createFeature( id, featureType, o );
  }

  /**
   * creates an instance of a Feature from its FeatureType and a GMLFeature that
   * contains the features data.
   * 
   * @param gmlFeature
   *          instance of a <CODE>GMLFeature</CODE>
   * @return instance of a <CODE>Feature</CODE>
   */
  public static Feature createFeature( GMLFeature gmlFeature )
  {
    Debug.debugMethodBegin();

    GMLProperty[] props = gmlFeature.getProperties();
    FeatureTypeProperty[] ftp = new FeatureTypeProperty[props.length];
    FeatureProperty[] fp = new FeatureProperty[props.length];
    for( int j = 0; j < props.length; j++ )
    {

      ftp[j] = createFeatureTypeProperty( props[j].getName(),
          getType( props[j].getPropertyType() ), true );
      Object o = props[j].getPropertyValue();
      if( o instanceof GMLGeometry )
      {
        try
        {
          o = GMLAdapter.wrap( (GMLGeometry)o );
        }
        catch( Exception e )
        {
          System.out.println( " eee " + e );
          continue;
        }
      }

      fp[j] = createFeatureProperty( props[j].getName(), o );
    }
    FeatureType featureType = createFeatureType( null, null, gmlFeature.getName(), ftp );

    String id = gmlFeature.getId();

    Feature feature = createFeature( id, featureType, fp );

    Debug.debugMethodEnd();
    return feature;
  }

  // TODO: suggest to deegree-mailinglist
  public static Feature createFeature( GMLFeature gmlFeature, FeatureType featureTypes[] )
      throws Exception
  {
    Debug.debugMethodBegin();
    FeatureType featureType = null;

    /*
     * Feature testFeature=createFeature(gmlFeature); FeatureType
     * testFT=testFeature.getFeatureType(); FeatureTypeProperty
     * testFTP[]=testFT.getProperties(); for(int i=0;i <testFTP.length;i++) {
     * System.out.println(" "+i+".Property: "+testFTP[i].getName()+"
     * ("+testFTP[i].getType()+")"); }
     */
    String featureName = gmlFeature.getName();
    //	System.out.println("FeatureName:"+featureName);
    //	System.out.println("featureTypes.length:"+featureTypes.length);
    int ft_i = 0;
    while( ft_i < featureTypes.length && !featureName.equals( featureTypes[ft_i].getName() ) )
      ft_i++;

    if( ft_i < featureTypes.length )
    {
      featureType = featureTypes[ft_i];
      //		System.out.println("select featureType: "+featureType.getName());
    }
    else
      throw new Exception( "could not find named feature " + featureName + " in schema" );

    GMLProperty[] gmlProps = gmlFeature.getProperties();

    FeatureProperty[] fp = new FeatureProperty[gmlProps.length];
    // every gmlProp must fit to a featurePropertyType
    for( int p = 0; p < gmlProps.length; p++ )
    {
      GMLProperty gmlProp = gmlProps[p];

      FeatureTypeProperty ftp = featureType.getProperty( gmlProp.getName() );
      if( ftp == null )
        throw new Exception( "property '" + gmlProp.getName() + "' not defined in schema" );

      Object o = wrap( ftp, gmlProp );

      fp[p] = createFeatureProperty( gmlProp.getName(), o );
    }
    String id = gmlFeature.getId();
    Feature feature = createFeature( id, featureType, fp );
    Debug.debugMethodEnd();
    return feature;
  }

  private static Object wrap( FeatureTypeProperty ftp, GMLProperty gmlProperty ) throws Exception
  {
    if( ftp instanceof XLinkFeatureTypeProperty )
      return wrapXLink( (XLinkFeatureTypeProperty)ftp, gmlProperty );
    return wrapNOXLink( ftp, gmlProperty );
  }

  private static Object wrapXLink( XLinkFeatureTypeProperty propType, GMLProperty gmlProperty )
  {
    Object value = null;
    // TODO support xlink:actuate=onLoad
    switch( propType.getXLinkType() )
    {
    case XLinkFeatureTypeProperty.XLINK_SIMPLE:
    case XLinkFeatureTypeProperty.XLINK_LOCATOR:
      //
      value = gmlProperty.getAttributeValue( "http://www.w3.org/1999/xlink", "href" );
      break;
    case XLinkFeatureTypeProperty.XLINK_EXTENDED:
      //TODO
      break;
    case XLinkFeatureTypeProperty.XLINK_RESOURCE:
      value = new XLinkResource();
      //TODO
      break;
    case XLinkFeatureTypeProperty.XLINK_ARC:
      value = new XLinkArc( propType.getLabelFrom(), propType.getLabelTo() );
      break;
    default:
      break;
    }

    return value;

  }

  private static Object wrapNOXLink( FeatureTypeProperty ftp, GMLProperty gmlProperty )
      throws Exception
  {
    final String type = ftp.getType();
    final ITypeHandler typeHandler = TypeRegistrySingleton.getTypeRegistry()
        .getTypeHandlerForClassName( type );

    if( typeHandler != null )
      return typeHandler.unmarshall( gmlProperty.getElement() );

    final Object o = gmlProperty.getPropertyValue();
    if( o == null )
    {
      if( ftp.isNullable() )
        return null;
      else
        throw new Exception( "Property " + ftp.getName() + " is not nullable, but value is" );
    }

    try
    {
      //		System.out.println("Object"+o.getClass().toString());
      if( o instanceof String )
      {
        String string = (String)o;
        if( "java.lang.String".equals( type ) )
          return string;
        if( "java.lang.Float".equals( type ) )
          return new Float( string );
        if( "java.lang.Double".equals( type ) )
          return new Double( string );
        if( "java.lang.Integer".equals( type ) )
        {
          double value = Double.parseDouble( string );
          Integer integer = new Integer( (int)value );
          if( integer.intValue() != value )
            throw new Exception();
          return integer;
        }
        if( "java.lang.Boolean".equals( type ) )
          return new Boolean( string );
      }
      if( o instanceof GMLGeometry && type.startsWith( "org.deegree.model.geometry." ) )
        return GMLAdapter.wrap( (GMLGeometry)o );
      throw new Exception();
    }
    catch( Exception e )
    {
      throw new Exception( "could not convert property (" + o.toString() + ") to " + type );
    }
  }

  /**
   * returns the name of the (toplevel)class that is assigned to the submitted
   * GML property type.
   * 
   * @param t
   *          GML property type
   */
  private static String getType( int t )
  {

    String type = "java.lang.Object";
    switch( t )
    {
    case GMLProperty.STRING:
      type = "java.lang.String";
      break;
    case GMLProperty.GEOMETRY:
      type = "org.deegree.model.geometry.GM_Object";
      break;
    case GMLProperty.POINT:
      type = "org.deegree.model.geometry.GM_Point";
      break;
    case GMLProperty.LINESTRING:
      type = "org.deegree.model.geometry.GM_LineString";
      break;
    case GMLProperty.POLYGON:
      type = "org.deegree.model.geometry.GM_Polygon";
      break;
    case GMLProperty.MULTIGEOMETRY:
      type = "org.deegree.model.geometry.GM_Object";
      break;
    case GMLProperty.MULTILINESTRING:
      type = "org.deegree.model.geometry.GM_Object";
      break;
    case GMLProperty.MULTIPOINT:
      type = "org.deegree.model.geometry.GM_MultiPoint";
      break;
    case GMLProperty.MULTIPOLYGON:
      type = "org.deegree.model.geometry.GM_Object";
      break;
    case GMLProperty.FEATURE:
      type = "org.deegree.model.feature.Feature";
      break;
    case GMLProperty.FEATURECOLLECTION:
      type = "org.deegree.model.feature.FeatureCollection";
      break;
    case GMLProperty.BOX:
      type = "org.deegree.model.geometry.GM_Envelope";
      break;
    }
    return type;
  }

  /**
   * creates an instance of a FeatureCollection with an initial capacity and a
   * defined featuretype.
   * 
   * @param id
   *          unique id of the <CODE>FeatureCollection</CODE>
   * @param featureType
   *          <CODE>FeatureType</CODE> of the <CODE>Feature</CODE>
   * @param properties
   *          properties (content) of the <CODE>Feature</CODE>
   * @param initialCapacity
   *          initial capacity of the <CODE>FeatureCollection</CODE>
   * @return instance of an empty <CODE>FeatureCollection</CODE>
   */
  public static FeatureCollection createFeatureCollection( String id, FeatureType featureType,
      FeatureProperty[] properties, int initialCapacity )
  {
    return new FeatureCollection_Impl( id, featureType, properties, initialCapacity );
  }

  /**
   * creates an instance of a FeatureCollection with an initial capacity. The
   * returned FeatureCollection doesn't have a FeatureType nor properties. It is
   * just a collection of Features.
   * 
   * @param id
   *          unique id of the <CODE>FeatureCollection</CODE>
   * @param initialCapacity
   *          initial capacity of the <CODE>FeatureCollection</CODE>
   * @return instance of an empty <CODE>FeatureCollection</CODE>
   */
  public static FeatureCollection createFeatureCollection( String id, int initialCapacity )
  {
    return new FeatureCollection_Impl( id, initialCapacity );
  }

  /**
   * creates an instance of a FeatureCollection from an array of Features. The
   * returned FeatureCollection doesn't have a FeatureType nor properties. It is
   * just a collection of Features.
   * 
   * @param id
   *          unique id of the <CODE>FeatureCollection</CODE> instance
   * @param features
   *          <CODE>Feature</CODE> s to fill in into the <CODE>
   *          FeatureCollection</CODE>
   * @return instance of a <CODE>FeatureCollection</CODE> containing the
   *         submitted features
   */
  public static FeatureCollection createFeatureCollection( String id, Feature[] features )
  {
    return new FeatureCollection_Impl( id, features );
  }

  /**
   * creates an instance of a FeatureCollection from a GMLFeatureCollection
   * 
   * @param gmlFc
   *          <CODE>GMLFeatureCollection</CODE> to create the <CODE>
   *          FeatureCollection</CODE> instance from
   * @throws Exception -
   * @return instance of a <CODE>FeatureCollection</CODE>
   */
  public static FeatureCollection createFeatureCollection( GMLFeatureCollection gmlFc )
      throws Exception
  {
    Debug.debugMethodBegin();

    String id = gmlFc.getId();

    GMLFeature[] gmlFeat = gmlFc.getFeatures();
    FeatureCollection fc = null;
    if( gmlFeat != null )
    {
      fc = createFeatureCollection( id, gmlFeat.length );

      for( int i = 0; i < gmlFeat.length; i++ )
      {
        Feature feature = createFeature( gmlFeat[i] );
        fc.appendFeature( feature );
      }
    }
    else
    {
      fc = createFeatureCollection( id, 100 );
    }

    Debug.debugMethodEnd();
    return fc;
  }
}