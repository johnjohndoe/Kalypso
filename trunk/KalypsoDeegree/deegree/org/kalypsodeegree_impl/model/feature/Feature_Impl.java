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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.opengis.gc.GC_GridCoverage;

/**
 * Features are, according to the Abstract Specification, digital
 * representations of real world entities. Feature Identity thus refers to
 * mechanisms to identify such representations: not to identify the real world
 * entities that are the subject of a representation. Thus two different
 * representations of a real world entity (say the Mississippi River) will be
 * two different features with distinct identities. Real world identification
 * systems, such as title numbers, while possibly forming a sound basis for an
 * implementation of a feature identity mechanism, are not of themselves such a
 * mechanism.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class Feature_Impl implements Feature, Serializable
{

  protected String id = "";

  protected FeatureType featureType = null;

  protected ArrayList geoProps = new ArrayList();

  protected HashMap properties = new HashMap();

  protected Object[] propRef = null;

  protected GM_Envelope envelope = null;

  /**
   * initializes a feature with its id its FeatureType and an array of
   * properties. It is assumed that the properties are in the same order then
   * the property definition within the FeatureType.
   */
  Feature_Impl( String id, FeatureType featureType, Object[] properties )
  {
    this.id = id;
    this.featureType = featureType;
    //	System.out.println("Feature_Impl() id "+id);
    if( featureType != null && properties != null )
    {
      FeatureTypeProperty[] ftp = featureType.getProperties();
      propRef = properties;
      for( int i = 0; i < ftp.length; i++ )
      {
        this.properties.put( ftp[i].getName(), new int[]
        { i } );
        if( properties[i] != null
            && ( properties[i] instanceof GM_Object || properties[i] instanceof GC_GridCoverage ) )
        {
          geoProps.add( properties[i] );
        }
      }
    }
  }

  /**
   * initializes a feature with its id its FeatureType and an array of
   * properties. It is assumed that the properties are in the same order then
   * the property definition within the FeatureType.
   */
  protected Feature_Impl( String id, FeatureType featureType, FeatureProperty[] properties )
  {
    this.id = id;
    this.featureType = featureType;

    if( featureType != null && properties != null )
    {
      propRef = new Object[properties.length];
      for( int i = 0; i < properties.length; i++ )
      {
        this.properties.put( properties[i].getName(), new int[]
        { i } );
        Object o = properties[i].getValue();
        if( o != null && ( o instanceof GM_Object || o instanceof GC_GridCoverage ) )
        {
          geoProps.add( o );
        }
        propRef[i] = o;
      }
    }
  }

  /**
   * returns the id of the Feature. the id has to be a name space that must be
   * unique for each feature. use the adress of the datasource in addition to a
   * number for example .
   */
  public String getId()
  {
    return id;
  }

  /**
   * returns the FeatureType of this Feature
   */
  public FeatureType getFeatureType()
  {
    return featureType;
  }

  /**
   * returns the properties of the feature as array of Objects
   */
  public Object[] getProperties()
  {
    return propRef;
  }

  /**
   * returns the property of the feature that matches the submitted name TODO
   * --> throw ModelException
   */
  public Object getProperty( String name )
  {
    int[] i = (int[])properties.get( name );
    if( i == null )
    {
      return null;
    }
    return propRef[i[0]];
  }

  /**
   * returns the property of the feature that matches the submitted index
   */
  public Object getProperty( int index )
  {
    return propRef[index];
  }

  /**
   * returns all geometry properties of the feature. If no geometry could be
   * found an <tt>GM_Object[]</tt> with zero length will be returned.
   */
  public GM_Object[] getGeometryProperties()
  {
    return (GM_Object[])geoProps.toArray( new GM_Object[geoProps.size()] );
  }

  /**
   * Returns the default geometry of the <tt>Feature</tt>. If there are no
   * geometry properties at all, or the default geometry is null (which is
   * possible when using ESRI-Shapefiles), null is returned.
   * <p>
   * 
   * @return default geometry or null, if the <tt>Feature</tt> has none
   */
  public GM_Object getDefaultGeometryProperty()
  {
    if( geoProps.size() < 1 )
      return null;
    return (GM_Object)geoProps.get( 0 );
  }

  /**
   * the value for the submitted property. if no property with the submitted
   * name exists the property will be added
   */
  public void setProperty( FeatureProperty property )
  {

    Object o = null;
    int[] index = (int[])properties.get( property.getName() );
    // index zeigt auf positionen von property in propref
    if( index == null ) // existiert noch nicht
    {
      Object[] tmp = new Object[propRef.length + 1];
      for( int i = 0; i < propRef.length; i++ )
      {
        tmp[i] = propRef[i];
      }
      tmp[tmp.length - 1] = property.getValue();
      properties.put( property.getName(), new int[]
      { tmp.length - 1 } );
      propRef = tmp;
    }
    else
    {
      o = propRef[index[0]];
      propRef[index[0]] = property.getValue();
    }

    if( property.getValue() instanceof GM_Object || property.getValue() instanceof GC_GridCoverage )
    {
      if( o != null )
      {
        geoProps.remove( o );
      }
      geoProps.add( property.getValue() );
      envelope = null;
    }
  }

  /**
   * returns the envelope / boundingbox of the feature
   */
  public GM_Envelope getEnvelope()
  {
    if( envelope == null )
    {
      if( geoProps.size() > 0 )
      {
        GM_Object geo = (GM_Object)geoProps.get( 0 );
        GM_Envelope env = null;
        if( !( geo instanceof GM_Point ) )
        {
          env = geo.getEnvelope();
        }
        else
        {
          env = GeometryFactory.createGM_Envelope( ( (GM_Point)geo ).getPosition(),
              ( (GM_Point)geo ).getPosition() );
        }
        for( int i = 1; i < geoProps.size(); i++ )
        {
          GM_Envelope env2 = null;
          if( !( geo instanceof GM_Point ) )
          {
            env2 = geo.getEnvelope();
          }
          else
          {
            env2 = GeometryFactory.createGM_Envelope( ( (GM_Point)geo ).getPosition(),
                ( (GM_Point)geo ).getPosition() );
          }
          env = env.merge( env2 );
        }
        envelope = env;
      }
    }
    return envelope;
  }

  public String toString()
  {
    String ret = getClass().getName() + "\n";
    ret = "id = " + id + "\n";
    ret += "featureType = " + featureType + "\n";
    ret += "geoProps = " + geoProps + "\n";
    ret += "properties = " + properties + "\n";
    return ret;
  }

}