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
package org.deegree_impl.graphics;

import java.util.ArrayList;

import org.deegree.graphics.FeatureLayer;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Object;
import org.deegree.model.geometry.GM_Point;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.ct.GeoTransformer;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * A Layer is a collection of <tt>Feature</tt> s building a thematic 'unit'
 * waterways or country borders for example. <tt>Feature</tt> s can be added
 * or removed from the layer. A <tt>Feature</tt> can e changed by a modul of
 * the application using the layer because only references to <tt>Feature</tt>
 * s are stored within a layer.
 * 
 * <p>
 * ------------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class FeatureLayer_Impl extends Layer_Impl implements FeatureLayer
{

  protected FeatureCollection fc = null;

  /**
   * creates a layer with EPSG:4326 as default coordinate system
   */
  FeatureLayer_Impl( String name ) throws Exception
  {
    super( name );

    fc = FeatureFactory.createFeatureCollection( name, 50 );

    init( fc );
  }

  /**
   * Creates a new FeatureLayer_Impl object.
   * 
   * @param name
   * @param crs
   * 
   * @throws Exception
   */
  FeatureLayer_Impl( String name, CS_CoordinateSystem crs ) throws Exception
  {
    super( name, crs );

    fc = FeatureFactory.createFeatureCollection( name, 50 );

    init( fc );
  }

  /**
   * Creates a new Layer_Impl object.
   * 
   * @param name
   * @param crs
   * @param fc
   * 
   * @throws Exception
   */
  FeatureLayer_Impl( String name, CS_CoordinateSystem crs, FeatureCollection fc ) throws Exception
  {
    super( name, crs );
    init( fc );
  }

  /**
   * initializes serveral parameters of the layer and homogenizes the coordinate
   * reference systems of the features
   */
  private void init( FeatureCollection feature ) throws Exception
  {
    Debug.debugMethodBegin();

    if( fc == null )
    {
      this.fc = FeatureFactory.createFeatureCollection( feature.getId(), feature.getSize() );
    }

    // create object for coordinate transformation
    GeoTransformer gt = new GeoTransformer( crs );

    double minx = 9E99;
    double maxx = -9E99;
    double miny = 9E99;
    double maxy = -9E99;

    for( int i = 0; i < feature.getSize(); i++ )
    {
      Feature feat = feature.getFeature( i );
      Object[] prop = feat.getProperties();
      Object[] propN = new Object[prop.length];
      boolean changed = false;
      for( int k = 0; k < prop.length; k++ )
      {
        if( prop[k] instanceof GM_Object )
        {

          String s1 = cs.getName();
          CS_CoordinateSystem _cs_ = ( (GM_Object)prop[k] ).getCoordinateSystem();
          String s2 = null;

          if( _cs_ != null )
          {
            s2 = _cs_.getName();
          }
          else
          {
            // default reference system
            s2 = "EPSG:4326";
          }

          if( !s1.equals( s2 ) )
          {
            // transforms the coordinate reference system of the
            // geometry to the crs of the layer.
            for( int j = 0; j < prop.length; j++ )
            {
              propN[j] = prop[j];
            }
            propN[k] = gt.transform( (GM_Object)prop[k] );
            changed = true;
          }

          if( prop[k] instanceof GM_Point )
          {
            GM_Position pos = ( (GM_Point)prop[k] ).getPosition();

            if( pos.getX() > maxx )
            {
              maxx = pos.getX();
            }
            else if( pos.getX() < minx )
            {
              minx = pos.getX();
            }

            if( pos.getY() > maxy )
            {
              maxy = pos.getY();
            }
            else if( pos.getY() < miny )
            {
              miny = pos.getY();
            }
          }
          else
          {
            GM_Envelope en = ( (GM_Object)prop[k] ).getEnvelope();

            if( en.getMax().getX() > maxx )
            {
              maxx = en.getMax().getX();
            }
            if( en.getMin().getX() < minx )
            {
              minx = en.getMin().getX();
            }

            if( en.getMax().getY() > maxy )
            {
              maxy = en.getMax().getY();
            }
            if( en.getMin().getY() < miny )
            {
              miny = en.getMin().getY();
            }
          }

        }
      }

      if( changed )
      {
        feat = FeatureFactory.createFeature( feat.getId(), feat.getFeatureType(), propN );
      }
      fc.appendFeature( feat );
    }

    boundingbox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    Debug.debugMethodEnd();

  }

  private void recalculateBoundingbox()
  {
    Debug.debugMethodBegin();

    double minx = 9E99;
    double maxx = -9E99;
    double miny = 9E99;
    double maxy = -9E99;

    for( int i = 0; i < fc.getSize(); i++ )
    {
      Object[] prop = fc.getFeature( i ).getProperties();
      for( int k = 0; k < prop.length; k++ )
      {
        if( prop[k] instanceof GM_Object )
        {
          if( prop[k] instanceof GM_Point )
          {
            GM_Position pos = ( (GM_Point)prop[k] ).getPosition();
            if( pos.getX() > maxx )
            {
              maxx = pos.getX();
            }
            else if( pos.getX() < minx )
            {
              minx = pos.getX();
            }
            if( pos.getY() > maxy )
            {
              maxy = pos.getY();
            }
            else if( pos.getY() < miny )
            {
              miny = pos.getY();
            }
          }
          else
          {
            GM_Envelope en = ( (GM_Object)prop[k] ).getEnvelope();
            if( en.getMax().getX() > maxx )
            {
              maxx = en.getMax().getX();
            }
            if( en.getMin().getX() < minx )
            {
              minx = en.getMin().getX();
            }
            if( en.getMax().getY() > maxy )
            {
              maxy = en.getMax().getY();
            }
            if( en.getMin().getY() < miny )
            {
              miny = en.getMin().getY();
            }
          }
        }
      }
    }

    boundingbox = GeometryFactory.createGM_Envelope( minx, miny, maxx, maxy );

    Debug.debugMethodEnd();
  }

  /**
   * returns the feature that matches the submitted id
   */
  public Feature getFeatureById( String id )
  {
    return fc.getFeatureById( id );
  }

  /**
   * returns the feature that matches the submitted id
   */
  public Feature[] getFeaturesById( String[] ids )
  {
    Debug.debugMethodBegin();

    ArrayList list = new ArrayList();

    Feature feature = null;

    for( int i = 0; i < fc.getSize(); i++ )
    {
      feature = fc.getFeature( i );

      for( int k = 0; k < ids.length; k++ )
      {
        if( feature.getId().equals( ids[k] ) )
        {
          list.add( feature );
          break;
        }
      }
    }

    Debug.debugMethodEnd();
    return (Feature[])list.toArray( new Feature[list.size()] );
  }

  /**
   * returns the feature that matches the submitted index
   */
  public Feature getFeature( int index )
  {
    Debug.debugMethodBegin( this, "getFeature" );

    Feature feature = fc.getFeature( index );

    Debug.debugMethodEnd();
    return feature;
  }

  /**
   * returns all features
   */
  public Feature[] getAllFeatures()
  {
    Debug.debugMethodBegin( this, "getAllFeatures" );

    Feature[] features = fc.getAllFeatures();

    Debug.debugMethodEnd();
    return features;
  }

  /**
   * adds a feature to the layer
   */
  public void addFeature( Feature feature ) throws Exception
  {
    Debug.debugMethodBegin( this, "addFeature" );

    fc.appendFeature( feature );
    recalculateBoundingbox();

    Debug.debugMethodEnd();
  }

  /**
   * adds a feature collection to the layer
   */
  public void addFeatureCollection( FeatureCollection featureCollection ) throws Exception
  {
    Debug.debugMethodBegin();

    fc.appendFeature( featureCollection );

    recalculateBoundingbox();

    Debug.debugMethodEnd();
  }

  /**
   * removes a display Element from the layer
   */
  public void removeFeature( Feature feature ) throws Exception
  {
    Debug.debugMethodBegin();

    fc.removeFeature( feature );

    recalculateBoundingbox();

    Debug.debugMethodEnd();
  }

  /**
   * removes the display Element from the layer that matches the submitted id
   */
  public void removeFeature( int id ) throws Exception
  {
    Debug.debugMethodBegin();

    removeFeature( getFeature( id ) );

    Debug.debugMethodEnd();
  }

  /**
   * returns the amount of features within the layer.
   */
  public int getSize()
  {
    return fc.getSize();
  }

  /**
   * sets the coordinate reference system of the MapView. If a new crs is set
   * all geometries of GeometryFeatures will be transformed to the new
   * coordinate reference system.
   */
  public void setCoordinatesSystem( CS_CoordinateSystem crs ) throws Exception
  {
    if( !cs.equals( crs ) )
    {
      this.crs = org.deegree_impl.model.cs.Adapters.getDefault().wrap( crs );
      init( fc );
    }
  }
}