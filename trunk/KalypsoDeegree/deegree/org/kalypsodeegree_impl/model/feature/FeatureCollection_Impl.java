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
import java.util.List;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureCollection;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree.model.geometry.GM_Envelope;

/**
 * This interface provides services for the management of groups of features.
 * These groups can come into being for a number of reasons: e.g. a project as a
 * whole, for the scope of a query, as the result of a query or arbitrarily
 * selected by a user for some common manipulation. A feature's membership of a
 * particular FeatureCollection does not necessarily imply any relationship with
 * other member features. Composite or compound features which own constituent
 * member Features (e.g. an Airport composed of Terminals, Runways, Aprons,
 * Hangars, etc) may also support the FeatureCollection interface to provide a
 * generic means for clients to access constituent members without needing to be
 * aware of the internal implementation details of the compound feature.
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class FeatureCollection_Impl extends Feature_Impl implements FeatureCollection, Serializable
{

  private List collection = null;

  private FeatureType ft = null;

  private GM_Envelope envelope = null;

  FeatureCollection_Impl( String id, FeatureType featureType, FeatureProperty[] properties,
      int initialCapacity )
  {
    //super( id, featureType, properties );
    super( featureType, id );
    for( int i = 0; i < properties.length; i++ )
      setProperty( properties[i] );
    collection = new ArrayList( initialCapacity );
    FeatureTypeProperty[] ftp = new FeatureTypeProperty[1];
    ftp[0] = FeatureFactory.createFeatureTypeProperty( "features",
        "org.deegree.model.feature.Feature[]", true );
    ft = FeatureFactory.createFeatureType( null, null, "feature collection", ftp );
  }

  /**
   * constructor for initializing a featur collection with an id and a initial
   * capacity.
   */
  FeatureCollection_Impl( String id, int initialCapacity )
  {
    this( id, null, null, initialCapacity );
  }

  /**
   * constructor for initializing a featur collection with an id and an array of
   * features.
   */
  FeatureCollection_Impl( String id, Feature[] feature )
  {
    this( id, feature.length );
    for( int i = 0; i < feature.length; i++ )
    {
      appendFeature( feature[i] );
    }
  }

  /**
   * returns the FeatureType of this Feature(Collection)
   */
  public FeatureType getFeatureType()
  {
    return ft;
  }

  /**
   * returns an array of all features
   */
  public Feature[] getAllFeatures()
  {
    return (Feature[])collection.toArray( new Feature[collection.size()] );
  }

  /**
   * returns the feature that is assigned to the submitted index. If the
   * submitted value for <tt>index</tt> is smaller 0 and larger then the
   * number features within the featurecollection-1 an exeption will be thrown.
   */
  public Feature getFeature( int index )
  {
    return (Feature)collection.get( index );
  }

  /**
   * returns the feature that is assigned to the submitted id. If no valid
   * feature could be found an <tt>Object[]</tt> with zero length will be
   * returned.
   */
  public Feature getFeatureById( String id )
  {
    Feature feature = null;
    for( int i = 0; i < collection.size(); i++ )
    {
      feature = (Feature)collection.get( i );
      if( feature.getId().equals( id ) )
      {
        break;
      }
    }
    return feature;
  }

  /**
   * removes a feature identified by its index (row) from the the feature
   * collection. The removed feature will be returned. If the submitted value
   * for <tt>index</tt> is smaller 0 and larger then the number features
   * within the featurecollection-1 an exeption will be thrown.
   */
  public Feature removeFeature( int index )
  {
    return (Feature)collection.remove( index );
  }

  /**
   * removes the submitted feature from the collection
   */
  public Feature removeFeature( Feature feature )
  {
    int index = collection.indexOf( feature );
    return removeFeature( index );
  }

  /**
   * removes the feature that is assigned to the submitted id. The removed
   * feature will be returned. If no valid feature could be found null will be
   * returned
   */
  public Feature removeFeatureById( String id )
  {
    Feature feature = getFeatureById( id );
    if( feature != null )
    {
      return removeFeature( feature );
    }
    return null;
  }

  /**
   * Appends a feature to the collection. If the submitted feature doesn't
   * matches the feature type defined for all features within the collection an
   * exception will be thrown.
   */
  public void appendFeature( Feature feature )
  {
    collection.add( feature );
  }

  /*
   * @see org.deegree.model.feature.FeatureCollection#appendFeatures(org.deegree.model.feature.Feature[])
   */
  public void appendFeatures( Feature[] features )
  {
    if( features != null )
    {
      for( int i = 0; i < features.length; i++ )
      {
        collection.add( features[i] );
      }
    }
  }

  /*
   * @see org.deegree.model.feature.FeatureCollection#appendFeatures(org.deegree.model.feature.FeatureCollection)
   */
  public void appendFeatures( FeatureCollection fc )
  {
    if( fc != null )
    {
      for( int i = 0; i < fc.getSize(); i++ )
      {
        collection.add( fc.getFeature( i ) );
      }
    }
  }

  /**
   * returns the number of features within the collection
   */
  public int getSize()
  {
    return collection.size();
  }

  /**
   * returns the envelope / boundingbox of the feature collection
   */
  public GM_Envelope getEnvelope()
  {
    if( envelope == null && collection.size() > 0 )
    {
      GM_Envelope env = ( (Feature)collection.get( 0 ) ).getEnvelope();
      for( int i = 1; i < collection.size(); i++ )
      {
        GM_Envelope env2 = ( (Feature)collection.get( i ) ).getEnvelope();
        if( env != null )
        {
          if( env2 != null )
          {
            env = env.merge( env2 );
          }
        }
        else
        {
          env = env2;
        }
      }
      envelope = env;
    }
    return envelope;
  }

  public String toString()
  {
    String ret = null;
    ret = "collection = " + collection + "\n";
    return ret;
  }

}