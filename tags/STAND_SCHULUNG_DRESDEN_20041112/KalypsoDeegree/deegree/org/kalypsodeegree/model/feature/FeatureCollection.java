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
package org.deegree.model.feature;

public interface FeatureCollection extends Feature
{
  /**
   * returns the feature at the submitted index
   */
  public Feature getFeature( int index );

  /**
   * returns the feature identified by it's id
   */
  public Feature getFeatureById( String id );

  /**
   * returns an array of all features
   */
  public Feature[] getAllFeatures();

  /**
   * adds a feature to the collection
   */
  public void appendFeature( Feature feature );

  /**
   * adds a list of features to the collection
   */
  public void appendFeatures( Feature[] feature );

  /**
   * adds a list of features to the collection
   */
  public void appendFeatures( FeatureCollection feature );

  /**
   * removes the submitted feature from the collection
   */
  public Feature removeFeature( Feature feature );

  /**
   * removes the feature at the submitted index from the collection
   */
  public Feature removeFeature( int index );

  /**
   * removes the feature that is assigned to the submitted id. The removed
   * feature will be returned. If no valid feature could be found null will be
   * returned
   */
  public Feature removeFeatureById( String id );

  /**
   * returns the number of features within the collection
   */
  public int getSize();

}