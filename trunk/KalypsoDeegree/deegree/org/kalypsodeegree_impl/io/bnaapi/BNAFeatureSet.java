/*----------------    FILE HEADER  ------------------------------------------

 This file has been provided to deegree by
 Emanuele Tajariol e.tajariol@libero.it
 
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
package org.deegree_impl.io.bnaapi;

import java.util.HashMap;
import java.util.Vector;

/**
 * Class representing the features read from a BNA File.
 * <p>
 * 
 * @version 2003.08.04
 * @author Emanuele Tajariol
 *  
 */
public class BNAFeatureSet
{
  /** Contains the features keyed with ther unique name. */
  private HashMap _hm;

  /**
   * Provides an indexed access to features. Access order is arbitrary and
   * doesnt have to reflect data ordering inside the BNA file.
   */
  private Vector _list;

  /** Creates a new empty featureset. */
  public BNAFeatureSet()
  {
    _hm = new HashMap();
    _list = new Vector();
  }

  /**
   * Add a new feature.
   * <P>
   * If this set contains a feature with the same head of the added feature,
   * then their geometries will be merged.
   */
  public void addFeature( BNAFeature feature )
  {
    String hash = feature.getHeads();

    // Check if a BNAFeat with the same hash exists. If it does, then
    // the geometries of the two features can be merged, in that they belong to
    // the same feature.
    BNAFeature oldFeat = (BNAFeature)_hm.get( hash );

    if( oldFeat != null )
    {
      for( int i = 0, size = feature.size(); i < size; i++ )
      {
        oldFeat.addGeometry( feature.getGeometry( i ) );
      }

      return;
    }

    _hm.put( hash, feature );
    _list.add( feature );
  }

  /**
   * @return the number of Features present in this set.
   */
  public int size()
  {
    return _list.size();
  }

  /**
   * @return the index-th feature of this set.
   */
  public BNAFeature getFeature( int index )
  {
    return (BNAFeature)_list.get( index );
  }
}

//------------------------------------------------------------------------------
