/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 
 history:
 
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
 
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypsodeegree_impl.filterencoding;

import java.util.ArrayList;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Encapsulates the information of a <Filter>element that consists of a number of FeatureId constraints (only) (as
 * defined in the FeatureId DTD).
 * 
 * @author Markus Schneider
 * @version 06.08.2002
 */
public class FeatureFilter extends AbstractFilter
{

  /** FeatureIds the FeatureFilter is based on */
  ArrayList featureIds = new ArrayList();

  /** Adds a FeatureId constraint. */
  public void addFeatureId( FeatureId featureId )
  {
    featureIds.add( featureId );
  }

  /** Returns the contained FeatureIds. */
  public ArrayList getFeatureIds()
  {
    return featureIds;
  }

  /**
   * Calculates the <tt>FeatureFilter</tt>'s logical value based on the ID of the given <tt>Feature</tt>. FIXME!!!
   * Use a TreeSet (or something) to speed up comparison.
   * 
   * @param feature
   *          that determines the Id
   * @return true, if the <tt>FeatureFilter</tt> evaluates to true, else false
   * @throws FilterEvaluationException
   *           if the evaluation fails
   */
  public boolean evaluate( Feature feature ) throws FilterEvaluationException
  {
    String id = feature.getId();
    for( int i = 0; i < featureIds.size(); i++ )
    {
      FeatureId featureId = (FeatureId)featureIds.get( i );
      if( id.equals( featureId.getValue() ) )
        return true;
    }
    return false;
  }

  /** Produces an indented XML representation of this object. */
  public StringBuffer toXML()
  {
    StringBuffer sb = new StringBuffer( 500 );
    sb.append( "<ogc:Filter xmlns:ogc='http://www.opengis.net/ogc'>" );
    for( int i = 0; i < featureIds.size(); i++ )
    {
      FeatureId fid = (FeatureId)featureIds.get( i );
      sb.append( fid.toXML() );
    }
    sb.append( "</ogc:Filter>" );
    return sb;
  }
}