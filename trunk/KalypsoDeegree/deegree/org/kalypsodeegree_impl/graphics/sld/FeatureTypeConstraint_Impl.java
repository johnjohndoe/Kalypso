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
package org.deegree_impl.graphics.sld;

import java.util.ArrayList;

import org.deegree.graphics.sld.Extent;
import org.deegree.graphics.sld.FeatureTypeConstraint;
import org.deegree.services.wfs.filterencoding.Filter;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.Debug;

/**
 * A FeatureTypeConstraint element is used to identify a feature type by
 * well-known name, using the FeatureTypeName element.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
class FeatureTypeConstraint_Impl implements FeatureTypeConstraint, Marshallable
{
  private ArrayList extents = null;

  private Filter filter = null;

  private String featureTypeName = null;

  /**
   * constructor initializing the class with the <FeatureTypeConstraint>
   */
  FeatureTypeConstraint_Impl( String featureTypeName, Filter filter, Extent[] extents )
  {
    this.extents = new ArrayList();
    setFeatureTypeName( featureTypeName );
    setFilter( filter );
    setExtents( extents );
  }

  /**
   * returns the name of the feature type
   * 
   * @return the name of the feature type
   */
  public String getFeatureTypeName()
  {
    return featureTypeName;
  }

  /**
   * sets the name of the feature type
   * 
   * @param featureTypeName
   *          the name of the feature type
   */
  public void setFeatureTypeName( String featureTypeName )
  {
    this.featureTypeName = featureTypeName;
  }

  /**
   * returns a feature-filter as defined in WFS specifications.
   * 
   * @return the filter of the FeatureTypeConstraints
   */
  public Filter getFilter()
  {
    return filter;
  }

  /**
   * sets a feature-filter as defined in WFS specifications.
   * 
   * @param filter
   *          the filter of the FeatureTypeConstraints
   */
  public void setFilter( Filter filter )
  {
    this.filter = filter;
  }

  /**
   * returns the extent for filtering the feature type
   * 
   * @return the extent for filtering the feature type
   */
  public Extent[] getExtents()
  {
    return (Extent[])extents.toArray( new Extent[extents.size()] );
  }

  /**
   * sets the extent for filtering the feature type
   * 
   * @param extents
   *          extents for filtering the feature type
   */
  public void setExtents( Extent[] extents )
  {
    this.extents.clear();

    if( extents != null )
    {
      for( int i = 0; i < extents.length; i++ )
      {
        addExtent( extents[i] );
      }
    }
  }

  /**
   * Adds an Extent to the Extent-List of a FeatureTypeConstraint
   * 
   * @param extent
   *          an extent to add
   */
  public void addExtent( Extent extent )
  {
    extents.add( extent );
  }

  /**
   * Removes an Extent from the Extent-List of a FeatureTypeConstraint
   * 
   * @param extent
   *          an extent to remove
   */
  public void removeExtent( Extent extent )
  {
    extents.remove( extents.indexOf( extent ) );
  }

  /**
   * 
   * 
   * @return the FeatureTypeConstraint as String
   */
  public String toString()
  {
    String ret = getClass().getName() + "\n";
    ret = "featureTypeName = " + featureTypeName + "\n";
    ret += ( "filter = " + filter + "\n" );
    ret += ( "extents = " + extents + "\n" );

    return ret;
  }

  /**
   * exports the content of the FeatureTypeConstraint as XML formated String
   * 
   * @return xml representation of the FeatureTypeConstraint
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<FeatureTypeConstraint>" );
    sb.append( "<FeatureTypeName>" ).append( featureTypeName );
    sb.append( "</FeatureTypeName>" );
    if( filter != null )
    {
      sb.append( filter.toXML() );
    }
    if( extents != null )
    {
      for( int i = 0; i < extents.size(); i++ )
      {
        sb.append( ( (Marshallable)extents.get( i ) ).exportAsXML() );
      }
    }
    sb.append( "</FeatureTypeConstraint>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}