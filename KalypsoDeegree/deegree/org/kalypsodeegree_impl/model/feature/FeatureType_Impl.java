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

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;

/**
 * 
 * 
 * 
 * <p>
 * -----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version $Revision$ $Date$
 */
class FeatureType_Impl implements FeatureType, Serializable
{

  private FeatureType[] parents = null;

  private FeatureType[] children = null;

  private String name = "";

  private FeatureTypeProperty[] properties = null;

  public FeatureType_Impl( FeatureType[] parents, FeatureType[] children, String name,
      FeatureTypeProperty[] properties )
  {
    this.parents = parents;
    this.children = children;
    this.name = name;
    this.properties = properties;
  }

  /**
   * returns the direct parents of the FeatureType. If it hasn't a parent null
   * should be returned
   */
  public FeatureType[] getParents()
  {
    return parents;
  }

  /**
   * returns the direct children of the FeatureType
   */
  public FeatureType[] getChildren()
  {
    return children;
  }

  /**
   * returns the name of the FeatureType
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns the properties of this feature type
   */
  public FeatureTypeProperty[] getProperties()
  {
    return properties;
  }

  /**
   * returns a property of this feature type identified by its name
   */
  public FeatureTypeProperty getProperty( String name )
  {
    FeatureTypeProperty ftp = null;
    for( int i = 0; i < properties.length; i++ )
    {
      if( properties[i].getName().equals( name ) )
      {
        ftp = properties[i];
        break;
      }
    }
    return ftp;
  }

  public String toString()
  {
    String ret = null;
    ret = "parents = " + parents + "\n";
    ret += "children = " + children + "\n";
    ret += "name = " + name + "\n";
    ret += "properties = ";
    for( int i = 0; i < properties.length; i++ )
    {
      ret += properties[i] + "\n";
    }
    return ret;
  }

  /**
   * @link aggregationByValue
   * @clientCardinality 0..*
   */
  /* # public final static FeatureTypeProperty_Impl lnkFeatureTypeProperty; */
}