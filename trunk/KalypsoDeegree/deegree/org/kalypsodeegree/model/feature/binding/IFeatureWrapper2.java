/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree.model.feature.binding;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * The better feature wrapper. Use instead of IFeatureWrapper.<br>
 * TODO: rename and implement the {@link Feature}-Interface.
 * 
 * @author Gernot Belger
 */
public interface IFeatureWrapper2
{
  /** Returns the id of the bound feature. */
  public String getGmlID( );

  /** Returns the gml:name property of the bound feature. */
  public String getName( );

  /** Sets the gml:name property */
  public void setName( final String name );

  /** Returns the gml:description property of the bound feature. */
  public String getDescription( );

  /** Sets the gml_description property */
  public void setDescription( final String desc );

  /**
   * Return the gml:location property of the bound feature.<br>
   * REMARK: gml:location is deprecated in the GML3-Schema.
   */
  public GM_Object getLocation( );

  /**
   * Sets the gml:location property to the bound feature.<br>
   * REMARK: gml:location is deprecated in the GML3-Schema.
   */
  public void setLocation( final GM_Object location );

  /** Returns the bound feature, which this wrapper class is representing. */
  public Feature getFeature( );
}
