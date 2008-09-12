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
package org.kalypsodeegree.model.feature.validation;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;

public interface IFeatureMarkerCollector
{
  /**
   * Creates a (problem-)marker on the given resource. All validation rules should use this method, so changes in the
   * implementation (e.g. the type of the marker) are reflected on all rules.
   * 
   * @param feature
   *          The feature wherre the problem has occured, may not be <code>null</code>
   * @param property
   *          The property, where the problem has occured. If <code>null</code>, the problem applies to the wqhole
   *          feature.
   * @param resoultionPluginId
   *          the plugin-id from which to instantiate the resolutionClasses
   * @param resolutionClasses
   *          An array of class-names of marker resolutions (quick fixes)
   *          <p>
   *          these classes must not contain references to Objects, only simple Datatypes are allowed
   * @throws CoreException
   */
  public void createMarker( final Feature feature, final IPropertyType property, final boolean isSevere, final String message, final String location, final String resolutionPluginId, final Object[] markerResolutions ) throws CoreException;

  /**
   * Clear all markers which may apply to this collector
   * 
   * @throws CoreException
   */
  public void reset( ) throws CoreException;
}
