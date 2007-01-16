/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
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
