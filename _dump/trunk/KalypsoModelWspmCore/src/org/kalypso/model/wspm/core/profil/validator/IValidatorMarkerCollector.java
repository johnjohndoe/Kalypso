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
package org.kalypso.model.wspm.core.profil.validator;

import org.eclipse.core.runtime.CoreException;

public interface IValidatorMarkerCollector
{
  public static final String MARKER_ATTRIBUTE_POINTPOS = "profile.marker.attribute.pointpos";

  public static final String MARKER_ATTRIBUTE_POINTPROPERTY = "profile.marker.attribute.pointProperty";

  public static final String MARKER_ATTRIBUTE_QUICK_FIX_PLUGINID = "profile.marker.attribute.quickFix.pluginid";

  public static final String MARKER_ATTRIBUTE_QUICK_FIX_RESOLUTIONS = "profile.marker.attribute.quickFix.resolutions";

  /**
   * Creates a (profile-)marker on the given resource. All validation rules should use this method, so changes in the
   * implementation (e.g. the type of the marker) are reflekted on all rules.
   * 
   * @param resoultionPluginId
   *          the plugin-id from which to instantiate the resolutionClasses
   * @param resolutionClasses
   *          An array of class-names of marker resolutions (quick fixes)
   *          <p>
   *          these classes must not contain references to Objects, only simple Datatypes are allowed
   * @throws CoreException
   */
  public void createProfilMarker( final boolean isSevere, final String message, final String location, final int pointPos, final String pointProperty, final String resolutionPluginId, final Object[] markerResolutions ) throws CoreException;

  /**
   * Clear all markers which may apply to this collector
   * 
   * @throws CoreException
   */
  public void reset( ) throws CoreException;
}
