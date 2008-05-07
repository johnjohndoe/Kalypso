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
package org.kalypso.ui.catalogs;

/**
 * Constants used in the ui-properties for qnames.
 * 
 * @author Gernot Belger
 */
public interface IFeatureTypePropertiesConstants
{
  //
  // Common feature constants
  //

  /** How many levels of children should be created if a feature is created (-1 means infinite, 0 means none) */
  public final static String FEATURE_CREATION_DEPTH = "feature.creationDepth"; //$NON-NLS-1$

  public final static String FEATURE_CREATION_DEPTH_DEFAULT = "0"; //$NON-NLS-1$

  //
  // GmlTree Constants
  //

  /** Show children of this element, defaults to true */
  public final static String GMLTREE_SHOW_CHILDREN = "gmltree.showChildren"; //$NON-NLS-1$

  public final static String GMLTREE_SHOW_CHILDREN_DEFAULT = "true"; //$NON-NLS-1$

  //
  // Map Constants
  //

  /**
   * Extension id of a {@link org.kalypso.ogc.gml.IKalypsoThemeInfo} registered with the
   * <code>org.kalypso.core.themeInfo</code> extension-point.
   */
  public static final String THEME_INFO_ID = "map.themeInfoId"; //$NON-NLS-1$
}
