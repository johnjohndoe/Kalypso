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
package org.kalypso.model.product.utils;

import org.eclipse.osgi.framework.internal.core.FrameworkProperties;

/**
 * Utility class, which helps retrieving system/platform properties.
 * 
 * @author Holger Albert
 */
@SuppressWarnings("restriction")
public class ConfigPropertyUtilities
{

  /**
   * The property for the rights of the user.
   */
  public static final String PROPERTY_EXPERT = "org.kalypso.model.product.expert"; //$NON-NLS-1$

  /**
   * The default value of the property for the rights of the user.
   */
  public static final String PROPERTY_DEFAULT_EXPERT = "false"; //$NON-NLS-1$

  private ConfigPropertyUtilities( )
  {
  }

  /**
   * This function returns true, if the user should have expert privileges.
   * 
   * @return True, if the user should be an expert.
   */
  public static boolean isExpert( )
  {
    /* Get the property. */
    final String expert = FrameworkProperties.getProperty( PROPERTY_EXPERT, PROPERTY_DEFAULT_EXPERT );

    /* If it is not set or set to false, the user has no expert rights. */
    if( expert == null || expert.equalsIgnoreCase( "false" ) ) //$NON-NLS-1$
      return false;

    return true;
  }
}