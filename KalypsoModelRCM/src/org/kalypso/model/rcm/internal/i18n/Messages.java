/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.rcm.internal.i18n;

import java.util.ResourceBundle;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.contribs.java.i18n.I18nUtils;

/**
 * @author Gernot Belger
 */
public class Messages
{
  private static final String BUNDLE_NAME = "org.kalypso.model.rcm.internal.i18n.messages"; //$NON-NLS-1$

  private static final ResourceBundle RESOURCE_BUNDLE = ResourceBundle.getBundle( BUNDLE_NAME );

  private Messages( )
  {
  }

  /**
   * java reflections needs this method-signatur
   */
  public static String getString( final String key )
  {
    return getString( key, ArrayUtils.EMPTY_OBJECT_ARRAY );
  }

  public static String getString( final String key, final Object... args )
  {
    return I18nUtils.formatMessage( RESOURCE_BUNDLE, key, args );
  }
}