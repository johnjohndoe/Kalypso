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
package org.kalypso.ogc.gml;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.PropertyTester;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;

/**
 * This class is for testing properties of {@link IKalypsoFeatureTheme}. Supported properties are:
 * <li> feature type (including substitution) </li>
 * 
 * @author Stefan Kurzbach
 */
public class KalypsoFeatureThemeTester extends PropertyTester
{

  /**
   * test the feature type of the theme
   */
  private static final String FEATURE_TYPE = "featureType"; //$NON-NLS-1$

  /**
   * include this argument if the theme must substitute the given feature type instead of being equal to it
   */
  private static final String ARG_SUBSTITUTES = "substitutes"; //$NON-NLS-1$

  /**
   * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object, java.lang.String, java.lang.Object[],
   *      java.lang.Object)
   */
  public boolean test( final Object receiver, final String property, final Object[] args, final Object expectedValue )
  {
    // can only test IKalypsoFeatureTheme
    // return now if input is unexpected
    if( !(receiver instanceof IKalypsoFeatureTheme) || property == null || expectedValue == null || !(expectedValue instanceof String) )
      return false;

    final IKalypsoFeatureTheme theme = (IKalypsoFeatureTheme) receiver;

    // parse arguments
    boolean substitute = false;
    for( final Object arg : args )
    {
      if( ARG_SUBSTITUTES.equals( arg ) )
        substitute = true;
    }

    // tests if the feature type qName string representation is equal to the expected value
    if( FEATURE_TYPE.equals( property ) )
    {
      // check feature type of theme
      final IFeatureType featureType = theme.getFeatureType();

      // if feature type is null, return false
      // ignore null qName?
      // TODO: maybe wait for feature type to become available? blocking might not be feasible though
      if( featureType == null || featureType.getQName() == null )
        return false;

      if( substitute )
      {
        // check for substitution
        final QName expectedQName = QName.valueOf( (String) expectedValue );
        return GMLSchemaUtilities.substitutes( featureType, expectedQName );
      }
      else
      {
        // check for string equality
        final String themeQName = featureType.getQName().toString();
        return themeQName.equals( expectedValue );
      }
    }
    // other properties here else if

    return false;
  }
}
