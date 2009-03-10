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
package org.kalypso.ogc.gml.map.properties;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.PropertyTester;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.KalypsoUIDebug;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * This property tester tests a mapPanel for its active theme.
 * 
 * @author Gernot Belger
 */
public class ActiveThemeTester extends PropertyTester
{
  private static final String PROPERTY_BASE = "activeTheme";

  /** Property 'activeThemeNotNull': tests, returns <code>true</code>, if an active theme is set. */
  private final static String PROPERTY_NOTNULL = PROPERTY_BASE + "NotNull";

  /**
   * Property 'activeThemeQName': compares the given argument to the toString of the qname of the active theme. Returns
   * <code>false</code>, if the active theme is not an feature theme.
   */
  private final static String PROPERTY_QNAME = PROPERTY_BASE + "QName";

  /** Property 'activeThemeNotNull': tests, returns <code>true</code>, if the active theme has a non trivial extent. */
  private final static String PROPERTY_HASEXTENT = PROPERTY_BASE + "HasExtent";

  /**
   * Property 'activeThemeNotNull': tests, returns <code>true</code>, if the active theme is a feature theme (i.e.
   * inherits from {@link IKalypsoFeatureTheme}).
   */
  private final static String PROPERTY_ISFEATURETHEME = PROPERTY_BASE + "IsFeatureTheme";

  /**
   * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object, java.lang.String, java.lang.Object[],
   *      java.lang.Object)
   */
  public boolean test( final Object receiver, final String property, final Object[] args, final Object expectedValue )
  {
    if( !(receiver instanceof IMapPanel) )
      return false;

    final IMapPanel mapPanel = (IMapPanel) receiver;
    final IMapModell mapModell = mapPanel.getMapModell();
    if( mapModell == null )
      return false;

    final IKalypsoTheme theme = mapModell.getActiveTheme();

    final boolean result = test( theme, property, expectedValue );
    
    KalypsoUIDebug.PROPERTY_TESTER.printf( "Testing property '%s' for expectedValue '%s' on theme '%s': %s%n", property, expectedValue, theme, result );
    
    return result;
  }

  private boolean test( final IKalypsoTheme theme, final String property, final Object expectedValue )
  {
    if( PROPERTY_NOTNULL.equals( property ) )
      return testNotNull( theme );

    if( PROPERTY_QNAME.equals( property ) )
      return testQName( theme, expectedValue );

    if( PROPERTY_HASEXTENT.equals( property ) )
      return testHasExtent( theme );

    if( PROPERTY_ISFEATURETHEME.equals( property ) )
      return testIsFeatureTheme( theme );

    return false;
  }

  private boolean testIsFeatureTheme( final IKalypsoTheme theme )
  {
    return theme instanceof IKalypsoFeatureTheme;
  }

  private boolean testHasExtent( final IKalypsoTheme theme )
  {
    if( theme == null )
      return false;

    final GM_Envelope fullExtent = theme.getFullExtent();

    return fullExtent != null;
  }

  private boolean testQName( final IKalypsoTheme theme, final Object expectedValue )
  {
    if( theme == null || expectedValue == null )
      return false;

    if( !(theme instanceof IKalypsoFeatureTheme) )
      return false;

    final IFeatureType featureType = ((IKalypsoFeatureTheme) theme).getFeatureType();
    if( featureType == null )
      return false;
    
    final QName expectedQName = QName.valueOf( expectedValue.toString() );
    return GMLSchemaUtilities.substitutes( featureType, expectedQName );
  }

  private boolean testNotNull( final IKalypsoTheme theme )
  {
    return theme != null;
  }

}
