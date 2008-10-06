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
package org.kalypso.ogc.gml.map.themeinfo;

import java.util.Formatter;
import java.util.List;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.IKalypsoThemeInfo;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * {@link IKalypsoThemeInfo}-implementation for features which contain
 * {@link org.kalypsodeegree.model.geometry.GM_TriangulatedSurface}s.<br>
 * Show the interpolated value at the given position.
 * 
 * @author Gernot Belger
 */
public class TriangulatedSurfaceThemeInfo implements IKalypsoThemeInfo
{
  private static final String DEFAULT_FORMAT_STRING = Messages.getString("org.kalypso.ogc.gml.map.themeinfo.TriangulatedSurfaceThemeInfo.0"); //$NON-NLS-1$

  /**
   * Value of the property for a format string.<br>
   * A {@link Formatter}-style format string, can only contain one variable of type f. Example:
   * <code>Value: %.3f</code>.<br>
   * Defaults to <code>Wert: %.2f</code>.
   */
  public final static String PROP_FORMAT = "format"; //$NON-NLS-1$

  /**
   * Value of the property which indicates the name of the featue-property containing the triangulated surface geometry.<br>
   * Must be a qname of the form {namespace}localPart.<br>
   * If not set, the first property of type gml:TriangulatedSurface will be used.
   */
  public final static String PROP_GEOMETRY = "geometry"; //$NON-NLS-1$

  private IKalypsoFeatureTheme m_theme;

  private String m_formatString;

  private QName m_geomName;

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#init(org.kalypso.ogc.gml.IKalypsoTheme, java.util.Properties)
   */
  public void init( final IKalypsoTheme theme, final Properties props )
  {
    Assert.isLegal( theme instanceof IKalypsoFeatureTheme );

    m_theme = (IKalypsoFeatureTheme) theme;

    final IFeatureType featureType = m_theme.getFeatureType();

    m_formatString = props.getProperty( PROP_FORMAT, DEFAULT_FORMAT_STRING );

    final String qnameStr = props.getProperty( PROP_GEOMETRY, null );
    m_geomName = qnameStr == null ? null : QName.valueOf( qnameStr );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  public void appendInfo( final Formatter formatter, final GM_Position pos )
  {
    // not yet implemented, use quick-info
    appendQuickInfo( formatter, pos );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendQuickInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_theme );

    final CommandableWorkspace workspace = m_theme.getWorkspace();

    final FeatureList featureList = m_theme.getFeatureList();
    if( featureList == null )
      return;

    final List tins = featureList.query( pos, null );
    for( final Object tinObject : tins )
    {
      /* Search for the first feature which provides a value */
      final Feature feature = FeatureHelper.getFeature( workspace, tinObject );

      final GM_TriangulatedSurface surface = findSurface( feature );
      if( surface == null )
        return;
      final double value = surface.getValue( pos );
      if( !Double.isNaN( value ) )
      {
        /* Replace tokens like in feature-annotations */
        final String formatString = FeatureHelper.tokenReplace( feature, m_formatString );

        formatter.format( formatString, value );
        return;
      }
    }
  }

  private GM_TriangulatedSurface findSurface( final Feature feature )
  {
    if( m_geomName == null )
    {
      final GM_Object[] geometryProperties = feature.getGeometryProperties();
      for( final GM_Object geom : geometryProperties )
      {
        if( geom instanceof GM_TriangulatedSurface )
          return (GM_TriangulatedSurface) geom;
      }

      return null;
    }

    return (GM_TriangulatedSurface) feature.getProperty( m_geomName );
  }

}
