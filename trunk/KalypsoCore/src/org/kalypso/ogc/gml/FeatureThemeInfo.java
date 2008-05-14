/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.util.Formatter;
import java.util.List;
import java.util.Properties;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.Assert;
import org.kalypso.core.i18n.Messages;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class FeatureThemeInfo implements IKalypsoThemeInfo
{
  private IKalypsoFeatureTheme m_theme = null;

  private String m_format;

  private QName m_geom;

  public FeatureThemeInfo( )
  {
    // empty
  }

  public FeatureThemeInfo( final KalypsoFeatureTheme theme, final Properties props )
  {
    init( theme, props );
  }

  /**
   * Final, because called from a constructor.
   * 
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#init(org.kalypso.ogc.gml.IKalypsoTheme)
   */
  public final void init( final IKalypsoTheme theme, final Properties props )
  {
    Assert.isLegal( theme instanceof IKalypsoFeatureTheme );

    m_theme = (IKalypsoFeatureTheme) theme;

    m_format = props.getProperty( "format" ); //$NON-NLS-1$
    final String geomStr = props.getProperty( "geometry" ); //$NON-NLS-1$
    m_geom = geomStr == null ? null : QName.valueOf( geomStr );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  public void appendInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_theme, Messages.getString("org.kalypso.ogc.gml.FeatureThemeInfo.2") ); //$NON-NLS-1$

    // not yet implemented
    appendQuickInfo( formatter, pos );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendQuickInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_theme, Messages.getString("org.kalypso.ogc.gml.FeatureThemeInfo.4") ); //$NON-NLS-1$

    final FeatureList featureList = m_theme.getFeatureList();
    if( featureList == null )
    {
      formatter.format( Messages.getString("org.kalypso.ogc.gml.FeatureThemeInfo.5") ); //$NON-NLS-1$
      return;
    }

    final List foundFeatures = featureList.query( pos, null );
    if( foundFeatures.size() == 0 )
    {
      formatter.format( "-" ); //$NON-NLS-1$
      return;
    }
    
    /** 
     * explanation: it is possible (with ATKIS data) that one shape covers the part 
     * of another's shape area, without intersecting (one shape "inside" another)
     * If several such features contains the same position, 
     * the topmost is actually drawn - and that feature is the last in the query list.
     * That is the reason why we are here searching for the last one. 
     */
    Feature feature = null;
    for( int i = foundFeatures.size() - 1; i >= 0; i-- )
    {
      feature = (Feature) foundFeatures.get( i );
      if( m_geom != null )
      {
        final Object property = feature.getProperty( m_geom );
        if( property instanceof GM_Object && ((GM_Object) property).contains( pos ) )
          break;
      }
      else
      {
        if( feature.getDefaultGeometryProperty().contains( pos ) )
          break;
      }
      feature = null;
    }
    if( feature == null )
    {
      formatter.format( "-" ); //$NON-NLS-1$
      return;
    }
    final String label;
    if( m_format == null )
      label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
    else
      label = FeatureHelper.tokenReplace( feature, m_format );

    formatter.format( "%s", label ); //$NON-NLS-1$
  }

}
