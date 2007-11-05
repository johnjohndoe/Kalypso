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

import java.util.Formatter;
import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.Assert;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Gernot Belger
 */
public class DefaultFeatureThemeInfo implements IKalypsoThemeInfo
{
  private IKalypsoFeatureTheme m_theme = null;

  public DefaultFeatureThemeInfo( )
  {
    // empty
  }

  public DefaultFeatureThemeInfo( final KalypsoFeatureTheme theme, final Properties props )
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
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  public void appendInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_theme, "call init before first use" );

    // not yet implemented
    appendQuickInfo( formatter, pos );
  }

  /**
   * @see org.kalypso.ogc.gml.IKalypsoThemeInfo#appendQuickInfo(java.util.Formatter,
   *      org.kalypsodeegree.model.geometry.GM_Position)
   */
  @SuppressWarnings("unchecked")
  public void appendQuickInfo( final Formatter formatter, final GM_Position pos )
  {
    Assert.isNotNull( m_theme, "call init before first use" );

    final FeatureList featureList = m_theme.getFeatureList();
    if( featureList == null )
    {
      formatter.format( "- keine Daten geladen -" );
      return;
    }

    final List foundFeature = featureList.query( pos, null );
    if( foundFeature.size() == 0 )
    {
      formatter.format( "-" );
      return;
    }

    final CommandableWorkspace workspace = m_theme.getWorkspace();

    final Object object = foundFeature.get( 0 );
    final Feature feature = FeatureHelper.getFeature( workspace, object );

    final String label = FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
    formatter.format( "%s", label );
  }

}
