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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.HashMap;

import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfilPointMarkerProvider;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.TuhhProfil;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;

import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;
import de.openali.odysseus.chart.framework.model.style.IStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.PointStyle;
import de.openali.odysseus.chart.framework.util.StyleUtils;

/**
 * @author kimwerner
 */
public class LayerStyleProviderTuhh implements ILayerStyleProvider
{
  private HashMap<String, IStyle> m_styles = null;

  public void createStyles( )
  {
    m_styles = new HashMap<String, IStyle>();
    // TODO: read styles from *.kod file

    IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );
    ((ILineStyle) getStyleFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + "_LINE", LineStyle.class )).setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );
    ((ILineStyle) getStyleFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE + "_LINE", LineStyle.class )).setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    ((ILineStyle) getStyleFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL + "_LINE", LineStyle.class )).setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );

    ((ILineStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_STATIONLINE_LINE", LineStyle.class )).setDash( 0f, new float[] { 1, 1, 1 } );

    ((IPointStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS + "_POINT", PointStyle.class )).getStroke().setColor( new RGB( 0, 0, 0 ) );
    ((IPointStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS + "_POINT", PointStyle.class )).setInlineColor( new RGB( 0, 0, 0 ) );
    ((IPointStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS + "_POINT", PointStyle.class )).setAlpha(50 );

    ((ILineStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_LINE_ACTIVE", LineStyle.class )).setColor( new RGB( 255, 0, 0 ) );
    ((IPointStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_POINT_ACTIVE", PointStyle.class )).setInlineColor( new RGB( 255, 0, 0 ) );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.IComponentStyleProvider#dispose()
   * @deprecated styles dispose themselves
   */
  public void dispose( )
  {

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.ILayerStyleProvider#getStyleFor(java.lang.String, java.lang.Class)
   */
  @SuppressWarnings("unchecked")
  public <T extends IStyle> T getStyleFor( String id, Class<T> defaultStyle )
  {
    if( m_styles == null )
      createStyles();
    final IStyle style = m_styles.get( id );
    if( (style != null) )
      return (T) style;
    final IStyle newStyle = defaultStyle == null ? null : StyleUtils.getDefaultStyle( defaultStyle );
    if( newStyle != null )
      m_styles.put( id, newStyle );
    return (T) newStyle;

  }
}
