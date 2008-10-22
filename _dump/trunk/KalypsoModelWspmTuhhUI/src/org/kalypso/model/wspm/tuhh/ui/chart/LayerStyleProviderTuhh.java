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

  private void createStyles( )
  {
    m_styles = new HashMap<String, IStyle>();
    // TODO: read styles from *.kod file
    createPointMarkerSytles();
    createCrossSectionSytles();
    createRoughnessSytles();
    createBridgeSytles();
    createWeirSytles();
  }

  private void createPointMarkerSytles( )
  {
    IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );

    final ILineStyle lsT = getStyleFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE + "_LINE", LineStyle.class );
    lsT.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_TRENNFLAECHE ) );

    final ILineStyle lsD = lsT.copy();
    lsD.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE ) );
    m_styles.put( IWspmTuhhConstants.MARKER_TYP_DURCHSTROEMTE + "_LINE", lsD );

    final ILineStyle lsB = lsT.copy();
    lsB.setColor( markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_BORDVOLL ) );
    m_styles.put( IWspmTuhhConstants.MARKER_TYP_BORDVOLL + "_LINE", lsB );
  }

  private void createWeirSytles( )
  {
    IProfilPointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( TuhhProfil.PROFIL_TYPE );
    final RGB col = markerProvider.getColorFor( IWspmTuhhConstants.MARKER_TYP_WEHR );

    final ILineStyle lsT = getStyleFor( IWspmTuhhConstants.MARKER_TYP_WEHR + "_LINE", LineStyle.class );
    lsT.setColor( col );

    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR + "_LINE", LineStyle.class );
    ls.setColor( col );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR + "_LINE_ACTIVE", ls.copy() );

    final IPointStyle ps = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR + "_POINT", PointStyle.class );
    ps.setInlineColor( col );
    ps.getStroke().setColor( col );
    ps.setWidth( 5 );
    ps.setHeight( 5 );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR + "_POINT_ACTIVE", ps.copy() );
  }

  private void createCrossSectionSytles( )
  {
    final RGB yellow = new RGB( 255, 150, 0 );
    final RGB red = new RGB( 255, 0, 0 );

    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_LINE", LineStyle.class );
    ls.setColor( yellow );

    final ILineStyle ls_A = ls.copy();
    ls_A.setColor( red );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_LINE_ACTIVE", ls_A );

    final IPointStyle ps = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_POINT", PointStyle.class );
    ps.setInlineColor( yellow );
    ps.getStroke().setColor( yellow );
    ps.setWidth( 5 );
    ps.setHeight( 5 );

    final IPointStyle ps_A = ps.copy();
    ps_A.setInlineColor( red );
    ps_A.getStroke().setColor( red );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_POINT_ACTIVE", ps_A );

    ((ILineStyle) getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE + "_STATIONLINE_LINE", LineStyle.class )).setDash( 0f, new float[] { 1, 1, 1 } );
  }

  private void createBridgeSytles( )
  {
    final RGB green = new RGB( 0, 128, 0 );
    final RGB blue = new RGB( 0, 128, 179 );

    final ILineStyle ls = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE + "_LINE", LineStyle.class );
    ls.setColor( green );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE + "_LINE_ACTIVE", ls.copy() );

    final IPointStyle ps = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE + "_POINT", PointStyle.class );
    ps.setInlineColor( green );
    ps.getStroke().setColor( green );
    ps.setWidth( 5 );
    ps.setHeight( 5 );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE + "_POINT_ACTIVE", ps.copy() );

    final ILineStyle lsU = ls.copy();
    lsU.setColor( blue );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE + "_LINE", lsU );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE + "_LINE_ACTIVE", lsU.copy() );

    final IPointStyle psU = ps.copy();
    psU.setInlineColor( blue );
    psU.getStroke().setColor( blue );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE + "_POINT", psU );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE + "_POINT_ACTIVE", psU.copy() );
  }

  private void createRoughnessSytles( )
  {
    final IPointStyle psKS = getStyleFor( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS + "_POINT", PointStyle.class );
    psKS.getStroke().setColor( new RGB( 0, 0, 0 ) );
    psKS.setInlineColor( new RGB( 0, 0, 0 ) );
    psKS.setAlpha( 50 );
    m_styles.put( IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST + "_POINT", psKS.copy() );
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
