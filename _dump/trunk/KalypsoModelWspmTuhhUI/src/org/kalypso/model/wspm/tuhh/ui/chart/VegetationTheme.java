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

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.model.style.impl.LineStyle;

/**
 * @author kimwerner
 */
public class VegetationTheme extends AbstractProfilTheme

{
  private final ICoordinateMapper m_cm;

  private final ILineStyle m_LineStyle;

  public VegetationTheme( final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    super( IWspmTuhhConstants.LAYER_BEWUCHS, "Bewuchs", chartLayers, null );
    m_LineStyle = (ILineStyle) styleProvider.getStyleFor( chartLayers[0].getId() + "LINE", LineStyle.class );
    m_LineStyle.setColor( new RGB(0,255,0) );
    m_cm = cm;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilTheme#getHover(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHover( Point pos )
  {
    // TODO Auto-generated method stub
    return super.getHover( pos );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilTheme#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {
    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length - 2;
    final PolylineFigure pf = new PolylineFigure();

    pf.setStyle( getLineStyle() );
    for( int i = 0; i < len; i++ )
    {
      final Point point1 = m_cm.numericToScreen( ProfilUtil.getDoubleValueFor( getDomainComponent().getId(), profilPoints[i] ), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, profilPoints[i] )+1 );
      final Point point2 = m_cm.numericToScreen( ProfilUtil.getDoubleValueFor( getDomainComponent().getId(), profilPoints[i + 1] ), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, profilPoints[i + 1] )+1 );
      pf.setPoints( new Point[] { point1, point2 } );
      pf.paint( gc );
    }
  }

}
