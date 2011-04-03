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

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.BuildingUtil;
import org.kalypso.model.wspm.tuhh.core.profile.buildings.IProfileBuilding;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;

import de.openali.odysseus.chart.framework.model.figure.IFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.AbstractFigure;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.model.style.IPointStyle;

/**
 * @author kimwerner
 */
public class CulvertLayer extends AbstractProfilLayer
{
  public CulvertLayer( final IProfil profil, final ILayerStyleProvider styleProvider )
  {
    super( IWspmTuhhConstants.LAYER_TUBES, profil, IWspmConstants.POINT_PROPERTY_HOEHE, styleProvider );
    getLineStyle().setColor( new RGB( 255, 255, 100 ) );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getId()
   */
  @Override
  public String getIdentifier( )
  {
    return getTube() == null ? Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.TubeLayer.0" ) : getTube().getObservation().getName(); //$NON-NLS-1$
  }

  private IProfileBuilding getTube( )
  {
    final IProfileBuilding[] objects = getProfil().getProfileObjects( IProfileBuilding.class );
    if( ArrayUtils.isEmpty( objects ) )
      return null;

    return objects[0];
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return getTube() == null ? "" : getTube().getObservation().getDescription(); //$NON-NLS-1$
  }

  /**
   * @see de.openali.odysseus.chart.framework.model.layer.IChartLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( final GC gc )
  {
    final IProfileBuilding tube = getTube();
    if( tube == null )
      return;

    final Double x = BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_X, tube );
    final Double y = BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BEZUGSPUNKT_Y, tube );
    final Double b = BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_BREITE, tube );
    final Double h = BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_HOEHE, tube );
    final Double m = BuildingUtil.getDoubleValueFor( IWspmTuhhConstants.BUILDING_PROPERTY_STEIGUNG, tube );

    final IAxis targetAx = getTargetAxis();
    final IAxis domAx = getDomainAxis();
    final String tubeId = getTube().getId();

    IFigure<IPointStyle> tubeFigure = null;

    if( getTube().getId().equals( IWspmTuhhConstants.BUILDING_TYP_TRAPEZ ) )
    {
      if( x.isNaN() || y.isNaN() || b.isNaN() || m.isNaN() || h.isNaN() )
        return;
      tubeFigure = new AbstractFigure<IPointStyle>()
      {
        @Override
        protected void paintFigure( final GC grc )
        {
          final int leftX = domAx.numericToScreen( x - b / 2 );
          final int upperY = targetAx.numericToScreen( y + h );
          final int rightX = domAx.numericToScreen( x + b / 2 );
          final int lowerY = targetAx.numericToScreen( y );
          final int deltaX = m.intValue() > 0 ? (lowerY - upperY) / m.intValue() : 0;
          grc.fillPolygon( new int[] { leftX, lowerY, leftX - deltaX, upperY, rightX + deltaX, upperY, rightX, lowerY } );
        }
      };
    }

    else if( tubeId.equals( IWspmTuhhConstants.BUILDING_TYP_KREIS ) || tubeId.equals( IWspmTuhhConstants.BUILDING_TYP_EI ) || tubeId.equals( IWspmTuhhConstants.BUILDING_TYP_MAUL ) )
    {
      if( x.isNaN() || y.isNaN() || b.isNaN() )
        return;
      tubeFigure = new AbstractFigure<IPointStyle>()
      {
        @Override
        protected void paintFigure( final GC grc )
        {
          final int leftX = domAx.numericToScreen( x - b / 2 );
          final int upperY = targetAx.numericToScreen( y + (h.isNaN() ? b : h) );
          final int rightX = domAx.numericToScreen( x + b / 2 );
          final int lowerY = targetAx.numericToScreen( y );
          grc.fillOval( leftX, upperY, rightX - leftX, lowerY - upperY );
        }
      };

    }
    if( tubeFigure == null )
      return;
    tubeFigure.setStyle( getPointStyle() );
    tubeFigure.paint( gc );
  }
}
