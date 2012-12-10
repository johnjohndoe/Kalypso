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
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilChange;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.ui.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;
import org.kalypso.observation.result.IRecord;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;

/**
 * @author kimwerner
 */
public class VegetationTheme extends AbstractProfilTheme
{
  public static final String TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.VegetationTheme.2" ); //$NON-NLS-1$

  public VegetationTheme( final IProfil profil, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    super( profil, IWspmTuhhConstants.LAYER_BEWUCHS, TITLE, chartLayers, cm );
    setLineStyle( styleProvider.getStyleFor( IWspmTuhhConstants.LAYER_BEWUCHS + "_LINE", ILineStyle.class ) ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#onProfilChanged(org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint,
   *      org.kalypso.model.wspm.core.profil.IProfilChange[])
   */
  @Override
  public void onProfilChanged( final ProfilChangeHint hint, final IProfilChange[] changes )
  {
    if( hint.isActivePointChanged() || hint.isPointValuesChanged() )
    {
      fireLayerContentChanged();
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#createLegendEntries()
   */
  @Override
  public ILegendEntry[] createLegendEntries( )
  {

    {
      final LegendEntry le = new LegendEntry( this, toString() )
      {
        @Override
        public void paintSymbol( final GC gc, final Point size )
        {
          final Rectangle clipping = gc.getClipping();
          drawIcon( gc, new Rectangle( clipping.x + clipping.width / 2, clipping.y + clipping.height, clipping.width, clipping.height ) );
        }
      };

      return new ILegendEntry[] { le };
    }
  }

  private Rectangle getHoverRectInternal( final IRecord pp1, final IRecord pp2 )
  {
    if( segmenthasVegetation( pp1 ) )
    {
      final Double y1 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE, pp1 );
      final Double y2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_HOEHE, pp2 );
      final Double x1 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, pp1 );
      final Double x2 = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BREITE, pp2 );

      final Point p1 = new Point( getDomainAxis().numericToScreen( x1 ), getTargetAxis().numericToScreen( y1 ) - 3 );
      final Point p2 = new Point( getDomainAxis().numericToScreen( x2 ), getTargetAxis().numericToScreen( y2 ) - 3 );

      final int width = p2.x - p1.x;
      final int midX = p1.x + width / 2;
      final int height = Math.abs( p1.y - p2.y );
      final int midY = Math.min( p1.y, p2.y ) + height / 2;

      return new Rectangle( midX, midY, width, height );

    }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#getHover(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHover( final Point pos )
  {
    final IProfil profil = getProfil();
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length - 2;

    for( int i = 0; i < len; i++ )
    {
      final Rectangle hover = getHoverRectInternal( profilPoints[i], profilPoints[i + 1] );
      if( hover == null )
        continue;
      final int size = Math.min( hover.height / 2, Math.min( hover.width / 2, 10 ) );
      if( (pos.x >= hover.x - size) && (pos.y >= hover.y - size) && (pos.x < hover.x + size) && (pos.y < hover.y + size) )
        return new EditInfo( this, null, null, i, getTooltipInfo( profilPoints[i] ), pos );
    }
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#getLegendNodes()
   */
  @Override
  public IChartLayer[] getLegendNodes( )
  {
    return new IChartLayer[] {};
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTooltipInfo(org.kalypso.observation.result.IRecord)
   */
  @Override
  public String getTooltipInfo( final IRecord point )
  {
    final Double ax = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, point );
    final Double ay = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY, point );
    final Double dp = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP, point );
    return String.format( " AX: %.4f %n AY: %.4f %n DP: %.4f", new Object[] { ax, ay, dp } ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#removeYourself()
   */
  @Override
  public void removeYourself( )
  {
    final IProfil profil = getProfil();
    final ProfilOperation operation = new ProfilOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.VegetationTheme.1" ), getProfil(), true ); //$NON-NLS-1$
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_BEWUCHS_AX ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP ) ) );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#getTargetRange(de.openali.odysseus.chart.framework.model.mapper.IAxis)
   */
  @Override
  public IDataRange<Number> getTargetRange( final IDataRange<Number> domainIntervall )
  {
    // don't calculate axis size and ticks
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.chart.AbstractProfilTheme#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( final GC gc )
  {

    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length - 1;
    final PolylineFigure pf = new PolylineFigure();

    pf.setStyle( getLineStyle() );
    for( int i = 0; i < len; i++ )
    {

      final Rectangle hover = getHoverRectInternal( profilPoints[i], profilPoints[i + 1] );
      if( hover == null )
        continue;
      drawIcon( gc, hover );

    }
  }

  protected void drawIcon( final GC gc, final Rectangle clipping )
  {
    getLineStyle().apply( gc );
    if( clipping.width > 12 )
    {
      final int size = Math.min( clipping.width, 20 );
      final int left = clipping.x - size / 2;
      final int top = clipping.y - size / 2;
      final int right = left + size;
      final int bottom = top + size;

      gc.drawLine( clipping.x - 3, bottom - size / 2, clipping.x + 3, bottom - size / 2 );
      gc.drawLine( clipping.x, bottom - size / 2, clipping.x, clipping.y - size / 2 );
      gc.drawOval( left + 2, top - size / 2, right - left - 4, bottom - clipping.y + 4 );
    }
    else
      gc.drawLine( clipping.x, clipping.y - 12, clipping.x, clipping.y );
  }

  final boolean segmenthasVegetation( final IRecord point )
  {
    final Double ax = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AX, point );
    final Double ay = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_AY, point );
    final Double dp = ProfilUtil.getDoubleValueFor( IWspmTuhhConstants.POINT_PROPERTY_BEWUCHS_DP, point );
    return !ax.isNaN() && !ay.isNaN() && !dp.isNaN() && ax * ay * dp != 0;

  }
}
