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
package org.kalypso.model.wspm.tuhh.ui.chart.themes;

import java.math.BigDecimal;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.IVegetationClass;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfilChangeHint;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfilOperationJob;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.VegetationPanel;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

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

  private ILegendEntry[] m_legendEntries;

  public VegetationTheme( final IProfil profil, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    super( profil, IWspmTuhhConstants.LAYER_BEWUCHS, TITLE, chartLayers, cm );
    setLineStyle( styleProvider.getStyleFor( IWspmTuhhConstants.LAYER_BEWUCHS + "_LINE", ILineStyle.class ) ); //$NON-NLS-1$
  }

  @Override
  public void onProfilChanged( final ProfilChangeHint hint )
  {
    if( hint.isSelectionChanged() || hint.isPointValuesChanged() )
    {
      fireLayerContentChanged();
    }
  }

  private ILegendEntry[] createLegendEntries( )
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

  private Rectangle getHoverRectInternal( final IProfileRecord point1, final IProfileRecord point2 )
  {
    if( Objects.isNull( point1, point2 ) )
      return null;

    if( segmenthasVegetation( point1 ) )
    {
      final Point p1 = new Point( getDomainAxis().numericToScreen( point1.getBreite() ), getTargetAxis().numericToScreen( point1.getHoehe() ) - 3 );
      final Point p2 = new Point( getDomainAxis().numericToScreen( point2.getBreite() ), getTargetAxis().numericToScreen( point2.getHoehe() ) - 3 );

      final int width = p2.x - p1.x;
      final int midX = p1.x + width / 2;
      final int height = Math.abs( p1.y - p2.y );
      final int midY = Math.min( p1.y, p2.y ) + height / 2;

      return new Rectangle( midX, midY, width, height );

    }
    return null;
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    final IProfil profil = getProfil();
    final IProfileRecord[] points = profil.getPoints();

    for( final IProfileRecord p1 : points )
    {
      final IProfileRecord p2 = p1.getNextPoint();
      if( Objects.isNull( p2 ) )
        continue;

      final Rectangle hover = getHoverRectInternal( p1, p2 );
      if( hover == null )
      {
        continue;
      }
      final int size = Math.min( hover.height / 2, Math.min( hover.width / 2, 10 ) );
      if( pos.x >= hover.x - size && pos.y >= hover.y - size && pos.x < hover.x + size && pos.y < hover.y + size )
        return new EditInfo( this, null, null, ArrayUtils.indexOf( points, p1 ), getTooltipInfo( p1 ), pos );
    }

    return null;
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {

    if( ArrayUtils.isEmpty( m_legendEntries ) )
    {
      m_legendEntries = createLegendEntries();
    }
    return m_legendEntries;
  }

  @Override
  public IChartLayer[] getLegendNodes( )
  {
    return new IChartLayer[] {};
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTooltipInfo(org.kalypso.observation.result.IRecord)
   */
  @Override
  public String getTooltipInfo( final IProfileRecord point )
  {
    final Double ax = ProfilUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX, point );
    final Double ay = ProfilUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY, point );
    final Double dp = ProfilUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP, point );

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
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ) ) );
    new ProfilOperationJob( operation ).schedule();
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme#getTargetRange(de.openali.odysseus.chart.framework.model.mapper.IAxis)
   */
  @Override
  public IDataRange< ? > getTargetRange( final IDataRange< ? > domainIntervall )
  {
    // don't calculate axis size and ticks
    return null;
  }

  @Override
  public void paint( final GC gc )
  {
    final IProfil profil = getProfil();
    if( profil == null )
      return;

    final PolylineFigure pf = new PolylineFigure();
    pf.setStyle( getLineStyle() );

    final IProfileRecord[] points = profil.getPoints();
    for( final IProfileRecord point : points )
    {
      final Rectangle hover = getHoverRectInternal( point, point.getNextPoint() );
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
    {
      gc.drawLine( clipping.x, clipping.y - 12, clipping.x, clipping.y );
    }
  }

  final boolean segmenthasVegetation( final IProfileRecord point )
  {
    final Double ax = getAx( point );
    final Double ay = getAy( point );
    final Double dp = getDp( point );

    if( Doubles.isNaN( ax, ax, dp ) )
      return false;

    return ax * ay * dp != 0;

  }

  private Double getDp( final IProfileRecord point )
  {
    final Double bewuchs = point.getBewuchsDp();
    if( Objects.isNotNull( bewuchs ) )
      return bewuchs;

    final IVegetationClass clazz = WspmClassifications.findVegetationClass( point );
    if( Objects.isNotNull( clazz ) )
    {
      final BigDecimal decimal = clazz.getDp();
      if( Objects.isNotNull( decimal ) )
        return decimal.doubleValue();
    }

    return null;
  }

  private Double getAy( final IProfileRecord point )
  {
    final Double bewuchs = point.getBewuchsDp();
    if( Objects.isNotNull( bewuchs ) )
      return bewuchs;

    final IVegetationClass clazz = WspmClassifications.findVegetationClass( point );
    if( Objects.isNotNull( clazz ) )
    {
      final BigDecimal decimal = clazz.getAy();
      if( Objects.isNotNull( decimal ) )
        return decimal.doubleValue();
    }

    return null;
  }

  private Double getAx( final IProfileRecord point )
  {
    final Double bewuchs = point.getBewuchsDp();
    if( Objects.isNotNull( bewuchs ) )
      return bewuchs;

    final IVegetationClass clazz = WspmClassifications.findVegetationClass( point );
    if( Objects.isNotNull( clazz ) )
    {
      final BigDecimal decimal = clazz.getAx();
      if( Objects.isNotNull( decimal ) )
        return decimal.doubleValue();
    }

    return null;
  }

  @Override
  public IProfilView createLayerPanel( )
  {
    return new VegetationPanel( getProfil() );
  }

}
