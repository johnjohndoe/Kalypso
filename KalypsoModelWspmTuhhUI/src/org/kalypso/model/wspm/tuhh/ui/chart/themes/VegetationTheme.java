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

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.tuhh.ui.panel.vegetation.VegetationPanel;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.IProfilView;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilTheme;
import org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer;

import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener.ContentChangeType;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.IChartLayer;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author kimwerner
 */
public class VegetationTheme extends AbstractProfilTheme
{
  public static final String TITLE = Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.VegetationTheme.2" ); //$NON-NLS-1$

  private ILegendEntry[] m_legendEntries;

  private final ILineStyle m_style;

  public VegetationTheme( final IProfile profil, final IProfilChartLayer[] chartLayers, final ICoordinateMapper cm, final ILayerStyleProvider styleProvider )
  {
    super( profil, IWspmTuhhConstants.LAYER_BEWUCHS, TITLE, chartLayers, cm );

    m_style = styleProvider.getStyleFor( IWspmTuhhConstants.LAYER_BEWUCHS + "_LINE", ILineStyle.class ); //$NON-NLS-1$
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isSelectionChanged() || hint.isPointValuesChanged() )
    {
      fireLayerContentChanged( ContentChangeType.value );
    }
  }

  private ILegendEntry[] createLegendEntries( )
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

  private Rectangle getHoverRectInternal( final IProfileRecord point1, final IProfileRecord point2 )
  {
    if( Objects.isNull( point1, point2 ) )
      return null;

    if( segmenthasVegetation( point1 ) )
    {
      final Double breite1 = point1.getBreite();
      final Double hoehe1 = point1.getHoehe();

      final Double breite2 = point2.getBreite();
      final Double hoehe2 = point2.getHoehe();

      if( Objects.isNull( breite1, hoehe1, breite2, hoehe2 ) )
        return null;

      final Point p1 = new Point( getDomainAxis().numericToScreen( breite1 ), getTargetAxis().numericToScreen( hoehe1 ) - 3 );
      final Point p2 = new Point( getDomainAxis().numericToScreen( breite2 ), getTargetAxis().numericToScreen( hoehe2 ) - 3 );

      final int width = p2.x - p1.x;
      final int midX = p1.x + width / 2;
      final int height = Math.abs( p1.y - p2.y );
      final int midY = Math.min( p1.y, p2.y ) + height / 2;

      return new Rectangle( midX, midY, width, height );

    }
    return null;
  }

  @Override
  // FIXME: does not work; create edit infos during paint!
  public EditInfo getHover( final Point pos )
  {
    final IProfile profil = getProfil();
    final IProfileRecord[] points = profil.getPoints();

    for( final IProfileRecord p1 : points )
    {
      final IProfileRecord p2 = p1.getNextPoint();
      if( Objects.isNull( p2 ) )
        continue;

      final Rectangle hover = getHoverRectInternal( p1, p2 );
      if( hover == null )
        continue;

      final int size = Math.min( hover.height / 2, Math.min( hover.width / 2, 10 ) );
      if( pos.x >= hover.x - size && pos.y >= hover.y - size && pos.x < hover.x + size && pos.y < hover.y + size )
        return new EditInfo( this, null, null, ArrayUtils.indexOf( points, p1 ), getTooltipInfo( p1 ), pos );
    }

    return null;
  }

  private String getTooltipInfo( final IProfileRecord point )
  {
    return String.format( " AX: %.4f %n AY: %.4f %n DP: %.4f", WspmClassifications.getAx( point ), WspmClassifications.getAy( point ), WspmClassifications.getDp( point ) ); //$NON-NLS-1$
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

  @Override
  public void removeYourself( )
  {
    final IProfile profil = getProfil();
    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.VegetationTheme.1" ), getProfil(), true ); //$NON-NLS-1$
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AX ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_AY ) ) );
    operation.addChange( new PointPropertyRemove( profil, profil.hasPointProperty( IWspmPointProperties.POINT_PROPERTY_BEWUCHS_DP ) ) );
    new ProfileOperationJob( operation ).schedule();
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange domainIntervall )
  {
    // don't calculate axis size and ticks
    return null;
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final IProfile profil = getProfil();
    if( profil == null )
      return;

    final PolylineFigure pf = new PolylineFigure();
    pf.setStyle( m_style );

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
    m_style.apply( gc );
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
    final double ax = WspmClassifications.getAx( point );
    final double ay = WspmClassifications.getAy( point );
    final double dp = WspmClassifications.getDp( point );

    if( Doubles.isNaN( ax, ax, dp ) )
      return false;

    return ax * ay * dp != 0;

  }

  @Override
  public IProfilView createLayerPanel( )
  {
    return new VegetationPanel( getProfil() );
  }
}