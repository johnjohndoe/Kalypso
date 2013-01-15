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
package org.kalypso.model.wspm.tuhh.ui.chart.layers;

import java.math.BigDecimal;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.gml.classifications.helper.WspmClassifications;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.changes.PointPropertyRemove;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilePointsLayer;
import org.kalypso.observation.result.IComponent;

import de.openali.odysseus.chart.framework.model.data.DataRange;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.figure.impl.FullRectangleFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author kimwerner
 */
public class RoughnessLayer extends AbstractProfilePointsLayer
{
//  private IPointStyle m_styleClazzes;

  public RoughnessLayer( final String id, final IProfile profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider )
  {
    super( id, profil, targetRangeProperty, styleProvider );
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    return null;
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final IProfile profil = getProfil();
    if( profil == null )
      return;

    if( !hasRoughnessProperties() )
      return;

    // final int baseLine = chartImageInfo.getLayerRect().y + chartImageInfo.getLayerRect().height;
    final FullRectangleFigure fr = new FullRectangleFigure( getAreaStyle() );

//    final IAxis dom = getDomainAxis();
//    final IAxis tar = getTargetAxis();

    final IProfileRecord[] points = profil.getPoints();

    for( final IProfileRecord point : points )
    {
      final IProfileRecord next = point.getNextPoint();
      if( Objects.isNull( next ) )
        continue;

      final Double px1 = point.getBreite();
      final BigDecimal py1 = getValue( point );

      final Double px2 = next.getBreite();
      if( Objects.isNull( px1, py1, px2 ) )
        continue;
//
//      final int x1 = dom.logicalToScreen( px1 );
//      final int y1 = tar.logicalToScreen( py1 );
//      final int x2 = dom.logicalToScreen( px2 );
      final ICoordinateMapper< ? , ? > mapper = getCoordinateMapper();
      final Point bottomRightCorner = mapper.normalizedToScreen( 1.0, 0.0 );
      final Point topLeft = mapper.numericToScreen( px1, py1.doubleValue() );
      final Point topRight = mapper.numericToScreen(px2, py1.doubleValue() );

      fr.setRectangle( new Rectangle( topLeft.x, topLeft.y, Math.abs( topLeft.x - topRight.x ), Math.abs( bottomRightCorner.y - topRight.y ) ) );
      // fr.setRectangle( new Rectangle( x1, y1, Math.abs( x2 - x1 ), Math.abs( baseLine - y1 ) ) );
      fr.paint( gc );
    }
  }

  private boolean hasRoughnessProperties( )
  {
    final IProfile profil = getProfil();
    final IComponent property = profil.getPointPropertyFor( getTargetProperty() );
    if( Objects.isNull( property ) )
    {
      // nonsense...
//      if( Objects.isNull( profil.getProperty( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS ) ) )
      return false;
    }

    return true;
  }

  // FIXME: not used!
//  @Override
//  protected IPointStyle getPointStyle( )
//  {
//    if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( getTargetProperty() ) )
//    {
//      if( Objects.isNotNull( m_styleClazzes ) )
//        return m_styleClazzes;
//
//      m_styleClazzes = StyleUtils.getDefaultPointStyle();
//      m_styleClazzes.getStroke().setColor( new RGB( 0, 0, 0 ) );
//      m_styleClazzes.setInlineColor( new RGB( 137, 62, 16 ) );
//      m_styleClazzes.setAlpha( 40 );
//
//      return m_styleClazzes;
//    }
//
//    return super.getPointStyle();
//  }

  private BigDecimal getValue( final IProfileRecord point )
  {
    /**
     * TODO: 3 RoughnessLayer are initiated by default: ks, kst and roughness class layers. if a profile defines both ks
     * and kst values so we must show booth roughness class values, too. at the moment only roughness class ks values
     * will be shown! -> TODO: show two layers
     */
    /**
     * TODO 2: like calculation core, displaying / handling of roughness is configruated by a flag (use roughness
     * classes, use plain values)
     */

    final boolean preferClasses = false;

    return WspmClassifications.getRoughnessValue( point, getTargetProperty(), preferClasses );
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange domainIntervall )
  {
    final IProfile profil = getProfil();
    if( Objects.isNull( profil ) )
      return null;

    Double min = Double.MAX_VALUE;
    Double max = -Double.MAX_VALUE;
    final IProfileRecord[] points = profil.getPoints();
    for( final IProfileRecord point : points )
    {
      final BigDecimal value = getValue( point );
      if( Objects.isNotNull( value ) )
      {
        min = Math.min( min, value.doubleValue() );
        max = Math.max( max, value.doubleValue() );
      }
    }

    if( min.doubleValue() == Double.MAX_VALUE || max.doubleValue() == -Double.MAX_VALUE )
      return null;

    return new DataRange<>( min, max );
  }

  @Override
  public void removeYourself( )
  {
    final IProfile profil = getProfil();

    final ProfileOperation operation = new ProfileOperation( Messages.getString( "org.kalypso.model.wspm.tuhh.ui.chart.RoughnessLayer.0" ), getProfil(), true ); //$NON-NLS-1$
    operation.addChange( new PointPropertyRemove( profil, getTargetComponent() ) );
    new ProfileOperationJob( operation ).schedule();
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final LegendEntry le = new LegendEntry( this, toString() )
    {
      @Override
      public void paintSymbol( final GC gc, final Point size )
      {
        final Rectangle clipping = gc.getClipping();
        final FullRectangleFigure figure = new FullRectangleFigure( getAreaStyle() );

        final int left = clipping.x + clipping.width * 1 / 3;
        final int right = clipping.x + clipping.width * 2 / 3;
        final int bottom = clipping.y + clipping.height * 1 / 5;
        final int top = clipping.y + clipping.height;

        final Rectangle rect = new Rectangle( left, top, right - left, bottom - top );
        figure.setRectangle( rect );
        figure.paint( gc );
      }
    };

    return new ILegendEntry[] { le };
  }
}