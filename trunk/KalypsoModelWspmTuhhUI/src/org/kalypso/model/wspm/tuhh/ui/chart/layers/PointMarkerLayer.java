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

import java.awt.geom.Point2D;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.ObjectUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.model.wspm.core.IWspmPointProperties;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfilePointMarker;
import org.kalypso.model.wspm.core.profil.IProfilePointMarkerProvider;
import org.kalypso.model.wspm.core.profil.changes.ActiveObjectEdit;
import org.kalypso.model.wspm.core.profil.changes.PointMarkerSetPoint;
import org.kalypso.model.wspm.core.profil.changes.ProfileChangeHint;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperation;
import org.kalypso.model.wspm.core.profil.operation.ProfileOperationJob;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilePointsLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilLayerUtils;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;

import com.google.common.primitives.Doubles;

import de.openali.odysseus.chart.ext.base.layer.TooltipFormatter;
import de.openali.odysseus.chart.framework.model.data.IDataRange;
import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener.ContentChangeType;
import de.openali.odysseus.chart.framework.model.figure.impl.EmptyRectangleFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.layer.ILegendEntry;
import de.openali.odysseus.chart.framework.model.layer.impl.LegendEntry;
import de.openali.odysseus.chart.framework.model.mapper.IAxis;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author kimwerner
 */
public class PointMarkerLayer extends AbstractProfilePointsLayer
{
  private final Map<Rectangle, IProfilePointMarker> m_hoverRects = new ConcurrentHashMap<>();

  private final int m_offset;

  private final boolean m_close;

  public PointMarkerLayer( final IProfile profil, final String targetRangeProperty, final ILayerStyleProvider styleProvider, final int offset, final boolean close )
  {
    super( IWspmTuhhConstants.LAYER_DEVIDER + "_" + targetRangeProperty, profil, targetRangeProperty, styleProvider ); //$NON-NLS-1$

    m_offset = offset;

    m_close = close;
  }

  @Override
  public EditInfo drag( final Point newPos, final EditInfo dragStartData )
  {
    final IProfile profil = getProfil();

    final Point2D numericPos = ProfilLayerUtils.toNumeric( getCoordinateMapper(), newPos );

    final EmptyRectangleFigure startHoverFigure = (EmptyRectangleFigure)dragStartData.getHoverFigure();
    final Rectangle startRectangle = startHoverFigure.getRectangle();

    final IProfileRecord point = ProfileVisitors.findNearestPoint( profil, numericPos.getX() );
    if( point == null )
      return null;

    final Double x = point.getBreite();
    if( !Doubles.isFinite( x ) )
      return null;

    final Point screen = getCoordinateMapper().numericToScreen( x, 0.0 );

    final Rectangle hoverRect = new Rectangle( screen.x - 5, startRectangle.y, 10, startRectangle.height );

    final EmptyRectangleFigure hoverFigure = new EmptyRectangleFigure();
    hoverFigure.setStyle( getLineStyleHover() );
    hoverFigure.setRectangle( hoverRect );

    return new EditInfo( this, null, hoverFigure, dragStartData.getData(), getTooltipInfo( point ), dragStartData.getPosition() );
  }

  @Override
  public final void executeDrop( final Point point, final EditInfo dragStartData )
  {
    final IProfilePointMarker draggedDevider = (IProfilePointMarker)dragStartData.getData();
    if( draggedDevider == null )
      return;

    final IProfile profil = getProfil();
    final Point2D numPoint = ProfilLayerUtils.toNumeric( getCoordinateMapper(), point );
    if( numPoint == null )
      return;

    final IProfileRecord profilPoint = draggedDevider.getPoint();
    final IProfileRecord newPoint = ProfileVisitors.findNearestPoint( profil, numPoint.getX() );
    if( newPoint == profilPoint )
      return;

    final IProfilePointMarker[] deviders = profil.getPointMarkerFor( profilPoint );
    final IProfilePointMarker[] targetDeviders = profil.getPointMarkerFor( newPoint );

    for( final IProfilePointMarker devider : deviders )
    {
      // BUGIFX: prohibit that a marker is moved on another marker of the same type, which
      // will incidentally remove it
      if( movesOnSameDeviderType( devider, targetDeviders ) )
      {
        continue;
      }

      if( devider.getComponent().getId().equals( getTargetComponent().getId() ) )
      {
        moveDevider( devider, newPoint );
      }
    }
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    for( final Map.Entry<Rectangle, IProfilePointMarker> entry : m_hoverRects.entrySet() )
    {
      final Rectangle hoverRect = entry.getKey();
      final IProfilePointMarker devider = entry.getValue();

      if( hoverRect.contains( pos ) )
      {
        final EmptyRectangleFigure hoverFigure = new EmptyRectangleFigure();
        hoverFigure.setStyle( getLineStyleHover() );
        hoverFigure.setRectangle( hoverRect );

        final IProfileRecord point = devider.getPoint();

        return new EditInfo( this, hoverFigure, null, devider, getTooltipInfo( point ), pos );
      }
    }

    return null;
  }

  @Override
  public synchronized ILegendEntry[] getLegendEntries( )
  {
    final IProfilePointMarkerProvider markerProvider = KalypsoModelWspmCoreExtensions.getMarkerProviders( getProfil().getType() );

    final LegendEntry le = new LegendEntry( this, getTitle() )
    {
      @Override
      public void paintSymbol( final GC gc, final Point size )
      {
        final IComponent cmp = getTargetComponent();
        if( cmp != null )
        {
          markerProvider.drawMarker( new String[] { cmp.getId() }, gc );
        }
      }
    };

    return new ILegendEntry[] { le };
  }

  @Override
  public IDataRange<Double> getTargetRange( final IDataRange<Double> domainIntervall )
  {
    return null;
  }

  @Override
  protected String getTooltipInfo( final IProfileRecord point )
  {
    final Double width = point.getBreite();

    try
    {
      final IComponent domainComponent = getDomainComponent();
      final IComponent targetComponent = getTargetComponent();

      final String header = targetComponent.getName();

      final TooltipFormatter formatter = new TooltipFormatter( header, new String[] { "%s", "%,.2f", "%s" }, new int[] { SWT.LEFT, SWT.RIGHT, SWT.LEFT } ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      final String domainUnit = ComponentUtilities.getComponentUnitLabel( domainComponent ); //$NON-NLS-1$
      formatter.addLine( domainComponent.getName(), width, domainUnit );

      return formatter.format();
    }
    catch( final RuntimeException e )
    {
      return e.getLocalizedMessage();
    }
  }

  protected void moveDevider( final IProfilePointMarker devider, final IProfileRecord newPoint )
  {
    final IProfile profil = getProfil();

    final ProfileOperation operation = new ProfileOperation( "", profil, true ); //$NON-NLS-1$
    operation.addChange( new PointMarkerSetPoint( devider, newPoint ) );
    operation.addChange( new ActiveObjectEdit( profil, newPoint.getBreiteAsRange(), null ) );
    new ProfileOperationJob( operation ).schedule();
  }

  private boolean movesOnSameDeviderType( final IProfilePointMarker devider, final IProfilePointMarker[] targetDeviders )
  {
    final String id = devider.getComponent().getId();
    for( final IProfilePointMarker marker : targetDeviders )
    {
      final String targetId = marker.getComponent().getId();
      if( ObjectUtils.equals( id, targetId ) )
        return true;
    }

    return false;
  }

  @Override
  public void onProfilChanged( final ProfileChangeHint hint )
  {
    if( hint.isPointPropertiesChanged() || hint.isMarkerMoved() || hint.isMarkerDataChanged() || hint.isProfilPropertyChanged() )
    {
      getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
    }
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final IProfile profil = getProfil();
    final IComponent target = getTargetComponent();

    m_hoverRects.clear();

    if( profil == null || target == null )
      return;

    final IProfilePointMarker[] deviders = profil.getPointMarkerFor( target.getId() );

    /* calculate top and bottom */
    final int screenBottom = chartImageInfo.getLayerRect().y + chartImageInfo.getLayerRect().height;
    final int screenTop = chartImageInfo.getLayerRect().y + m_offset;
    final int top = screenTop + m_offset;

    final IAxis domainAxis = getDomainAxis();

    final PolylineFigure pf = new PolylineFigure();
    pf.setStyle( getLineStyle() );

    for( final IProfilePointMarker devider : deviders )
    {
      final Double breite = ProfileUtil.getDoubleValueFor( IWspmPointProperties.POINT_PROPERTY_BREITE, devider.getPoint() );
      final int screenX = domainAxis.numericToScreen( breite );
      final Point p1 = new Point( screenX, screenBottom );
      final Point p2 = new Point( screenX, top );

      pf.setPoints( new Point[] { p1, p2 } );
      pf.paint( gc );

      /* remember hover rect for get hvoer */
      final Rectangle hoverRect = new Rectangle( p1.x - 5, top - 2, 10, screenBottom - top + 4 );
      m_hoverRects.put( hoverRect, devider );
    }

    if( m_close && deviders.length > 1 )
    {
      final int screenX1 = domainAxis.numericToScreen( ProfileUtil.getDoubleValueFor( getDomainComponent().getId(), deviders[0].getPoint() ) );
      final int screenX2 = domainAxis.numericToScreen( ProfileUtil.getDoubleValueFor( getDomainComponent().getId(), deviders[deviders.length - 1].getPoint() ) );

      pf.setPoints( new Point[] { new Point( screenX1, top ), new Point( screenX2, top ) } );
      pf.paint( gc );
    }
  }
}