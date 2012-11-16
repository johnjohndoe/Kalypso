/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.geom.Point2D;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.util.ProfileUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilLayerUtils;
import org.kalypso.model.wspm.ui.view.chart.ProfilePointHover;
import org.kalypso.transformation.transformer.GeoTransformerException;

import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener.ContentChangeType;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.mapper.ICoordinateMapper;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author kimwerner, Thomas Jung
 */
class ProfilOverlayLayer extends PointsLineLayer
{
  static String LAYER_OVERLAY = "org.kalypso.model.wspm.tuhh.ui.chart.overlay.LAYER_OVERLAY"; //$NON-NLS-1$

  static String LAYER_ID = "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay"; //$NON-NLS-1$

  private ChannelEditData m_data;

  final private RGB COLOR_ALLOW = new RGB( 0, 255, 0 );

  final private RGB COLOR_DENIE = new RGB( 255, 0, 0 );

  final private RGB COLOR_BLANK = new RGB( 255, 255, 255 );

  final private RGB COLOR_LINE = new RGB( 0, 153, 255 );

  private IProfile m_profile;

  /**
   * manages the displaying of the intersected profile layer in the profile chart view and handles the user
   * interactivity.
   */
  public ProfilOverlayLayer( final IProfile profil, final ILayerStyleProvider styleProvider )
  {
    super( LAYER_ID, profil, IWspmConstants.POINT_PROPERTY_HOEHE, styleProvider );

    final RGB rgb = new RGB( 0, 153, 255 );
    getLineStyle().setColor( rgb );
    getPointStyle().setInlineColor( COLOR_BLANK );
    getPointStyle().getStroke().setColor( COLOR_LINE );
    getLineStyleHover().setColor( COLOR_LINE );
    getLineStyleHover().setAlpha( getLineStyleHover().getAlpha() * 4 );
  }

  @Override
  public IProfile getProfil( )
  {
    return m_profile;
  }

  @Override
  public EditInfo getHover( final Point pos )
  {
    /* never return null, in order to suppress hover of other layers */
    final EditInfo nilInfo = new EditInfo( null, null, null, null, null, pos );

    if( !isVisible() )
      return nilInfo;

    final ProfilePointHover helper = new ProfilePointHover( this, getPointStyleHover() );
    final EditInfo pointHover = helper.getHover( pos );
    if( pointHover != null )
      return pointHover;

    /* never return null, in order to suppress hover of other layers */
    return nilInfo;
  }

  @Override
  public EditInfo drag( final Point curserPos, final EditInfo dragStartData )
  {
    if( dragStartData == null || dragStartData.getData() == null )
      return null;

    /**
     * get Screen and logical Points
     */
    final int left = curserPos.x;
    final int top = getTargetAxis().getScreenHeight() - 10;

    final Point2D curserPoint = ProfilLayerUtils.toNumeric( getCoordinateMapper(), curserPos );

    final IProfile profile = m_data.getActiveProfile().getOriginalProfile();
    final IProfileRecord profilePoint = ProfileVisitors.findNearestPoint( profile, curserPoint.getX() );
    final IProfileRecord fePoint = ProfileVisitors.findNearestPoint( m_profile, curserPoint.getX() );

    final Point profilePointScreen = toScreenInternal( profilePoint );
    final Point fePointScreen = toScreenInternal( fePoint );

    /**
     * set LineStyles
     */
    final PolylineFigure snapped = new PolylineFigure();
    final ILineStyle ls = getLineStyleActive();
    snapped.setStyle( ls );
    final PolylineFigure lineFigure_move = new PolylineFigure();
    lineFigure_move.setStyle( getLineStyleHover() );

    /**
     * snap Point
     */
    IProfileRecord snapPoint = null;
    if( Math.abs( left - profilePointScreen.x ) < 5 )
    {
      ls.setColor( COLOR_ALLOW );
      snapPoint = profilePoint;
    }
    if( Math.abs( left - fePointScreen.x ) < 5 )
    {
      ls.setColor( COLOR_DENIE );
      snapPoint = fePoint;
    }

    /**
     * set hoverFigure
     */
    lineFigure_move.setPoints( new Point[] { new Point( left, 10 ), new Point( left, top ) } );
    IPaintable hoverFigure = lineFigure_move;

    if( snapPoint != null )
    {
      final Point spScreen = toScreenInternal( snapPoint );
      snapped.setPoints( new Point[] { new Point( spScreen.x, 10 ), new Point( spScreen.x, top ) } );
      hoverFigure = new IPaintable()
      {
        @Override
        public void paint( final GC gc )
        {
          lineFigure_move.paint( gc );
          snapped.paint( gc );
        }
      };
    }

    return new EditInfo( this, null, hoverFigure, dragStartData.getData(), getTooltipInfo( snapPoint ), curserPos );
  }

  @Override
  public void executeDrop( final Point point, final EditInfo dragStartData )
  {
    if( dragStartData == null || dragStartData.getData() == null )
      return;

    final Integer index = (Integer)dragStartData.getData();

    final IProfileData activeProfile = m_data.getActiveProfile();
    final IProfile origProfil = activeProfile.getOriginalProfile();
    final IProfile segmentedProfile = activeProfile.getWorkingProfile();

    /* data and my state should be the same, else something is wrong */
    if( m_profile == null || m_profile != segmentedProfile )
      return;

    /* check if destination point is at least 5px away from source record, else do nothing */
    final IProfileRecord draggedRecord = segmentedProfile.getPoint( index );
    final int draggedScreenX = toScreenInternal( draggedRecord ).x;
    if( Math.abs( point.x - draggedScreenX ) < 5 )
    {
      // force repaint
      getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
      return;
    }

    /* calculate destination width */
    final Point2D numericPoint = ProfilLayerUtils.toNumeric( getCoordinateMapper(), point );
    if( numericPoint == null )
      return;

    final double destinationWidth = calculateDestinationWidth( origProfil, point.x, numericPoint.getX() );

    try
    {
      /* move the record */
      final ProfileOverlayMovePointOperation worker = new ProfileOverlayMovePointOperation( origProfil, segmentedProfile );
      final IProfile newSegmentedProfile = worker.moveRecord( draggedRecord, destinationWidth );

      if( newSegmentedProfile != null )
      {
        activeProfile.updateWorkingProfile( newSegmentedProfile );

        // REMARK: the set segmented profile may have been further adjusted, so we need to get it from the data
        final IProfile newAdjustedSegmentedProfile = activeProfile.getWorkingProfile();

        setProfile( newAdjustedSegmentedProfile, m_data );
      }
    }
    catch( final GeoTransformerException e )
    {
      e.printStackTrace();
    }

    /* repaint chart and map */
    getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
    m_data.triggerMapRepaint();
  }

  private double calculateDestinationWidth( final IProfile origProfil, final int destinationScreenX, final double unsnappedWidth )
  {
    /* snap to a record of the original profile near to destination */
    final IProfileRecord snappedRecord = ProfileVisitors.findNearestPoint( origProfil, unsnappedWidth );
    final Point snappedScreen = toScreenInternal( snappedRecord );

    if( snappedScreen == null || Math.abs( destinationScreenX - snappedScreen.x ) > 5 )
      return unsnappedWidth;

    // Here we have to get the real width of the original profile point otherwise we have a rounding problem by
    return snappedRecord.getBreite();
  }

  @Override
  public final String getTooltipInfo( final IProfileRecord point )
  {
    if( point == null )
      return null;

    return super.getTooltipInfo( point );

    // TODO: check if this really makes sens: show rw/hw of point, but not breite/hoehe

//    final IProfil profil = getProfil();
//    final IComponent HW = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
//    final IComponent RW = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
//
//    if( HW == null || RW == null )
//      return ""; //$NON-NLS-1$
//    try
//    {
//
//      return String.format( TOOLTIP_FORMAT, new Object[] { RW.getName(), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, point ), HW.getName(),
//          ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, point ), ComponentUtilities.getComponentUnitLabel( HW ) } );
//    }
//    catch( final RuntimeException e )
//    {
//      return e.getLocalizedMessage();
//    }
  }

  @Override
  public String getIdentifier( )
  {
    return LAYER_OVERLAY;
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final IProfile profil = m_profile;

    if( profil == null )
      return;

    final IProfileRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length;
    final Point[] points = new Point[len];

    for( int i = 0; i < len; i++ )
      points[i] = toScreenInternal( profilPoints[i] );

    final PolylineFigure pl = new PolylineFigure();
    pl.setStyle( getLineStyle() );
    pl.setPoints( points );
    pl.paint( gc );

    final PointFigure figure = new PointFigure( getPointStyle() );
    for( final Point point : points )
    {
      figure.setCenterPoint( point.x, point.y );
      figure.paint( gc );
    }
  }

  public void setProfile( final IProfile profile, final ChannelEditData data )
  {
    m_profile = profile;

    m_data = data;

    getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
  }

  @Override
  public String getTitle( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.ProfilOverlayLayer.1" ); //$NON-NLS-1$
  }

  private Point toScreenInternal( final IProfileRecord point )
  {
    final ICoordinateMapper cm = getCoordinateMapper();
    if( Objects.isNull( cm ) )
      return null;

    final Double x = ProfileUtil.getDoubleValueFor( getDomainComponent(), point );
    final Double y = ProfileUtil.getDoubleValueFor( getTargetProperty(), point );
    if( Objects.isNull( x, y ) )
      return null;
    else if( x.isNaN() || y.isNaN() )
      return null;

    return cm.numericToScreen( x, y );
  }

}
