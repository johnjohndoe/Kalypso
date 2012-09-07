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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay;

import java.awt.geom.Point2D;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.IProfileData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.editdata.ISegmentData;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.profil.visitors.ProfileVisitors;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Point;

import de.openali.odysseus.chart.framework.model.event.ILayerManagerEventListener.ContentChangeType;
import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.MarkerFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;
import de.openali.odysseus.chart.framework.util.img.ChartImageInfo;

/**
 * @author kimwerner, Thomas Jung
 */
public class ProfilOverlayLayer extends PointsLineLayer
{
//  CreateMainChannelWidget m_widget = null;

  public static String LAYER_OVERLAY = "org.kalypso.model.wspm.tuhh.ui.chart.overlay.LAYER_OVERLAY"; //$NON-NLS-1$

  public static String LAYER_ID = "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay"; //$NON-NLS-1$

  private CreateChannelData m_data;

  final private RGB COLOR_ALLOW = new RGB( 0, 255, 0 );

  final private RGB COLOR_DENIE = new RGB( 255, 0, 0 );

  final private RGB COLOR_BLANK = new RGB( 255, 255, 255 );

  final private RGB COLOR_LINE = new RGB( 0, 153, 255 );

  /**
   * manages the displaying of the intersected profile layer in the profile chart view and handles the user
   * interactivity.
   */
  public ProfilOverlayLayer( final IProfil profil, final ILayerStyleProvider styleProvider )
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
  public EditInfo drag( final Point curserPos, final EditInfo dragStartData )
  {
    /**
     * get Screen and logical Points
     */
    final int left = curserPos.x;
    final int top = getTargetAxis().getScreenHeight() - 10;

    final Point2D curserPoint = toNumeric( curserPos );

    final IProfil profile = m_data.getActiveProfile().getProfilOrg();
    final IProfileRecord profilePoint = ProfileVisitors.findNearestPoint( profile, curserPoint.getX() );
    final IProfileRecord fePoint = ProfileVisitors.findNearestPoint( getProfil(), curserPoint.getX() );

    final Point profilePointScreen = toScreen( profilePoint );
    final Point fePointScreen = toScreen( fePoint );

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
      final Point spScreen = toScreen( snapPoint );
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
    final Integer index = (Integer) dragStartData.getData();

    final IProfil profil = getProfil();
    final IProfileData activeProfile = m_data.getActiveProfile();
    final IProfil origProfil = activeProfile.getProfilOrg();

    final IRecord targetPoint = profil.getPoint( index );

    /**
     * get Screen and logical Points
     */
    final Point2D curserPoint = toNumeric( point );
    if( curserPoint == null )
      return;

    final IProfileRecord profilePoint = ProfileVisitors.findNearestPoint( origProfil, curserPoint.getX() );
    final IProfileRecord fePoint = ProfileVisitors.findNearestPoint( profil, curserPoint.getX() );
    final Point profilePointScreen = toScreen( profilePoint );
    final Point fePointScreen = toScreen( fePoint );

    /**
     * break, no Changes
     */
    if( Math.abs( point.x - fePointScreen.x ) < 5 )
    {
      // force repaint
      getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
      return;
    }
    /**
     * snap Point
     */
    Double width = curserPoint.getX();

    // check if there is a point in snap distance
    if( Math.abs( point.x - profilePointScreen.x ) < 5 )
    {
      // Here we have to get the real width of the original profile point otherwise we have a rounding problem by
      width = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, profilePoint );
    }

    // check if width is less than the first profile point
    final double widthFirstProfilePoint = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, origProfil.getPoint( 0 ) );
    if( width < widthFirstProfilePoint )
      return;

    // check if width is greater than the last profile point
    final double widthLastProfilePoint = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, origProfil.getPoint( origProfil.getPoints().length - 1 ) );
    if( width > widthLastProfilePoint )
      return;

    /* set the initial height to the profile height */
    /* and get the geo coordinates for the moved profile point */
    double heigth = 0;
    GM_Point gmPoint = null;
    GM_Point geoPoint = null;
    try
    {
      heigth = WspmProfileHelper.getHeightByWidth( width, origProfil );
      gmPoint = WspmProfileHelper.getGeoPositionKalypso( width, origProfil );
      if( gmPoint == null )
        return;

      // FIXME: check if srsname is null. This would lead in an drawing error
      final String srsName = origProfil.getSrsName();
      geoPoint = WspmGeometryUtilities.pointFromPoint( gmPoint, srsName );
      // geoPoint = WspmGeometryUtilities.pointFromRwHw( gmPoint.getX(), gmPoint.getY(), gmPoint.getZ() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    final IRecord[] points = profil.getPoints();

    /* save the first and last widths of the intersected profile for comparing them later with the new widths */
    final double oldStartWdith = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[0] );
    final double oldEndWdith = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, points[points.length - 1] );

    // /* set the new profile */
    final IProfil newProfile = createNewProfile( targetPoint, width, heigth, geoPoint );
    final IComponent breiteComponent = newProfile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    if( breiteComponent != null )
      newProfile.getResult().setSortComponents( new IComponent[] { breiteComponent } );
    setProfil( newProfile );

    // TODO: not so nice, use the same profile object for both segments, so that changes goes to both segments
    // directly
    // updateProfileForNeighbourSegment();

    // TODO: both adjacent segments must be updated
    final ISegmentData upstreamSegment = activeProfile.getUpSegment();
    final ISegmentData downstreamSegment = activeProfile.getDownSegment();

    /* check if the first or last intersection point has been moved -> update intersected profile and bank linestrings */
    checkIntersectionPoints( upstreamSegment, downstreamSegment, oldStartWdith, oldEndWdith );

    // final SegmentData currentSegment = m_data.getSelectedSegment();
    // currentSegment.updateProfileIntersection();

    // FIXME
    // if( upstreamSegment != null )
    // upstreamSegment.updateProfileIntersection();
    // if( downstreamSegment != null )
    // downstreamSegment.updateProfileIntersection();

    // final CreateChannelData.PROF prof = m_data.getCurrentProfile();
    // FIXME
    // m_data.completationCheck();

    // CreateChannelData.PROF prof = m_data.getCurrentProfile();
    // setProfil( currentSegment.getProfUpIntersProfile() );

    if( breiteComponent != null )
      getProfil().getResult().setSortComponents( new IComponent[] { breiteComponent } );

    getEventHandler().fireLayerContentChanged( this, ContentChangeType.value );
  }

  @Override
  public final String getTooltipInfo( final IProfileRecord point )
  {
    final IProfil profil = getProfil();
    final IComponent HW = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent RW = profil.hasPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    if( HW == null || RW == null )
      return ""; //$NON-NLS-1$
    try
    {

      return String.format( TOOLTIP_FORMAT, new Object[] { RW.getName(), ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, point ), HW.getName(),
          ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, point ), ComponentUtilities.getComponentUnitLabel( HW ) } );
    }
    catch( final RuntimeException e )
    {
      return e.getLocalizedMessage();
    }
  }

  @Override
  public String getIdentifier( )
  {
    return LAYER_OVERLAY;
  }

  @Override
  public void paint( final GC gc, final ChartImageInfo chartImageInfo, final IProgressMonitor monitor )
  {
    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IProfileRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length;
    final Point[] points = new Point[len];

    for( int i = 0; i < len; i++ )
      points[i] = toScreen( profilPoints[i] );

    final PolylineFigure pl = new PolylineFigure();
    pl.setStyle( getLineStyle() );
    pl.setPoints( points );
    pl.paint( gc );

    final MarkerFigure figure = new MarkerFigure( getPointStyle() );
    for( final Point point : points )
    {
      figure.setCenterPoint( point.x, point.y );
      figure.paint( gc );
    }

    // final PointFigure pfl = new PointFigure();
    // pfl.setStyle( getPointStyle() );
    // pfl.setPoints( points );
    // pfl.paint( gc );
  }

  public void setProfile( final IProfil profile, final CreateChannelData data )
  {
    super.setProfil( profile );

    m_data = data;

    if( profile != null )
    {
      final IComponent breiteComponent = profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      if( breiteComponent != null )
        profile.getResult().setSortComponents( new IComponent[] { breiteComponent } );
    }
  }

  @Override
  public String getTitle( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.ProfilOverlayLayer.1" ); //$NON-NLS-1$
  }

  // FIXME: use ChannelEditUtil
  private IProfil createNewProfile( final IRecord profilePoint, final double width, final double heigth, final GM_Point geoPoint )
  {
    final IProfil profil = getProfil();
    /* set the new values for the moved profile point */
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE ), width );
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), geoPoint.getX() );
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ), geoPoint.getY() );

    /* sort profile points by width */
    final IProfileRecord[] points = profil.getPoints();

    // TODO: save the sorted points as new m_profile
    final IProfil tmpProfil = ProfilFactory.createProfil( profil.getType() );

    /* get / create components */
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profil.getType() );

    final IComponent breiteComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent rwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    tmpProfil.addPointProperty( breiteComponent );
    tmpProfil.addPointProperty( hoeheComponent );
    tmpProfil.addPointProperty( rwComponent );
    tmpProfil.addPointProperty( hwComponent );
    final int iBreite = tmpProfil.indexOfProperty( breiteComponent );
    final int iHoehe = tmpProfil.indexOfProperty( hoeheComponent );
    final int iRW = tmpProfil.indexOfProperty( rwComponent );
    final int iHW = tmpProfil.indexOfProperty( hwComponent );

    for( final IProfileRecord element : points )
    {
      final IProfileRecord profilPoint = tmpProfil.createProfilPoint();

      final double breite = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, element );
      final double hoehe = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, element );
      final double rw = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, element );
      final double hw = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, element );

      profilPoint.setValue( iBreite, breite );
      profilPoint.setValue( iHoehe, hoehe );
      profilPoint.setValue( iRW, rw );
      profilPoint.setValue( iHW, hw );

      tmpProfil.addPoint( profilPoint );
    }
    /* station */
    tmpProfil.setStation( profil.getStation() );

    /* coordinate system */
    tmpProfil.setSrsName( profil.getSrsName() );

    return tmpProfil;
  }

  /**
   * checks if the intersection points have been moved and updates them in the data class (also for the neighbour
   * segments).
   *
   * @param oldStartWdith
   *          the width coordinate of the first intersection profile point before user interaction
   * @param oldEndWdith
   *          the width coordinate of the last intersection profile point before user interaction
   */
  private void checkIntersectionPoints( final ISegmentData upstreamSegment, final ISegmentData downstreamSegment, final double oldStartWdith, final double oldEndWdith )
  {
    try
    {
      final IProfil profil = getProfil();
      final IRecord firstPoint = profil.getPoints()[0];
      final IRecord lastPoint = profil.getPoints()[profil.getPoints().length - 1];

      final double newStartWidth = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, firstPoint );
      final double newEndWidth = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, lastPoint );

      /* check, what intersection points have changed */
      final IProfileData activeProfile = m_data.getActiveProfile();
      final IProfil activeDataProfile = activeProfile.getProfilOrg();

      final int iHoehe = activeDataProfile.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );

      final GM_Point startPoint = WspmProfileHelper.getGeoPositionKalypso( newStartWidth, activeDataProfile );
      final GM_Point endPoint = WspmProfileHelper.getGeoPositionKalypso( newEndWidth, activeDataProfile );

      if( oldStartWdith != newStartWidth ) // first intersection point has been moved
      {
        /* intersection points have to have the same heigth as the orig profile */
        firstPoint.setValue( iHoehe, startPoint.getZ() );

        // FIXME
        // upstreamSegment.setIntersPoint( startPoint, PROF.DOWN, WIDTHORDER.FIRST, newStartWidth );
        // downstreamSegment.setIntersPoint( startPoint, PROF.UP, WIDTHORDER.FIRST, newStartWidth );
      }

      if( oldEndWdith != newEndWidth )
      {
        /* intersection points have to have the same heigth as the orig profile */
        lastPoint.setValue( iHoehe, endPoint.getZ() );

        // FIXME
        // upstreamSegment.setIntersPoint( endPoint, PROF.DOWN, WIDTHORDER.LAST, newEndWidth );
        // downstreamSegment.setIntersPoint( endPoint, PROF.UP, WIDTHORDER.LAST, newEndWidth );
      }

      if( oldStartWdith != newStartWidth || oldEndWdith != newEndWidth )
      {
        // FIXME
        // upstreamSegment.setNewIntersectedProfile( profil, PROF.DOWN );
        // downstreamSegment.setNewIntersectedProfile( profil, PROF.UP );
      }
    }
    catch( final IndexOutOfBoundsException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }
}
