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
import java.util.List;

import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.WIDTHORDER;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.SegmentData;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.PointsLineLayer;
import org.kalypso.observation.result.ComponentUtilities;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Point;

import de.openali.odysseus.chart.framework.model.figure.IPaintable;
import de.openali.odysseus.chart.framework.model.figure.impl.PointFigure;
import de.openali.odysseus.chart.framework.model.figure.impl.PolylineFigure;
import de.openali.odysseus.chart.framework.model.layer.EditInfo;
import de.openali.odysseus.chart.framework.model.style.ILineStyle;

/**
 * @author kimwerner, Thomas Jung
 */
public class ProfilOverlayLayer extends PointsLineLayer
{

  CreateMainChannelWidget m_widget = null;

  public static String LAYER_ID = "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay";

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
    getLineStyle_hover().setColor( COLOR_LINE );
    getLineStyle_hover().setAlpha( getLineStyle_hover().getAlpha() * 4 );

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.PointsLineLayer#drag(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public EditInfo drag( final Point curserPos, final EditInfo dragStartData )
  {
    /**
     * get Screen and logical Points
     */
    final int left = curserPos.x;
    final int top = getTargetAxis().getScreenHeight() - 10;

    final Point2D curserPoint = toNumeric( curserPos );

    final IRecord profilePoint = ProfilUtil.findNearestPoint( m_data.getProfil(), curserPoint.getX() );
    final IRecord fePoint = ProfilUtil.findNearestPoint( getProfil(), curserPoint.getX() );

    final Point profilePointScreen = toScreen( profilePoint );
    final Point fePointScreen = toScreen( fePoint );

    /**
     * set LineStyles
     */
    final PolylineFigure snapped = new PolylineFigure();
    final ILineStyle ls = getLineStyle_active();
    snapped.setStyle( ls );
    final PolylineFigure lineFigure_move = new PolylineFigure();
    lineFigure_move.setStyle( getLineStyle_hover() );

    /**
     * snap Point
     */
    IRecord snapPoint = null;
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

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.PointsLineLayer#executeDrop(org.eclipse.swt.graphics.Point,
   *      de.openali.odysseus.chart.framework.model.layer.EditInfo)
   */
  @Override
  public void executeDrop( final Point point, final EditInfo dragStartData )
  {
    final Integer index = (Integer) dragStartData.getData();
    final IProfil profil = getProfil();
    final IProfil origProfil = m_data.getProfil();
    final IRecord targetPoint = profil.getPoint( index );

    /**
     * get Screen and logical Points
     */
    final Point2D curserPoint = toNumeric( point );
    if( curserPoint == null )
      return;

    final IRecord profilePoint = ProfilUtil.findNearestPoint( origProfil, curserPoint.getX() );
    final IRecord fePoint = ProfilUtil.findNearestPoint( profil, curserPoint.getX() );
    final Point profilePointScreen = toScreen( profilePoint );
    final Point fePointScreen = toScreen( fePoint );

    /**
     * break, no Changes
     */
    if( Math.abs( point.x - fePointScreen.x ) < 5 )
    {
      // force repaint
      getEventHandler().fireLayerContentChanged( this );
      return;
    }
    /**
     * snap Point
     */
    Double width = curserPoint.getX();
    if( Math.abs( point.x - profilePointScreen.x ) < 5 )
    {
      width = toNumeric( profilePointScreen ).getX();
    }

    // /* set the initial height to the profile height */
    // /* and get the geo coordinates for the moved profile point */
    double heigth = 0;
    GM_Point gmPoint = null;
    GM_Point geoPoint = null;
    try
    {
      heigth = WspmProfileHelper.getHeightByWidth( width, origProfil );
      gmPoint = WspmProfileHelper.getGeoPosition( width, origProfil );
      final String srsName = (String) profil.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
      geoPoint = WspmGeometryUtilities.pointFromPoint( gmPoint, srsName );
      geoPoint = WspmGeometryUtilities.pointFromRwHw( gmPoint.getX(), gmPoint.getY(), gmPoint.getZ() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    final IRecord[] points = profil.getPoints();
    // final int breiteIndexPoint = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );

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
    updateProfileForNeighbourSegment();

    // /* check if the first or last intersection point has been moved -> update intersected profile and bank
    // linestrings
    // */
    try
    {
      checkIntersectionPoints( oldStartWdith, oldEndWdith );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    final SegmentData currentSegment = m_data.getSelectedSegment();
    currentSegment.updateProfileIntersection();
    final CreateChannelData.PROF prof = m_data.getCurrentProfile();
    m_data.completationCheck();

    // CreateChannelData.PROF prof = m_data.getCurrentProfile();
    if( prof == CreateChannelData.PROF.UP )
      setProfil( currentSegment.getProfUpIntersProfile() );
    else
      setProfil( currentSegment.getProfDownIntersProfile() );

    if( breiteComponent != null )
      getProfil().getResult().setSortComponents( new IComponent[] { breiteComponent } );
    getEventHandler().fireLayerContentChanged( this );
  }

  @Override
  public final String getTooltipInfo( final IRecord point )
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

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getId()
   */
  @Override
  public String getIdentifier( )
  {
    return IWspmOverlayConstants.LAYER_OVERLAY;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.PointsLineLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( final GC gc )
  {
    final IProfil profil = getProfil();

    if( profil == null )
      return;
    final IRecord[] profilPoints = profil.getPoints();
    final int len = profilPoints.length;
    final Point[] points = new Point[len];

    for( int i = 0; i < len; i++ )
      points[i] = toScreen( profilPoints[i] );

    final PolylineFigure pl = new PolylineFigure();
    pl.setStyle( getLineStyle() );
    pl.setPoints( points );
    pl.paint( gc );

    final PointFigure pfl = new PointFigure();
    pfl.setStyle( getPointStyle() );
    pfl.setPoints( points );
    pfl.paint( gc );
  }

  public void setProfile( final IProfil profile, final CreateChannelData data, final CreateMainChannelWidget widget )
  {
    super.setProfil( profile );
    m_data = data;
    m_widget = widget;

    if( profile != null )
    {
      final IComponent breiteComponent = profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      if( breiteComponent != null )
        profile.getResult().setSortComponents( new IComponent[] { breiteComponent } );
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#getTitle()
   */
  @Override
  public String getTitle( )
  {
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay.ProfilOverlayLayer.1" ); //$NON-NLS-1$
  }

  private IProfil createNewProfile( final IRecord profilePoint, final double width, final double heigth, final GM_Point geoPoint )
  {
    final IProfil profil = getProfil();
    /* set the new values for the moved profile point */
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE ), width );
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), geoPoint.getX() );
    profilePoint.setValue( profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT ), geoPoint.getY() );

    /* sort profile points by width */
    final IRecord[] points = profil.getPoints();

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

    for( final IRecord element : points )
    {
      final IRecord profilPoint = tmpProfil.createProfilPoint();

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
    final String srsName = (String) profil.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
    tmpProfil.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, srsName );

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
  private void checkIntersectionPoints( final double oldStartWdith, final double oldEndWdith ) throws Exception
  {

    final IProfil profil = getProfil();
    final IRecord firstPoint = profil.getPoints()[0];
    final IRecord lastPoint = profil.getPoints()[profil.getPoints().length - 1];

    final double newStartWidth = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, firstPoint );
    final double newEndWidth = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, lastPoint );

    final CreateChannelData.PROF prof = m_data.getCurrentProfile();

    /* get the current segment to set the new intersection point for it */
    final SegmentData currentSegment = m_data.getSelectedSegment();

    /* get the neighbor segments of the current segment */
    final List<SegmentData> neighbourSegments = m_data.getNeighbourSegments( currentSegment );

    WIDTHORDER widthorder = null;
    double width = 0;
    GM_Point point = null;

    /* check, what intersection points have changed */
    final int iHoehe = m_data.getProfil().indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    if( oldStartWdith != newStartWidth && oldEndWdith != newEndWidth ) // both intersection points have been moved
    {
      // FIRST
      final WIDTHORDER widthorder1 = CreateChannelData.WIDTHORDER.FIRST;
      final double width1 = newStartWidth;
      final GM_Point point1 = WspmProfileHelper.getGeoPosition( width1, m_data.getProfil() );

      /* update the first intersection point */
      currentSegment.setIntersPoint( point1, prof, widthorder1, width1 );
      /* intersection points have to have the same heigth as the orig profile */

      firstPoint.setValue( iHoehe, point1.getZ() );

      // LAST
      final WIDTHORDER widthorder2 = CreateChannelData.WIDTHORDER.LAST;
      final double width2 = newEndWidth;
      final GM_Point point2 = WspmProfileHelper.getGeoPosition( width2, m_data.getProfil() );

      /* update the last intersection point */
      currentSegment.setIntersPoint( point2, prof, widthorder2, width2 );
      /* intersection points have to have the same heigth as the orig profile */
      lastPoint.setValue( iHoehe, point2.getZ() );

      /* update the intersected profile */
      currentSegment.setNewIntersectedProfile( profil, prof );

      // TODO: we should also update the neighbor segment of the current profile
      // currentSegment.updateProfileIntersection();

      /* update the neighbour segment */
      updateNeighbourSegment( prof, currentSegment, neighbourSegments, widthorder1, width1, point1, widthorder2, width2, point2 );

    }
    else if( oldStartWdith != newStartWidth ) // first intersection point has been moved
    {
      widthorder = CreateChannelData.WIDTHORDER.FIRST;
      width = newStartWidth;
      point = WspmProfileHelper.getGeoPosition( width, m_data.getProfil() );

      /* update the last intersection point */
      currentSegment.setIntersPoint( point, prof, widthorder, width );
      /* intersection points have to have the same heigth as the orig profile */

      firstPoint.setValue( iHoehe, point.getZ() );

      /* update the intersected profile */
      currentSegment.setNewIntersectedProfile( profil, prof );
      // currentSegment.updateProfileIntersection();

      /* update the neighbour segment */
      updateNeighbourSegment( point, prof, currentSegment, neighbourSegments, widthorder, width );
    }
    else if( oldEndWdith != newEndWidth )
    {
      widthorder = CreateChannelData.WIDTHORDER.LAST;
      width = newEndWidth;
      point = WspmProfileHelper.getGeoPosition( width, m_data.getProfil() );

      /* update the last intersection point */
      currentSegment.setIntersPoint( point, prof, widthorder, width );
      /* intersection points have to have the same heigth as the orig profile */
      lastPoint.setValue( iHoehe, point.getZ() );

      /* update the intersected profile */
      currentSegment.setNewIntersectedProfile( profil, prof );
      currentSegment.updateProfileIntersection();

      /* update the neighbour segment */
      updateNeighbourSegment( point, prof, currentSegment, neighbourSegments, widthorder, width );
    }
    else
      widthorder = null;

  }

  /**
   * updates both intersection points of the corresponding profile of the neighbouring segment.
   * 
   * @param prof
   *          profile side of the current segment (up/down)
   * @param currentSegment
   *          segment data of the current segment
   * @param neighbourSegments
   *          list of the neighbouring segments
   * @param widthorder1
   *          widthorder of the first intersection point (first/last)
   * @param width1
   *          width coordinate of the new intersection point
   * @param point1
   *          geo coords of the new intersection point
   * @param widthorder2
   *          widthorder of the second intersection point (first/last)
   * @param width2
   *          width coordinate of the new intersection point
   * @param point2
   *          geo coords of the new intersection point
   */
  private void updateNeighbourSegment( final CreateChannelData.PROF prof, final SegmentData currentSegment, final List<SegmentData> neighbourSegments, final WIDTHORDER widthorder1, final double width1, final GM_Point point1, final WIDTHORDER widthorder2, final double width2, final GM_Point point2 )
  {
    final IProfil profil = getProfil();
    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( final SegmentData segment : neighbourSegments )
    {
      // check if the changed profile is in the neighbour segment, if not, do nothing.
      if( segment != currentSegment )
      {
        if( segment.getProfilDownOrg().getStation() == profil.getStation() || segment.getProfilUpOrg().getStation() == profil.getStation() )
        {
          if( widthorder1 != null && widthorder2 != null )
          {
            try
            {
              segment.setIntersPoint( point1, profNeighbour, widthorder1, width1 );
              segment.setIntersPoint( point2, profNeighbour, widthorder2, width2 );
            }
            catch( final Exception e )
            {
              // TODO Auto-generated catch block
              e.printStackTrace();
            }
            segment.setNewIntersectedProfile( profil, profNeighbour );
            // segment.updateProfileIntersection();
          }
        }
      }
    }
  }

  /**
   * updates one intersection point of the corresponding profile of the neighbouring segment.
   * 
   * @param prof
   *          profile side of the current segment (up/down)
   * @param currentSegment
   *          segment data of the current segment
   * @param neighbourSegments
   *          list of the neighbouring segments
   * @param widthorder1
   *          widthorder of the first intersection point (first/last)
   * @param width1
   *          width coordinate of the new intersection point
   * @param point1
   *          geo coords of the new intersection point
   */
  private void updateNeighbourSegment( final GM_Point point, final CreateChannelData.PROF prof, final SegmentData currentSegment, final List<SegmentData> neighbourSegments, final WIDTHORDER widthorder, final double width )
  {
    final IProfil profil = getProfil();
    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( final SegmentData segment : neighbourSegments )
    {
      // check if the changed profile is in the neighbour segment, if not, do nothing.
      if( segment != currentSegment )
      {
        if( segment.getProfilDownOrg().getStation() == profil.getStation() || segment.getProfilUpOrg().getStation() == profil.getStation() )
        {
          if( widthorder != null )
            try
            {
              segment.setIntersPoint( point, profNeighbour, widthorder, width );
            }
            catch( final Exception e )
            {
              // TODO Auto-generated catch block
              e.printStackTrace();
            }
          segment.setNewIntersectedProfile( profil, profNeighbour );
        }
      }
    }
  }

  /**
   * updates the profile for the neighboring segment.
   */
  private void updateProfileForNeighbourSegment( )
  {
    final IProfil profil = getProfil();
    /* get the current segment to set the new profile for it */
    final SegmentData currentSegment = m_data.getSelectedSegment();

    /* get the neighbor segments of the current segment */
    final List<SegmentData> neighbourSegments = m_data.getNeighbourSegments( currentSegment );
    final CreateChannelData.PROF prof = m_data.getCurrentProfile();

    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( final SegmentData segment : neighbourSegments )
      // check if the changed profile is in the neighbor segment, if not, do nothing.
      if( segment != currentSegment )
        if( segment.getProfilDownOrg().getStation() == profil.getStation() || segment.getProfilUpOrg().getStation() == profil.getStation() )
          segment.setNewIntersectedProfile( profil, profNeighbour );
    // segment.updateProfileIntersection();
  }
}
