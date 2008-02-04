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
import java.awt.geom.Rectangle2D;
import java.util.List;

import org.eclipse.jface.resource.ColorRegistry;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.graphics.GCWrapper;
import org.kalypso.contribs.eclipse.swt.graphics.RectangleUtils;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.SegmentData;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.WIDTHORDER;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmGeometryUtilities;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer.EditData;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypsodeegree.model.geometry.GM_Point;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner, Thomas Jung
 */
public class ProfilOverlayLayer extends AbstractProfilChartLayer
{
  private IProfil m_profile;

  CreateMainChannelWidget m_widget;

  private final Color m_color;

  private CreateChannelData m_data;

  /**
   * manages the displaying of the intersected profile layer in the profile chart view and handles the user
   * interactivity.
   */
  public ProfilOverlayLayer( final ProfilChartView chartView )
  {
    super( IWspmOverlayConstants.LAYER_OVERLAY, chartView, chartView.getDomainRange(), chartView.getValueRangeLeft(), "Zeichenfl�che" );

    final ColorRegistry cr = chartView.getColorRegistry();
    if( !cr.getKeySet().contains( IWspmOverlayConstants.LAYER_OVERLAY ) )
      cr.put( IWspmOverlayConstants.LAYER_OVERLAY, new RGB( 0, 153, 255 ) );
    m_color = cr.get( IWspmOverlayConstants.LAYER_OVERLAY );
    m_widget = null;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#alwaysAllowsEditing()
   */
  @Override
  public boolean alwaysAllowsEditing( )
  {
    return true;
  }

  protected void drawVerticalLine( final GCWrapper gc, final Point editing, final Color color, final int lineStyle, final int lineWidth )
  {
    final int lineWidthBuffer = gc.getLineWidth();
    final int lineStyleBuffer = gc.getLineStyle();
    final Color foregroundBuffer = gc.getForeground();
    gc.setLineStyle( lineStyle );
    gc.setLineWidth( lineWidth );
    gc.setForeground( color );

    final AxisRange valueRange = getValueRange();
    final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
    final int top = valueRange.getScreenTo() + valueRange.getGapSpace();
    gc.drawLine( editing.x, bottom, editing.x, top );
    gc.setLineWidth( lineWidthBuffer );
    gc.setLineStyle( lineStyleBuffer );
    gc.setForeground( foregroundBuffer );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( final Point currentScreenPoint, final Object data ) // release
  {
    final EditData editData = (EditData) data;
    IRecord[] points = m_profile.getPoints();
    final IRecord profilePoint = points[editData.getIndex()];
    final Point2D logPoint = screen2logical( currentScreenPoint );
    double width;

    /* snap behaviour */
    /* find the nearest profile point and check its distance to the current mouse pointer position */
    final double currentWidth = logPoint.getX();
    double widthDifference = Double.NaN;
    Point nearestProfScreenPoint = null;

    final IProfil origProfil = m_data.getProfilEventManager().getProfil();

    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( origProfil, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( origProfil, IWspmConstants.POINT_PROPERTY_HOEHE );

    final IRecord nearestProfPoint = ProfilUtil.findNearestPoint( origProfil, currentWidth );
    if( nearestProfPoint != null )
    {
      final double xCoord = (Double) nearestProfPoint.getValue( breiteComponent );
      final double yCoord = (Double) nearestProfPoint.getValue( hoeheComponent );
      final Point2D point2 = new Point2D.Double( xCoord, yCoord );
      nearestProfScreenPoint = logical2screen( point2 );

      /*
       * calculate width difference between the current mouse pointer position and the nearest profile point of the
       * original profile in screen coordinates
       */
      widthDifference = Math.abs( nearestProfScreenPoint.x - currentScreenPoint.x );
    }
    if( !Double.isNaN( widthDifference ) && widthDifference < 5 )
    {
      /* check, if there is already a intersection profile point at that position */
      final IRecord nearestIntersectedProfPoint = ProfilUtil.findNearestPoint( m_profile, (Double) nearestProfPoint.getValue( breiteComponent ) );
      if( (Double) nearestIntersectedProfPoint.getValue( breiteComponent ) - (Double) nearestProfPoint.getValue( breiteComponent ) == 0 )
        /* set the drawn point back to start */
        return;
      else
        /* set the new drawn width to the snapped width position */
        width = (Double) nearestProfPoint.getValue( breiteComponent );
    }
    else
    {
      /* check, if the dragged point is lying too near at another intersection point */
      final Point profScreenPointInters = getNearestProfileScreenPoint( m_profile, currentWidth );

      /*
       * calculate width difference between the current mouse pointer position and the nearest profile point of the
       * intersected profile in screen coordinates
       */
      final double widthDifferenceInters = Math.abs( profScreenPointInters.x - currentScreenPoint.x );
      if( widthDifferenceInters < 5 )
        /* set the drawn point back to start */
        return;
      else
        /* set the new drawn width to current width position */
        width = logPoint.getX();
    }

    /* check that the point is lying inside the orig. profile */
    width = checkInsideProfile( width, origProfil );

    /* set the initial height to the profile height */
    /* and get the geo coordinates for the moved profile point */
    double heigth = 0;
    GM_Point gmPoint = null;
    GM_Point geoPoint = null;
    try
    {
      heigth = WspmProfileHelper.getHeigthPositionByWidth( width, origProfil );
      gmPoint = WspmProfileHelper.getGeoPosition( width, origProfil );
      final String srsName = (String) m_profile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
      geoPoint = WspmGeometryUtilities.pointFromPoint( gmPoint, srsName );
      geoPoint = WspmGeometryUtilities.pointFromRrHw( gmPoint.getX(), gmPoint.getY(), gmPoint.getZ() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    /* save the first and last widths of the intersected profile for comparing them later with the new widths */
    final double oldStartWdith = (Double) points[0].getValue( breiteComponent );
    final double oldEndWdith = (Double) points[points.length - 1].getValue( breiteComponent );

    /* set the new profile */
    m_profile = createNewProfile( profilePoint, width, heigth, geoPoint );

    if( breiteComponent != null )
      m_profile.getResult().setSortComponents( new IComponent[] { breiteComponent } );

    // TODO: not so nice, use the same profile object for both segments, so that changes goes to both segmetns directly
    updateProfileForNeighbourSegment();

    /* check if the first or last intersection point has been moved -> update intersected profile and bank linestrings */
    try
    {
      checkIntersectionPoints( oldStartWdith, oldEndWdith );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    final SegmentData currentSegment = m_data.getCurrentSegment( m_data.m_selectedSegment );
    currentSegment.updateProfileIntersection();
    final CreateChannelData.PROF prof = m_data.getCurrentProfile();
    m_data.completationCheck();

    // CreateChannelData.PROF prof = m_data.getCurrentProfile();
    if( prof == CreateChannelData.PROF.UP )
      m_profile = currentSegment.getProfUpIntersProfile();
    else
      m_profile = currentSegment.getProfDownIntersProfile();

    if( breiteComponent != null )
      m_profile.getResult().setSortComponents( new IComponent[] { breiteComponent } );

    getProfilChartView().getChart().repaint();
    m_widget.getPanel().repaint();

  }

  private IProfil createNewProfile( final IRecord profilePoint, final double width, final double heigth, final GM_Point geoPoint )
  {

    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent rwComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hwComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_HOCHWERT );

    /* set the new values for the moved profile point */
    profilePoint.setValue( breiteComponent, width );
    profilePoint.setValue( hoeheComponent, heigth );
    profilePoint.setValue( rwComponent, geoPoint.getX() );
    profilePoint.setValue( hwComponent, geoPoint.getY() );

    /* sort profile points by width */
    final IRecord[] points = m_profile.getPoints();

    // TODO: save the sorted points as new m_profile
    final IProfil tmpProfil = ProfilFactory.createProfil( m_profile.getType() );

    tmpProfil.addPointProperty( breiteComponent );
    tmpProfil.addPointProperty( hoeheComponent );
    tmpProfil.addPointProperty( rwComponent );
    tmpProfil.addPointProperty( hwComponent );

    for( int i = 0; i < points.length; i++ )
    {
      final IRecord profilPoint = tmpProfil.createProfilPoint();

      final double breite = (Double) points[i].getValue( breiteComponent );
      final double hoehe = (Double) points[i].getValue( hoeheComponent );
      final double rw = (Double) points[i].getValue( rwComponent );
      final double hw = (Double) points[i].getValue( hwComponent );

      profilPoint.setValue( breiteComponent, breite );
      profilPoint.setValue( hoeheComponent, hoehe );
      profilPoint.setValue( rwComponent, rw );
      profilPoint.setValue( hwComponent, hw );

      tmpProfil.addPoint( profilPoint );
    }
    /* station */
    tmpProfil.setStation( m_profile.getStation() );

    /* coordinate system */
    final String srsName = (String) m_profile.getProperty( IWspmConstants.PROFIL_PROPERTY_CRS );
    tmpProfil.setProperty( IWspmConstants.PROFIL_PROPERTY_CRS, srsName );

    return tmpProfil;
  }

  /**
   * checks if the width coordinate is lying inside the given profile. If not, the width coordinate is set at an end
   * point of the profile.
   * 
   * @param width
   *            the width to be checked
   * @param profil
   *            the profile
   */
  private static double checkInsideProfile( final double width, final IProfil profil )
  {
    final double firstProfileWidth = (Double) profil.getPoints()[0].getValue( ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_BREITE ) );
    final double lastProfileWidth = (Double) profil.getPoints()[profil.getPoints().length - 1].getValue( ProfilObsHelper.getPropertyFromId( profil, IWspmConstants.POINT_PROPERTY_BREITE ) );

    /* if not force it to do so */
    if( width < firstProfileWidth )
      return firstProfileWidth;
    else if( width > lastProfileWidth )
      return lastProfileWidth;
    return width;
  }

  /**
   * checks if the intersection points have been moved and updates them in the data class (also for the neighbour
   * segments).
   * 
   * @param oldStartWdith
   *            the width coordinate of the first intersection profile point before user interaction
   * @param oldEndWdith
   *            the width coordinate of the last intersection profile point before user interaction
   */
  @SuppressWarnings("unchecked")
  private void checkIntersectionPoints( final double oldStartWdith, final double oldEndWdith ) throws Exception
  {
    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_HOEHE );

    final IRecord firstPoint = m_profile.getPoints()[0];
    final IRecord lastPoint = m_profile.getPoints()[m_profile.getPoints().length - 1];

    final double newStartWidth = (Double) firstPoint.getValue( breiteComponent );
    final double newEndWidth = (Double) lastPoint.getValue( breiteComponent );

    final CreateChannelData.PROF prof = m_data.getCurrentProfile();

    /* get the current segment to set the new intersection point for it */
    final int currentSegmentNum = m_data.getSelectedSegment();
    final SegmentData currentSegment = m_data.getCurrentSegment( currentSegmentNum );

    /* get the neighbor segments of the current segment */
    final List<SegmentData> neighbourSegments = m_data.getNeighbourSegments( currentSegmentNum );

    WIDTHORDER widthorder = null;
    double width = 0;
    GM_Point point = null;

    /* check, what intersection points have changed */
    if( oldStartWdith != newStartWidth && oldEndWdith != newEndWidth ) // both intersection points have been moved
    {
      // FIRST
      final WIDTHORDER widthorder1 = CreateChannelData.WIDTHORDER.FIRST;
      final double width1 = newStartWidth;
      final GM_Point point1 = WspmProfileHelper.getGeoPosition( width1, m_data.getProfilEventManager().getProfil() );

      /* update the first intersection point */
      currentSegment.setIntersPoint( point1, prof, widthorder1, width1 );
      /* intersection points have to have the same heigth as the orig profile */
      firstPoint.setValue( hoeheComponent, point1.getZ() );

      // LAST
      final WIDTHORDER widthorder2 = CreateChannelData.WIDTHORDER.LAST;
      final double width2 = newEndWidth;
      final GM_Point point2 = WspmProfileHelper.getGeoPosition( width2, m_data.getProfilEventManager().getProfil() );

      /* update the last intersection point */
      currentSegment.setIntersPoint( point2, prof, widthorder2, width2 );
      /* intersection points have to have the same heigth as the orig profile */
      lastPoint.setValue( hoeheComponent, point2.getZ() );

      /* update the intersected profile */
      currentSegment.setNewIntersectedProfile( m_profile, prof );

      // TODO: we should also update the neighbor segment of the current profile
      currentSegment.updateProfileIntersection();

      /* update the neighbour segment */
      updateNeighbourSegment( prof, currentSegment, neighbourSegments, widthorder1, width1, point1, widthorder2, width2, point2 );

    }
    else if( oldStartWdith != newStartWidth ) // first intersection point has been moved
    {
      widthorder = CreateChannelData.WIDTHORDER.FIRST;
      width = newStartWidth;
      point = WspmProfileHelper.getGeoPosition( width, m_data.getProfilEventManager().getProfil() );

      /* update the last intersection point */
      currentSegment.setIntersPoint( point, prof, widthorder, width );
      /* intersection points have to have the same heigth as the orig profile */
      firstPoint.setValue( hoeheComponent, point.getZ() );

      /* update the intersected profile */
      currentSegment.setNewIntersectedProfile( m_profile, prof );
      currentSegment.updateProfileIntersection();

      /* update the neighbour segment */
      updateNeighbourSegment( point, prof, currentSegment, neighbourSegments, widthorder, width );
    }
    else if( oldEndWdith != newEndWidth )
    {
      widthorder = CreateChannelData.WIDTHORDER.LAST;
      width = newEndWidth;
      point = WspmProfileHelper.getGeoPosition( width, m_data.getProfilEventManager().getProfil() );

      /* update the last intersection point */
      currentSegment.setIntersPoint( point, prof, widthorder, width );
      /* intersection points have to have the same heigth as the orig profile */
      lastPoint.setValue( hoeheComponent, point.getZ() );

      /* update the intersected profile */
      currentSegment.setNewIntersectedProfile( m_profile, prof );
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
   *            profile side of the current segment (up/down)
   * @param currentSegment
   *            segment data of the current segment
   * @param neighbourSegments
   *            list of the neighbouring segments
   * @param widthorder1
   *            widthorder of the first intersection point (first/last)
   * @param width1
   *            width coordinate of the new intersection point
   * @param point1
   *            geo coords of the new intersection point
   * @param widthorder2
   *            widthorder of the second intersection point (first/last)
   * @param width2
   *            width coordinate of the new intersection point
   * @param point2
   *            geo coords of the new intersection point
   */
  private void updateNeighbourSegment( final CreateChannelData.PROF prof, final SegmentData currentSegment, final List<SegmentData> neighbourSegments, final WIDTHORDER widthorder1, final double width1, final GM_Point point1, final WIDTHORDER widthorder2, final double width2, final GM_Point point2 )
  {
    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( final SegmentData segment : neighbourSegments )
      // check if the changed profile is in the neighbour segment, if not, do nothing.
      if( segment != currentSegment )
        if( segment.getProfilDownOrg().getStation() == m_profile.getStation() || segment.getProfilUpOrg().getStation() == m_profile.getStation() )
        {
          if( widthorder1 != null && widthorder2 != null )
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
          segment.setNewIntersectedProfile( m_profile, profNeighbour );
          segment.updateProfileIntersection();
        }
  }

  /**
   * updates one intersection point of the corresponding profile of the neighbouring segment.
   * 
   * @param prof
   *            profile side of the current segment (up/down)
   * @param currentSegment
   *            segment data of the current segment
   * @param neighbourSegments
   *            list of the neighbouring segments
   * @param widthorder1
   *            widthorder of the first intersection point (first/last)
   * @param width1
   *            width coordinate of the new intersection point
   * @param point1
   *            geo coords of the new intersection point
   */
  private void updateNeighbourSegment( final GM_Point point, final CreateChannelData.PROF prof, final SegmentData currentSegment, final List<SegmentData> neighbourSegments, final WIDTHORDER widthorder, final double width )
  {
    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( final SegmentData segment : neighbourSegments )
      // check if the changed profile is in the neighbour segment, if not, do nothing.
      if( segment != currentSegment )
        if( segment.getProfilDownOrg().getStation() == m_profile.getStation() || segment.getProfilUpOrg().getStation() == m_profile.getStation() )
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
          segment.setNewIntersectedProfile( m_profile, profNeighbour );
          // segment.updateProfileIntersection();
        }
  }

  /**
   * updates the profile for the neigboring segment.
   * 
   */
  @SuppressWarnings("unchecked")
  private void updateProfileForNeighbourSegment( )
  {
    /* get the current segment to set the new profile for it */
    final int currentSegmentNum = m_data.getSelectedSegment();
    final SegmentData currentSegment = m_data.getCurrentSegment( currentSegmentNum );

    /* get the neighbour segments of the current segment */
    final List<SegmentData> neighbourSegments = m_data.getNeighbourSegments( currentSegmentNum );
    final CreateChannelData.PROF prof = m_data.getCurrentProfile();

    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( final SegmentData segment : neighbourSegments )
      // check if the changed profile is in the neighbour segment, if not, do nothing.
      if( segment != currentSegment )
        if( segment.getProfilDownOrg().getStation() == m_profile.getStation() || segment.getProfilUpOrg().getStation() == m_profile.getStation() )
          segment.setNewIntersectedProfile( m_profile, profNeighbour );
    // segment.updateProfileIntersection();
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    if( m_profile == null )
      return null;

    final IRecord[] points = m_profile.getPoints();
    Rectangle2D bounds = null;
    for( final IRecord point : points )
    {
      final Point2D p = ProfilUtil.getPoint2D( point, ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) );
      if( bounds == null )
        bounds = new Rectangle2D.Double( p.getX(), p.getY(), 0, 0 );
      else
        bounds.add( p );
    }
    return bounds;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( final Point point )
  {
    if( m_profile == null )
      return null;

    int index = 0;
    final IRecord[] points = m_profile.getPoints();
    for( final IRecord pp : points )
    {
      final Point p = logical2screen( ProfilUtil.getPoint2D( pp, ProfilObsHelper.getPropertyFromId( pp, IWspmConstants.POINT_PROPERTY_HOEHE ) ) );
      final Rectangle hover = RectangleUtils.buffer( p );
      if( hover.contains( point ) )
      {
        final String string = String.format( "Breite: %.4f  \nRW: %.4f \nHW: %.4f", points[index].getValue( ProfilObsHelper.getPropertyFromId( points[index], IWspmConstants.POINT_PROPERTY_BREITE ) ), points[index].getValue( ProfilObsHelper.getPropertyFromId( points[index], IWspmConstants.POINT_PROPERTY_RECHTSWERT ) ), points[index].getValue( ProfilObsHelper.getPropertyFromId( points[index], IWspmConstants.POINT_PROPERTY_HOCHWERT ) ) );

        return new EditInfo( this, hover, new EditData( index, null ), string );
      }
      index++;
    }
    return null;
  }

  public IProfil getProfile( )
  {
    return m_profile;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#getZOrder()
   */
  @Override
  public int getZOrder( )
  {
    return 10;
  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#paint(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper)
   */
  public void paint( final GCWrapper gc )
  {
    if( m_profile == null )
      return;

    final IRecord[] points = m_profile.getPoints();
    if( points.length < 2 )
      return;
    Point leftP = null;
    final int lineWidthBuffer = gc.getLineWidth();
    final int lineStyleBuffer = gc.getLineStyle();
    final Color foregroundBuffer = gc.getForeground();
    gc.setLineWidth( 3 );
    gc.setLineStyle( SWT.LINE_SOLID );
    gc.setForeground( m_color );
    for( final IRecord point : points )
      if( leftP == null )
        leftP = logical2screen( ProfilUtil.getPoint2D( point, ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) ) );
      else
      {
        final Point rightP = logical2screen( ProfilUtil.getPoint2D( point, ProfilObsHelper.getPropertyFromId( point, IWspmConstants.POINT_PROPERTY_HOEHE ) ) );
        gc.drawOval( leftP.x - 2, leftP.y - 2, 4, 4 );
        gc.drawLine( leftP.x, leftP.y, rightP.x, rightP.y );
        leftP = rightP;
      }
    gc.drawOval( leftP.x - 2, leftP.y - 2, 4, 4 );
    gc.setLineWidth( lineWidthBuffer );
    gc.setLineStyle( lineStyleBuffer );
    gc.setForeground( foregroundBuffer );
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#paintDrag(org.kalypso.contribs.eclipse.swt.graphics.GCWrapper,
   *      org.eclipse.swt.graphics.Point, java.lang.Object)
   */
  @Override
  public void paintDrag( final GCWrapper gc, final Point currentScreenPoint, final Object hoverData )
  {
    /* get components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( m_profile, IWspmConstants.POINT_PROPERTY_HOEHE );

    /* draw the vertical line at the current mouse position */
    drawVerticalLine( gc, currentScreenPoint, m_color, SWT.LINE_DASH, 1 );

    /* draw the rectangular buffer around the current mouse position */
    final Rectangle editRect = RectangleUtils.buffer( currentScreenPoint );

    /* snap behaviour */
    /* find the nearest original profile point and check its distance to the cursor position */
    final Point2D logPoint = screen2logical( currentScreenPoint );
    final double currentWidth = logPoint.getX();

    final IProfil origProfil = m_data.getProfilEventManager().getProfil();
    final IRecord point = ProfilUtil.findNearestPoint( origProfil, currentWidth );

    final double xCoord = (Double) point.getValue( breiteComponent );
    final double yCoord = (Double) point.getValue( hoeheComponent );

    final Point2D point2 = new Point2D.Double( xCoord, yCoord );
    final Point profScreenPoint = logical2screen( point2 );

    if( point != null )
    {
      final double widthDifference = Math.abs( profScreenPoint.x - currentScreenPoint.x );
      if( widthDifference < 5 )
      {
        /*
         * check if there is allready a intersection point on the snapped position. if that is the case, visualize it by
         * a red line
         */
        final IRecord nearestIntersectedProfPoint = ProfilUtil.findNearestPoint( m_profile, xCoord );
        if( (Double) nearestIntersectedProfPoint.getValue( breiteComponent ) - xCoord == 0 )
        {
          final Color color = new Color( null, 255, 70, 70 );
          final int lineStyle = SWT.LINE_SOLID;
          final int lineWidth = 2;
          drawVerticalLine( gc, profScreenPoint, color, lineStyle, lineWidth );
        }
        else
        {
          final Color color = new Color( null, 70, 255, 70 );
          final int lineStyle = SWT.LINE_SOLID;
          final int lineWidth = 2;
          drawVerticalLine( gc, profScreenPoint, color, lineStyle, lineWidth );
        }
      }
      else
      {
        /* check, if the dragged point is lying too near at another intersection point */
        final Point profScreenPointInters = getNearestProfileScreenPoint( m_profile, currentWidth );

        final double widthDifferenceInters = Math.abs( profScreenPointInters.x - currentScreenPoint.x );
        if( widthDifferenceInters < 5 )
        {
          final Color color = new Color( null, 255, 70, 70 );
          final int lineStyle = SWT.LINE_SOLID;
          final int lineWidth = 2;
          drawVerticalLine( gc, profScreenPointInters, color, lineStyle, lineWidth );
        }
      }
    }

    gc.drawFocus( editRect.x, editRect.y, editRect.width, editRect.height );

  }

  /**
   * gets the nearest intersection profile point and returns it in screen coordinates.
   * 
   * @param currentWidth
   *            width coordinate of the current screen point
   */
  private Point getNearestProfileScreenPoint( final IProfil profile, final double currentWidth )
  {
    /* get components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_HOEHE );

    final IRecord intersectedPoint = ProfilUtil.findNearestPoint( profile, currentWidth );
    final double xCoordInters = (Double) intersectedPoint.getValue( breiteComponent );
    final double yCoordInters = (Double) intersectedPoint.getValue( hoeheComponent );
    final Point2D point2Inters = new Point2D.Double( xCoordInters, yCoordInters );
    final Point profScreenPointInters = logical2screen( point2Inters );
    return profScreenPointInters;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // do nothing
  }

  public void setProfile( final IProfil profile, final CreateChannelData data, final CreateMainChannelWidget widget )
  {
    m_profile = profile;
    m_data = data;
    m_widget = widget;

    if( m_profile != null )
    {
      final IComponent breiteComponent = m_profile.hasPointProperty( IWspmConstants.POINT_PROPERTY_BREITE );
      if( breiteComponent != null )
        m_profile.getResult().setSortComponents( new IComponent[] { breiteComponent } );
    }

  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return getLabel();
  }
}
