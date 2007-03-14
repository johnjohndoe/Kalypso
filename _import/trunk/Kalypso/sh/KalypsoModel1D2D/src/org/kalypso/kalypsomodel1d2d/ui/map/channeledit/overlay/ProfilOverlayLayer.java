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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit.overlay;

import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.Collections;
import java.util.LinkedList;
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
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.util.ProfilComparator;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer;
import org.kalypso.model.wspm.ui.view.chart.ProfilChartView;
import org.kalypso.model.wspm.ui.view.chart.AbstractPolyLineLayer.EditData;
import org.kalypsodeegree.model.geometry.GM_Point;

import de.belger.swtchart.EditInfo;
import de.belger.swtchart.axis.AxisRange;

/**
 * @author kimwerner
 */
public class ProfilOverlayLayer extends AbstractProfilChartLayer
{
  private IProfil m_profile;

  CreateMainChannelWidget m_widget;

  private final Color m_color;

  private CreateChannelData m_data;

  public ProfilOverlayLayer( final ProfilChartView chartView )
  {
    super( IWspmOverlayConstants.LAYER_OVERLAY, chartView, chartView.getDomainRange(), chartView.getValueRangeLeft(), "Zeichenfl‰che" );

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

  protected void drawEditLine( final GCWrapper gc, final Point editing, final int index )
  {
    if( 0 <= index && index < m_profile.getPoints().size() )
    {
      final int lineWidthBuffer = gc.getLineWidth();
      final int lineStyleBuffer = gc.getLineStyle();
      final Color foregroundBuffer = gc.getForeground();
      gc.setLineStyle( SWT.LINE_DASH );
      gc.setLineWidth( 1 );
      gc.setForeground( m_color );

      // final Point p = logical2screen( m_points.get( index ) );
      final AxisRange valueRange = getValueRange();
      final int bottom = valueRange.getScreenFrom() + valueRange.getGapSpace();
      int top = valueRange.getScreenTo() + valueRange.getGapSpace();
      gc.drawLine( editing.x, bottom, editing.x, top );
      gc.setLineWidth( lineWidthBuffer );
      gc.setLineStyle( lineStyleBuffer );
      gc.setForeground( foregroundBuffer );
    }
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#editProfil(org.eclipse.swt.graphics.Point,
   *      java.lang.Object)
   */
  @Override
  protected void editProfil( Point point, Object data ) // release
  {
    final EditData editData = (EditData) data;
    final IProfilPoint profilePoint = m_profile.getPoints().get( editData.getIndex() );
    final Point2D logPoint = screen2logical( point );

    double width = logPoint.getX();

    /* check that the point is lying inside the orig. profile */
    final double firstProfileWidth = m_data.getProfilEventManager().getProfil().getPoints().getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double lastProfileWidth = m_data.getProfilEventManager().getProfil().getPoints().getLast().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    /* if not force it to do so */
    if( width < firstProfileWidth )
      width = firstProfileWidth;
    else if( width > lastProfileWidth )
      width = lastProfileWidth;

    /* set the initial heigth to the profile height */
    double heigth = 0;
    try
    {
      heigth = WspmProfileHelper.getHeigthPositionByWidth( width, m_data.getProfilEventManager().getProfil() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    /* get the geo coordinates for the moved profile point */
    GM_Point gmPoint = null;

    try
    {
      gmPoint = WspmProfileHelper.getGeoPosition( width, m_data.getProfilEventManager().getProfil() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    /* save the first and last widths of the intersected profile for comparing them with the new widths */
    final double oldStartWdith = m_profile.getPoints().getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double oldEndWdith = m_profile.getPoints().getLast().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );

    /* set the new values for the moved profile point */
    profilePoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, heigth );
    profilePoint.setValueFor( IWspmConstants.POINT_PROPERTY_BREITE, width );
    profilePoint.setValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT, gmPoint.getX() );
    profilePoint.setValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT, gmPoint.getY() );

    /* sort profile points by width */
    final LinkedList<IProfilPoint> points = m_profile.getPoints();
    final ProfilComparator comparator = new ProfilComparator( IWspmConstants.POINT_PROPERTY_BREITE );
    Collections.sort( points, comparator );

    getProfilChartView().getChart().repaint();

    /* check if the first or last intersection point was moved -> update bank linestrings */
    checkIntersectionPoints( gmPoint, oldStartWdith, oldEndWdith );

    m_data.completationCheck();
    m_widget.getPanel().repaint();

  }

  /**
   * checks if the intersection points have been moved and updates them in the data class (also for the neighbour
   * segments).
   */
  private void checkIntersectionPoints( GM_Point gmPoint, double oldStartWdith, double oldEndWdith )
  {
    final double newStartWidth = m_profile.getPoints().getFirst().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    final double newEndWidth = m_profile.getPoints().getLast().getValueFor( IWspmConstants.POINT_PROPERTY_BREITE );
    CreateChannelData.PROF prof = m_data.getCurrentProfile();

    /* get the current segment to set the new intersectionpoint for it */
    final int currentSegmentNum = m_data.getSelectedSegment();
    final SegmentData currentSegment = m_data.getCurrentSegment( currentSegmentNum );

    double width = 0;
    // TODO: manage it if both inters points have changed
    final CreateChannelData.WIDTHORDER widthorder;
    if( oldStartWdith != newStartWidth ) // first intersection point has been moved
    {
      widthorder = CreateChannelData.WIDTHORDER.FIRST;
      width = newStartWidth;
    }
    else if( oldEndWdith != newEndWidth )
    {
      widthorder = CreateChannelData.WIDTHORDER.LAST;
      width = newEndWidth;
    }
    else
      widthorder = null;

    if( widthorder != null )
      currentSegment.setIntersPoint( gmPoint, prof, widthorder, width );

    currentSegment.setNewIntersectedProfile( m_profile, prof );
    currentSegment.updateProfileIntersection();

    /* get the neighbour segments */
    final List<SegmentData> neighbourSegments = m_data.getNeighbourSegments( currentSegmentNum );

    /* change prof, because now it is the profile of the other side of the segment */
    CreateChannelData.PROF profNeighbour = prof;
    if( prof == CreateChannelData.PROF.DOWN )
      profNeighbour = CreateChannelData.PROF.UP;
    else if( prof == CreateChannelData.PROF.UP )
      profNeighbour = CreateChannelData.PROF.DOWN;

    for( SegmentData segment : neighbourSegments )
    {
      // ust look if the changed profile is in the neighbour segment, if not, do nothing.
      if( segment != currentSegment )
      {
        if( segment.getProfilDownOrg().getStation() == m_profile.getStation() || segment.getProfilUpOrg().getStation() == m_profile.getStation() )
        {
          if( widthorder != null )
            segment.setIntersPoint( gmPoint, profNeighbour, widthorder, width );
          segment.setNewIntersectedProfile( m_profile, profNeighbour );
          segment.updateProfileIntersection();
        }
      }
    }

  }

  /**
   * @see de.belger.swtchart.layer.IChartLayer#getBounds()
   */
  public Rectangle2D getBounds( )
  {
    if( m_profile == null )
      return null;

    final LinkedList<IProfilPoint> points = m_profile.getPoints();
    Rectangle2D bounds = null;
    for( final IProfilPoint point : points )
    {
      final Point2D p = ProfilUtil.getPoint2D( point, IWspmConstants.POINT_PROPERTY_HOEHE );
      if( bounds == null )
      {
        bounds = new Rectangle2D.Double( p.getX(), p.getY(), 0, 0 );
      }
      else
      {
        bounds.add( p );
      }
    }
    return bounds;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilChartLayer#getHoverInfo(org.eclipse.swt.graphics.Point)
   */
  @Override
  public EditInfo getHoverInfo( Point point )
  {
    if( m_profile == null )
      return null;

    int index = 0;
    final LinkedList<IProfilPoint> points = m_profile.getPoints();
    for( final IProfilPoint pp : points )
    {
      final Point p = logical2screen( ProfilUtil.getPoint2D( pp, IWspmConstants.POINT_PROPERTY_HOEHE ) );
      final Rectangle hover = RectangleUtils.buffer( p );
      if( hover.contains( point ) )
      {

        String string = String.format( "Breite: %.4f  \nRW: %.4f \nHW: %.4f", points.get( index ).getValueFor( IWspmConstants.POINT_PROPERTY_BREITE ), points.get( index ).getValueFor( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), points.get( index ).getValueFor( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

        return new EditInfo( this, hover, new EditData( index, "" ), string );
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
  public void paint( GCWrapper gc )
  {
    if( m_profile == null )
      return;

    final LinkedList<IProfilPoint> points = m_profile.getPoints();
    if( points.size() < 2 )
      return;
    Point leftP = null;
    final int lineWidthBuffer = gc.getLineWidth();
    final int lineStyleBuffer = gc.getLineStyle();
    final Color foregroundBuffer = gc.getForeground();
    gc.setLineWidth( 3 );
    gc.setLineStyle( SWT.LINE_SOLID );
    gc.setForeground( m_color );
    for( final IProfilPoint point : points )
    {
      if( leftP == null )
      {
        leftP = logical2screen( ProfilUtil.getPoint2D( point, IWspmConstants.POINT_PROPERTY_HOEHE ) );
      }
      else
      {
        final Point rightP = logical2screen( ProfilUtil.getPoint2D( point, IWspmConstants.POINT_PROPERTY_HOEHE ) );
        gc.drawOval( leftP.x - 2, leftP.y - 2, 4, 4 );
        gc.drawLine( leftP.x, leftP.y, rightP.x, rightP.y );
        leftP = rightP;
      }
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
  public void paintDrag( GCWrapper gc, Point editing, Object hoverData )
  {
    final EditData editData = (EditData) hoverData;
    final int index = editData.getIndex();
    drawEditLine( gc, editing, index - 1 );
    drawEditLine( gc, editing, index + 1 );

    final Rectangle editRect = RectangleUtils.buffer( editing );
    gc.drawFocus( editRect.x, editRect.y, editRect.width, editRect.height );

  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.IProfilChartLayer#removeYourself()
   */
  public void removeYourself( )
  {
    // do nothing
  }

  public void setProfile( final IProfil profile, CreateChannelData data, CreateMainChannelWidget widget )
  {
    m_profile = profile;
    m_data = data;
    m_widget = widget;
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
