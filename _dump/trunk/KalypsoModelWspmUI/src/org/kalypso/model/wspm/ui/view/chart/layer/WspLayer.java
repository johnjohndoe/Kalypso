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
package org.kalypso.model.wspm.ui.view.chart.layer;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.graphics.Region;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIPlugin;
import org.kalypso.model.wspm.ui.view.ILayerStyleProvider;
import org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer;
import org.kalypso.observation.result.IRecord;

/**
 * Displays constant wsp lines in the cross section.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public abstract class WspLayer extends AbstractProfilLayer
{
  /**
   * The profile.
   */
  private IProfil m_profil;

  /**
   * The wsp layer data.
   */
  private IWspLayerData m_data;

  /**
   * True, if the area below the wsp lines should be filled. If there are more than one wsp line, this option should be
   * false, because you could see only the most above line and its area.
   */
  private boolean m_fill;

  /**
   * The constructor.
   * 
   * @param profile
   *          The profile.
   * @param layerId
   *          The id of the layer.
   * @param styleProvider
   *          The style provider.
   * @param data
   *          The wsp layer data.
   * @param fill
   *          True, if the area below the wsp lines should be filled. If there are more than one wsp line, this option
   *          should be false, because you could see only the most above line and its area.
   */
  public WspLayer( IProfil profile, String layerId, ILayerStyleProvider styleProvider, IWspLayerData data, boolean fill )
  {
    super( profile, layerId, styleProvider );

    m_profil = profile;
    m_data = data;
    m_fill = fill;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.AbstractProfilLayer#paint(org.eclipse.swt.graphics.GC)
   */
  @Override
  public void paint( GC gc )
  {
    try
    {
      if( m_data == null )
        return;

      /* Get all active names. */
      String[] names = m_data.getActiveNames();
      if( names == null || names.length == 0 )
        return;

      /* Get the profile. */
      IProfil profile = getProfil();
      if( profile == null )
        return;

      /* Get the station. */
      BigDecimal station = ProfilUtil.stationToBigDecimal( profile.getStation() );

      /* Paint the values for the names. */
      for( int i = 0; i < names.length; i++ )
      {
        /* Get the name. */
        String name = names[i];

        /* Search the value. */
        double value = m_data.searchValue( name, station );
        if( Double.isNaN( value ) )
          continue;

        /* Paint the value. */
        paint( gc, value );
      }
    }
    catch( Exception ex )
    {
      /* Log the error message. */
      KalypsoModelWspmUIPlugin.getDefault().getLog().log( new Status( IStatus.ERROR, KalypsoModelWspmUIPlugin.ID, ex.getLocalizedMessage(), ex ) );
    }
  }

  /**
   * This function paints one wsp line.
   * 
   * @param gc
   *          The graphical context.
   * @param height
   *          The height of the wsp line.
   */
  private void paint( GC gc, double height )
  {
    Rectangle clipping = gc.getClipping();

    Point location = getCoordinateMapper().numericToScreen( 0.0, height );

    Region clipreg = new Region();
    int[] points = getPoints();

    clipreg.add( points );
    clipreg.intersect( clipping );

    Rectangle toprect = new Rectangle( clipping.x, location.y - 100000, clipping.width, 100000 );
    clipreg.subtract( toprect );

    /* If not fill, ... */
    if( !m_fill )
    {
      int linesize = 2;
      Rectangle bottomrect = new Rectangle( clipping.x, location.y + linesize, clipping.width, 10000 );
      clipreg.subtract( bottomrect );
    }

    gc.setClipping( clipreg );

    Color oldColor = gc.getBackground();

    gc.setBackground( gc.getDevice().getSystemColor( SWT.COLOR_BLUE ) );
    gc.fillRectangle( clipping );

    gc.setBackground( oldColor );
    gc.setClipping( clipping );
  }

  /**
   * This function creates the points of the polygon above the line of the cross section.
   * 
   * @return The points.
   */
  private int[] getPoints( )
  {
    /* Create the polygon above the line of the cross section. */
    List<Point> points = new ArrayList<Point>();

    /* Get the record (points) of the profile. */
    IRecord[] ppoints = m_profil.getPoints();
    for( int i = 0; i < ppoints.length; i++ )
    {
      /* Get the record. */
      IRecord record = ppoints[i];

      /* Get x and y from the record. */
      Double x = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_BREITE, record );
      Double y = ProfilUtil.getDoubleValueFor( IWspmConstants.POINT_PROPERTY_HOEHE, record );

      /* Convert to screen coordinates. */
      Point point = getCoordinateMapper().numericToScreen( x, y );

      if( i == 0 )
        points.add( new Point( point.x, -1000 ) );

      points.add( point );

      if( i == ppoints.length - 1 )
        points.add( new Point( point.x, -1000 ) );
    }

    /* The array for the screen coordinates. */
    int[] ps = new int[points.size() * 2];

    /* Add the screen coordinates to the array. */
    int count = 0;
    for( int i = 0; i < points.size(); i++ )
    {
      /* Get the screen coordinates. */
      Point p = points.get( i );

      /* Add the screen coordinates to the array. */
      ps[count++] = p.x;
      ps[count++] = p.y;
    }

    return ps;
  }

  /**
   * This function returns the wsp layer data.
   * 
   * @return The wsp layer data.
   */
  public IWspLayerData getData( )
  {
    return m_data;
  }
}