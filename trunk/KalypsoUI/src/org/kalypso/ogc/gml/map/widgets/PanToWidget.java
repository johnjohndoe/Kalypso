/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.gml.map.widgets;

import java.awt.Point;

import org.kalypso.ogc.gml.command.ChangeExtentCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * @author Andreas von D�mming
 */
public class PanToWidget extends AbstractWidget
{
  private Point m_endPoint = null;

  private Point m_startPoint = null;

  public PanToWidget( final String name, final String toolTip )
  {
    super( name, toolTip );
  }

  public PanToWidget( )
  {
    super( "pan to", "" ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void dragged( final Point p )
  {
    if( m_startPoint != null )
    {
      m_endPoint = p;

      final int dx = (int) (m_endPoint.getX() - m_startPoint.getX());
      final int dy = (int) (m_endPoint.getY() - m_startPoint.getY());
      getMapPanel().setOffset( dx, dy );
    }
  }

  @Override
  public void finish( )
  {
    getMapPanel().setOffset( 0, 0 );
  }

  @Override
  public void leftPressed( final Point p )
  {
    m_startPoint = p;
    m_endPoint = null;
    getMapPanel().setOffset( 0, 0 );
  }

  @Override
  public void leftReleased( final Point p )
  {
    m_endPoint = p;
    perform();
  }

  public void perform( )
  {
    final MapPanel mapPanel = getMapPanel();

    if( m_startPoint != null && m_endPoint != null && !m_startPoint.equals( m_endPoint ) )
    {
      // REMARK: immediately suppress painting, in order to fix the ugly flicker (map gets paint at old position), after
      // pan has been released
      mapPanel.stopPaint();

      final double mx = mapPanel.getWidth() / 2d - (m_endPoint.getX() - m_startPoint.getX());
      final double my = mapPanel.getHeight() / 2d - (m_endPoint.getY() - m_startPoint.getY());

      final GM_Envelope panBox = mapPanel.getPanToPixelBoundingBox( mx, my );

      m_startPoint = null;
      m_endPoint = null;

      if( panBox != null )
      {
        final ChangeExtentCommand command = new ChangeExtentCommand( mapPanel, panBox );
        postViewCommand( command, null );
      }
    }
  }
}