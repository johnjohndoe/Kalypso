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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.event.KeyEvent;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * 
 * @author Patrice Congo
 */
public class MapKeyNavigator
{
  
  /**
   * Implements map navigation algorithm to react on a key event:
   *    <ul>
   *        <li/> +; zoom in
   *        <li/> -; zoom out
   *        <li/> left arrow; move to left
   *        <li/> right arrow move to right
   *        <li/> up arrow; move up
   *        <li/> down arrow; move down
   *    </ul>
   * @param mapPanel the map to navigate
   * @param keyEvent the event to react on
   * @param doRepaint flag to control the repaint of the map.
   *            if true the map is repainted
   * @throws IllegalArgumentException if mapPanel or event is null    
   */
  public static final void navigateOnKeyEvent( 
                MapPanel mapPanel, 
                KeyEvent keyEvent, 
                boolean doRepaint )
                throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( mapPanel, "mapPanel" );
    Assert.throwIAEOnNullParam( keyEvent, "keyEvent" );
    int code = keyEvent.getKeyCode();
    char keyChar= keyEvent.getKeyChar();
    System.out.println("keyEvent:"+keyEvent+
                        "\n\tKeyEvent.VK_PLUS:"+KeyEvent.VK_PLUS+
                        "\n\t+"+(int)'+');
    /* zoom in */
    if( keyChar == '+'/*KeyEvent.VK_PLUS*/ )
    {
      System.out.println("plus");
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 10;
      final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 10;
      final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 10;
      final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 10;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );

    }
    else if( keyChar == KeyEvent.VK_MINUS )
    {/* zoom out */
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 10;
      final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 10;
      final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 10;
      final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 10;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_RIGHT/*KeyEvent.VK_RIGHT*/ )
    {
      //pan "arrows
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() + (currentMax.getX() - currentMin.getX()) / 20;
      final double newMinX = currentMin.getX() + (currentMax.getX() - currentMin.getX()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_LEFT /*KeyEvent.VK_LEFT*/ )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxX = currentMax.getX() - (currentMax.getX() - currentMin.getX()) / 20;
      final double newMinX = currentMin.getX() - (currentMax.getX() - currentMin.getX()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( newMinX, currentMin.getY() );
      final GM_Position newMax = GeometryFactory.createGM_Position( newMaxX, currentMax.getY() );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_UP/*KeyEvent.VK_UP*/ )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxY = currentMax.getY() + (currentMax.getY() - currentMin.getY()) / 20;
      final double newMinY = currentMin.getY() + (currentMax.getY() - currentMin.getY()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    else if( code == KeyEvent.VK_DOWN/*KeyEvent.VK_DOWN*/ )
    {
      final GM_Envelope currentBBox = mapPanel.getBoundingBox();

      GM_Envelope wishBBox = null;

      final GM_Position currentMax = currentBBox.getMax();
      final GM_Position currentMin = currentBBox.getMin();

      final double newMaxY = currentMax.getY() - (currentMax.getY() - currentMin.getY()) / 20;
      final double newMinY = currentMin.getY() - (currentMax.getY() - currentMin.getY()) / 20;

      final GM_Position newMin = GeometryFactory.createGM_Position( currentMin.getX(), newMinY );
      final GM_Position newMax = GeometryFactory.createGM_Position( currentMax.getX(), newMaxY );

      wishBBox = GeometryFactory.createGM_Envelope( newMin, newMax );

      mapPanel.setBoundingBox( wishBBox );
    }
    if(doRepaint)
    {
      mapPanel.repaint();
    }
  }

}
