/**
 * TODO: license definieren
 */

package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.util.command.ICommand;

/**
 * 
 * @author von Dömming
 */
public class ZoomInWidget extends AbstractWidget
{
  private Point endPoint = null;

  private Point startPoint = null;

  public void dragged( Point p )
  {
    if( startPoint == null )
      startPoint = p;
    {
      endPoint = p;
    }
  }

  public void leftPressed( Point p )
  {
    startPoint = p;
    endPoint = null;
  }

  public void leftReleased( Point p )
  {
    endPoint = p;
    perform();
  }

  public void paint( Graphics g )
  {
    if( startPoint != null && endPoint != null )
    {
      final double ratio = getRatio();

      double dx = Math.abs( endPoint.getX() - startPoint.getX() );
      double dy = Math.abs( endPoint.getY() - startPoint.getY() );

      if( dx < 5 )
        dx = 5;

      if( dx * ratio > dy )
        dy = dx * ratio;
      else
        dx = dy / ratio;

      final int x1 = (int)( startPoint.getX() - dx );
      final int y1 = (int)( startPoint.getY() - dy );

      g.drawRect( x1, y1, (int)dx * 2, (int)dy * 2 );
    }
  }

  protected final ICommand performIntern()
  {
    if( startPoint != null && endPoint != null )
    {
      final double ratio = getRatio();

      double dx = Math.abs( endPoint.getX() - startPoint.getX() );
      double dy = Math.abs( endPoint.getY() - startPoint.getY() );

      if( dx < 5 )
        dx = 5;

      if( dx * ratio > dy )
        dy = dx * ratio;
      else
        dx = dy / ratio;

      GM_Envelope zoomBox = getDragbox( (int)startPoint.getX(), (int)startPoint.getY(), (int)dx );

      startPoint = null;
      endPoint = null;

      return new ChangeExtentCommand( getMapPanel(), zoomBox );
    }

    return null;
  }

}