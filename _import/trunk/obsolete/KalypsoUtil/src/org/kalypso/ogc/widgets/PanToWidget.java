/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import java.awt.Point;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommand;

/**
 * @author vDoemming
 */
public class PanToWidget extends AbstractWidget
{
  private Point endPoint = null;

  private Point startPoint = null;
  
  public void dragged( Point p )
  {
    if( startPoint != null )
    {
      endPoint = p;

      int dx = (int)( endPoint.getX() - startPoint.getX() );
      int dy = (int)( endPoint.getY() - startPoint.getY() );
      getMapPanel().setOffset( dx, dy );
    }
  }

  public void finish()
  {
    getMapPanel().clearOffset();
  }

  public void leftPressed( Point p )
  {
    startPoint = p;
    endPoint = null;
    getMapPanel().clearOffset();
  }

  public void leftReleased( Point p )
  {
    endPoint = p;
    perform();
  }

  /**
   * @see org.kalypso.ogc.widgets.AbstractWidget#performIntern()
   */
  public ICommand performIntern()
  {
    if( startPoint != null && endPoint != null )
    {
      final MapPanel mapPanel = getMapPanel();
      final double mx = mapPanel.getWidth() / 2d - ( endPoint.getX() - startPoint.getX() );
      final double my = mapPanel.getHeight() / 2d - ( endPoint.getY() - startPoint.getY() );

      final GM_Envelope panBox = mapPanel.getPanToPixelBoundingBox( mx, my );

      startPoint = null;
      endPoint = null;

      if( panBox != null )
        return new ChangeExtentCommand( mapPanel, panBox );
    }
    return null;
  }
}