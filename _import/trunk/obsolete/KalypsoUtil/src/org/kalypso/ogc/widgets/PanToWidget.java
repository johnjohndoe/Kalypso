/**
 * TODO: license definieren
 */

package org.kalypso.ogc.widgets;

import java.awt.Point;

import org.deegree.model.geometry.GM_Envelope;
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
      m_mapPanel.setOffset( dx, dy );
    }
  }

  public void finish()
  {
    m_mapPanel.clearOffset();
  }

  public void leftPressed( Point p )
  {
    startPoint = p;
    endPoint = null;
    m_mapPanel.clearOffset();
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
      final double mx = m_mapPanel.getWidth() / 2d - ( endPoint.getX() - startPoint.getX() );
      final double my = m_mapPanel.getHeight() / 2d - ( endPoint.getY() - startPoint.getY() );

      final GM_Envelope panBox = m_mapPanel.getPanToPixelBoundingBox( mx, my );

      startPoint = null;
      endPoint = null;

      if( panBox != null )
        return new ChangeExtentCommand( m_mapPanel.getMapModell(), panBox );
    }
    return null;
  }
}