package org.kalypso.ogc.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author bce
 */
public interface IWidget 
{
  public void rightReleased( final Point p );

  public void clickPopup( final Point p );

  public void dragged( Point p );

  public void finish();

  // MouseClicks
  public void leftClicked( Point p );

  public void leftPressed( Point p );

  public void leftReleased( Point p );

  public void middleClicked( Point p );

  public void middlePressed( Point p );

  public void middleReleased( Point p );

  // MouseMotions
  public void moved( Point p );

  // Graphics
  public void paint( Graphics g );

  /**
   * fuehrt die aktion aus
   */
  public void perform();

  public void rightClicked( Point p );

  public void rightPressed( Point p );

  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel );
}