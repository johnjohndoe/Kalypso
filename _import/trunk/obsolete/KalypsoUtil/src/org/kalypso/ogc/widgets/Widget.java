
package org.kalypso.ogc.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;

/**
 * Implementiert alle Methoden von {@link Widget} leer.
 * 
 * @author von Dömming
 */
public abstract class Widget implements IWidget
{
  protected final MapPanel myMapPanel;
  private final ICommandManager myCommandManager;

  public Widget( final MapPanel mapPanel, final ICommandManager commandManager )
  {
    myMapPanel = mapPanel;
    myCommandManager = commandManager;
  }

  public abstract String getName();

  /**
   * @see org.kalypso.ogc.widgets.IWidget#activate()
   */
  public void activate()
  {
    // normales Widget macht hier nix
  }
  
  public void clickPopup( Point p )
  {
  //
  }

  public void dragged( Point p )
  {
  //
  }

  public void finish()
  {
  //
  }

  // MouseClicks
  public void leftClicked( Point p )
  {
  //
  }

  public void leftPressed( Point p )
  {
  //
  }

  public void leftReleased( Point p )
  {
  //
  }

  public void middleClicked( Point p )
  {
  //
  }

  public void middlePressed( Point p )
  {
  //
  }

  public void middleReleased( Point p )
  {
  //
  }

  // MouseMotions
  public void moved( Point p )
  {
  //
  }

  // Graphics
  public void paint( Graphics g )
  {
  //
  }

  public void rightClicked( Point p )
  {
  //
  }

  public void rightPressed( Point p )
  {
  //
  }

  public void rightReleased( Point p )
  {
  //
  }
  
  public final void perform()
  {
    final ICommand command = performIntern();
    if( command != null )
      myCommandManager.postCommand( command, null );
  }
  
  protected abstract ICommand performIntern();
  
  // Helper
  protected final GM_Envelope getDragbox( int mx, int my, int dx )
  {
      final double ratio = getRatio();

      final GeoTransform transform = myMapPanel.getMapModell().getProjection(  );
      double gisMX = transform.getSourceX( mx );
      double gisMY = transform.getSourceY( my );

      double gisX1 = transform.getSourceX( mx - dx );
      double gisDX = gisMX - gisX1;

      double gisDY = gisDX * ratio;

      double gisX2 = gisMX + gisDX;
      double gisY1 = gisMY - gisDY;
      double gisY2 = gisMY + gisDY;

      return GeometryFactory.createGM_Envelope( gisX1, gisY1, gisX2, gisY2 );
  }

  protected final double getRatio()
  {
    final GM_Envelope boundingBox = myMapPanel.getMapModell().getBoundingBox();
    
    final double ratio = boundingBox.getHeight(  ) / boundingBox.getWidth(  );
    return ratio;
  }

  
}