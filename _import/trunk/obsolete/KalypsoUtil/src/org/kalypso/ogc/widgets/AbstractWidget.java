package org.kalypso.ogc.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * 
 * 
 * @author bce
 */
public abstract class AbstractWidget implements IWidget 
{
  protected MapPanel m_mapPanel = null;
  protected ICommandTarget m_commandPoster;
  
  public void setCommandPoster( final ICommandTarget commandPoster )
  {
    m_commandPoster = commandPoster;
  }
  
  /**
   * @see org.kalypso.ogc.widgets.IWidget#activate(org.kalypso.ogc.MapPanel)
   */
  public final void activate( final MapPanel mapPanel )
  {
    m_mapPanel = mapPanel;
  }
  
  /**
   * @see org.kalypso.ogc.widgets.IWidget#perform()
   */
  public void perform()
  {
    final ICommand command = performIntern();
    m_commandPoster.postCommand( command, null );
  }

  protected abstract ICommand performIntern();

  // Helper
  protected final GM_Envelope getDragbox( int mx, int my, int dx )
  {
    if( m_mapPanel == null )
      return null;

    final double ratio = getRatio();

    final GeoTransform transform = m_mapPanel.getMapModell().getProjection();
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
    final GM_Envelope boundingBox = m_mapPanel.getMapModell().getBoundingBox();

    final double ratio = boundingBox.getHeight() / boundingBox.getWidth();
    return ratio;
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
  // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#finish()
   */
  public void finish()
  {
    // not implemented by default
    System.out.println( "finish" + this );

  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
  // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {

  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    // not implemented by default
  }
}