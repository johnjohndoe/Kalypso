package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.event.ModellEvent;
import org.kalypso.ogc.gml.event.ModellEventListener;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandTarget;

/**
 * @author bce
 */
public abstract class AbstractWidget implements IWidget, ModellEventListener 
{
  private MapPanel m_mapPanel = null;
  private ICommandTarget m_commandPoster;
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#activate(org.kalypso.util.command.ICommandTarget, org.kalypso.ogc.gml.mapmodel.MapPanel)
   */
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    // unregister Modelllistener
    if(m_mapPanel!=null)
    {      
      IMapModell mapModell = m_mapPanel.getMapModell();
      if(mapModell!=null)
        mapModell.removeModellListener(this);
    }
      // TODO: register modelllistener?
    m_commandPoster = commandPoster;
    m_mapPanel = mapPanel;
    m_mapPanel.getMapModell().addModellListener(this);
    // registerModelllistener
  }
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#perform()
   */
  public void perform()
  {
    final ICommand command = performIntern();
    if( command != null )
      m_commandPoster.postCommand( command, null );
  }

  protected abstract ICommand performIntern();

  protected final GM_Position getPosition(Point pixelPoint)
  {
    final GeoTransform transform = m_mapPanel.getProjection();
    GM_Position pixelPos=GeometryFactory.createGM_Position(pixelPoint.getX(),pixelPoint.getY());
    return transform.getSourcePoint(pixelPos );    
  }
  
  // Helper
  protected final GM_Envelope getDragbox( int mx, int my, int dx )
  {
    if( m_mapPanel == null )
      return null;

    final double ratio = getRatio();

    final GeoTransform transform = m_mapPanel.getProjection();
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
    final GM_Envelope boundingBox = m_mapPanel.getBoundingBox();

    final double ratio = boundingBox.getHeight() / boundingBox.getWidth();
    return ratio;
  }

  /*
   * returns GM_Envelope for the pixel xmin, ymin, xmax, ymax.
   *   
   * GeoTransformInterface declares the methods which have to be implemented
   * by each class that executes a geographical coordinat transformation.
   */
  protected final GM_Envelope getBox(double x, double y, double x2, double y2) {
     
      final GeoTransform gt = m_mapPanel.getProjection();
      
      double xmin = gt.getSourceX(x);
      double ymin = gt.getSourceY(y);
      double xmax = gt.getSourceX(x2);
      double ymax = gt.getSourceY(y2);

      GM_Envelope bbox = GeometryFactory.createGM_Envelope(xmin, ymin, xmax, ymax);
      return bbox;
  }
  
  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#clickPopup(java.awt.Point)
   */
  public void clickPopup( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#dragged(java.awt.Point)
   */
  public void dragged( Point p )
  {
  // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish()
  {
    // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftClicked(java.awt.Point)
   */
  public void leftClicked( Point p )
  {
  // not implemented by default

  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftPressed(java.awt.Point)
   */
  public void leftPressed( Point p )
  {

  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#leftReleased(java.awt.Point)
   */
  public void leftReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleClicked(java.awt.Point)
   */
  public void middleClicked( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middlePressed(java.awt.Point)
   */
  public void middlePressed( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#middleReleased(java.awt.Point)
   */
  public void middleReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#moved(java.awt.Point)
   */
  public void moved( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightClicked(java.awt.Point)
   */
  public void rightClicked( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightPressed(java.awt.Point)
   */
  public void rightPressed( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#rightReleased(java.awt.Point)
   */
  public void rightReleased( Point p )
  {
  // not implemented by default
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#paint(java.awt.Graphics)
   */
  public void paint( Graphics g )
  {
    // not implemented by default
  }

  protected final MapPanel getMapPanel()
  {
    return m_mapPanel;
  }

  protected final void postCommand( final ICommand command, final Runnable runAfterCommand )
  {
    m_commandPoster.postCommand( command, runAfterCommand );
  }

  public void onModellChange( final ModellEvent modellEvent )
  {
    //
  }

  public IKalypsoLayer getActiveLayer()
  {
    try
    {
      return m_mapPanel.getMapModell().getActiveTheme().getLayer();
    }
    catch(Exception e)
    {
      return null;
    }
    }
  
  public FeatureType getActiveFeatureType()
  {
    try
    {
      return ((KalypsoFeatureLayer)getActiveLayer()).getFeatureType();
    }
    catch(Exception e)
    {
      // no active layer
      // layer not loaded complete
      // not a featurelayer e.g. a wms-layer
      return null;
    }
  }
  
  public KalypsoFeatureLayer[] getAllKalypsoFeatureLayers()
  {
    List result=new ArrayList();
    IKalypsoTheme[] themes=m_mapPanel.getMapModell().getAllThemes(); 
    for( int i = 0; i < themes.length; i++ )
    {
      IKalypsoLayer layer=themes[i].getLayer();
      if(layer!=null && layer instanceof KalypsoFeatureLayer)
        result.add(layer);
    }
    return (KalypsoFeatureLayer[])result.toArray(new KalypsoFeatureLayer[result.size()]);
  }
}