package org.kalypso.ogc.gml.widgets;

import java.awt.Graphics;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.command.JMMarkSelectCommand;
import org.kalypso.ogc.gml.command.JMSelector;
import org.kalypso.ogc.gml.command.SingleSelectCommand;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.mapmodel.MapPanel;
import org.kalypso.util.command.ICommand;

public abstract class AbstractSelectWidget extends AbstractWidget
{
  private Point myEndPoint = null;

  private Point myStartPoint = null;

  private int myRadius = 20;

  abstract int getSelectionMode();

  abstract boolean allowOnlyOneSelectedFeature();

  public void dragged( Point p )
  {
    if( myStartPoint == null )
    {
      myStartPoint = p;
      myEndPoint = null;
    }
    else
      myEndPoint = p;
  }

  public void leftPressed( Point p )
  {
    myStartPoint = p;
    myEndPoint = null;
  }

  public void leftReleased( Point p )
  {
    if( myEndPoint != null ) // last update of endPoint

      myEndPoint = p;

    select();
  }

  public void paint( Graphics g )
  {
    if( myStartPoint != null && myEndPoint != null )
    {
      int px = (int)( myStartPoint.getX() < myEndPoint.getX() ? myStartPoint.getX() : myEndPoint
          .getX() );
      int py = (int)( myStartPoint.getY() < myEndPoint.getY() ? myStartPoint.getY() : myEndPoint
          .getY() );
      int dx = (int)Math.abs( myEndPoint.getX() - myStartPoint.getX() );
      int dy = (int)Math.abs( myEndPoint.getY() - myStartPoint.getY() );

      if( dx != 0 && dy != 0 )
        g.drawRect( px, py, dx, dy );
    }
  }

  public ICommand performIntern()
  {
    return null;
  }

  private void select()
  {
    // TODO: sollte diese ganze umrechnerei nicht einfach die view machen???

    final MapPanel mapPanel = getMapPanel();
    final IMapModell mapModell = mapPanel.getMapModell();
    GeoTransform transform = mapPanel.getProjection();
    final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
    KalypsoFeatureLayer activeLayer;
    if( activeTheme == null || !( activeTheme.getLayer() instanceof KalypsoFeatureLayer ) )
    {
      myStartPoint = null;
      myEndPoint = null;
      return;
    }
    activeLayer = (KalypsoFeatureLayer)activeTheme.getLayer();
    if( myStartPoint != null )
    {
      double g1x = transform.getSourceX( myStartPoint.getX() );
      double g1y = transform.getSourceY( myStartPoint.getY() );

      if( myEndPoint == null ) // not dragged
      {
        // TODO depend on featuretype
        // line and point with radius
        // polygon with without radius
        double gisRadius = transform.getSourceX( myStartPoint.getX() + myRadius ) - g1x;
        JMSelector selector = new JMSelector( getSelectionMode() );
        GM_Position pointSelect = GeometryFactory.createGM_Position( g1x, g1y );

        KalypsoFeature fe = selector.selectNearest( pointSelect, gisRadius, activeTheme, false,
            mapPanel.getSelectionID() );
        List listFe = new ArrayList();
        if( fe != null )
          listFe.add( fe );
        //List listFe = selector.select( pointSelect, activeTheme,
        // mapPanel.getSelectionID() );
        if( !listFe.isEmpty() )
        {
          fireCommand( listFe, activeLayer, mapPanel.getSelectionID() );
        }
      }
      else
      // dragged
      {
        double g2x = transform.getSourceX( myEndPoint.getX() );
        double g2y = transform.getSourceY( myEndPoint.getY() );
        boolean withinStatus = false;

        if( myEndPoint.getX() > myStartPoint.getX() && myEndPoint.getY() > myStartPoint.getY() )
          withinStatus = true;

        double minX = g1x < g2x ? g1x : g2x;
        double maxX = g1x > g2x ? g1x : g2x;
        double minY = g1y < g2y ? g1y : g2y;
        double maxY = g1y > g2y ? g1y : g2y;

        if( minX != maxX && minY != maxY )
        {
          JMSelector selector = new JMSelector( getSelectionMode() );
          GM_Envelope envSelect = GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY );
          List listFe = selector.select( envSelect, activeTheme, withinStatus, mapPanel
              .getSelectionID() );
          if( !listFe.isEmpty() )
            fireCommand( listFe, activeLayer, mapPanel.getSelectionID() );
        }
      }
    }
    myStartPoint = null;
    myEndPoint = null;
  }

  private void fireCommand( List listFe, KalypsoFeatureLayer activeLayer, int selectionId )
  {
    ICommand command = null;
    if( allowOnlyOneSelectedFeature() )
    {
      KalypsoFeature fe = (KalypsoFeature)listFe.get( 0 );
      command = new SingleSelectCommand( fe, selectionId, activeLayer, getAllKalypsoFeatureLayers() );
    }
    else
    {
      List[] feLists = new List[]
      { listFe };
      KalypsoFeatureLayer[] feLayers = new KalypsoFeatureLayer[]
      { activeLayer };
      command = new JMMarkSelectCommand( feLists, selectionId, getSelectionMode(), feLayers );
    }
    postCommand( command, null );
  }

  /**
   * @see org.kalypso.ogc.gml.widgets.IWidget#finish()
   */
  public void finish()
  {
    myStartPoint = null;
    myEndPoint = null;
    super.finish();
  }
}