package org.kalypso.ogc.widgets;

import java.awt.Graphics;
import java.awt.Point;

import org.deegree.graphics.transformation.GeoTransform;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.command.JMMarkSelectCommand;
import org.kalypso.util.command.ICommand;
import org.kalypso.util.command.ICommandManager;

public class SelectWidget extends GisMapEditorWidgetActionDelegate
{
  private Point cursorPoint = null;

  private Point endPoint = null;

  private Point startPoint = null;

  private int mySelectionId = 10;

  private int myRadius = 20;

  private boolean mySnapEnabled = true;

  public void dragged( Point p )
  {
    if( startPoint != null )
      endPoint = p;
  }

  public void leftPressed( Point p )
  {
    startPoint = p;
    endPoint = null;
  }

  public void leftReleased( Point p )
  {
    if( endPoint != null ) // last update of endPoint

      endPoint = p;

    select();
  }

  public void moved( Point p )
  {
    cursorPoint = p;
  }

  public void paint( Graphics g )
  {
    if( startPoint != null && endPoint != null )
    {
      int px = (int)( startPoint.getX() < endPoint.getX() ? startPoint.getX() : endPoint.getX() );
      int py = (int)( startPoint.getY() < endPoint.getY() ? startPoint.getY() : endPoint.getY() );
      int dx = (int)Math.abs( endPoint.getX() - startPoint.getX() );
      int dy = (int)Math.abs( endPoint.getY() - startPoint.getY() );

      if( dx != 0 && dy != 0 )
        g.drawRect( px, py, dx, dy );
    }
    else if( mySnapEnabled && cursorPoint != null )
    {
      g.drawRect( (int)cursorPoint.getX() - myRadius, (int)cursorPoint.getY() - myRadius,
          2 * myRadius, 2 * myRadius );
    }
  }

  public ICommand performIntern()
  {
    return null;
  }

  private void select()
  {
    MapModell mapModell = myEditor.getMapPanel().getMapModell();
    GeoTransform transform = mapModell.getProjection();
    if( startPoint != null )
    {
      double g1x = transform.getSourceX( startPoint.getX() );
      double g1y = transform.getSourceY( startPoint.getY() );
      double gisRadius=transform.getSourceX( startPoint.getX()+myRadius )-g1x;
   
      if( endPoint == null ) // not dragged
      {
        ICommand command = new JMMarkSelectCommand( mapModell.getActiveTheme(), GeometryFactory
            .createGM_Position( g1x, g1y ), gisRadius, mySelectionId );

        ( (ICommandManager)myEditor ).postCommand( command, null );
      }
      else
      // dragged
      {
        double g2x = transform.getSourceX( endPoint.getX() );
        double g2y = transform.getSourceY( endPoint.getY() );
        boolean withinStatus = false;

        if( endPoint.getX() > startPoint.getX() && endPoint.getY() > startPoint.getY() )
          withinStatus = true;

        double minX = g1x < g2x ? g1x : g2x;
        double maxX = g1x > g2x ? g1x : g2x;
        double minY = g1y < g2y ? g1y : g2y;
        double maxY = g1y > g2y ? g1y : g2y;

        if( minX != maxX && minY != maxY )
        {
          ICommand command = new JMMarkSelectCommand( mapModell.getActiveTheme(), GeometryFactory
              .createGM_Envelope( minX, minY, maxX, maxY ), withinStatus, gisRadius, mySelectionId );

          ( (ICommandManager)myEditor ).postCommand( command, null );
        }
      }
    }

    startPoint = null;
    endPoint = null;
  }
}