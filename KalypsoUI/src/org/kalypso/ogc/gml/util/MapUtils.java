/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.ogc.gml.util;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.swt.graphics.Rectangle;
import org.kalypso.contribs.eclipse.swt.awt.SWT_AWT_Utilities;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.command.CompositeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.mapfunctions.RectangleSelector;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class MapUtils
{
  public static void paintRect( final Graphics g, final MapPanel panel, final Feature feature, QName geomQName, final RectangleSelector rectangleSelector, final int grabRadius )
  {
    /* Draw drag rect if rectangle is big enough */
    if( rectangleSelector != null )
    {
      final Rectangle rectangle = rectangleSelector.getRectangle();
      if( rectangle != null && (rectangle.width > grabRadius || rectangle.height > grabRadius) )
      {
        g.drawRect( rectangle.x, rectangle.y, rectangle.width, rectangle.height );
        return;
      }
    }

    if( feature == null )
      return;
    final GM_Object geom = (GM_Object) feature.getProperty( geomQName );
    if( geom == null )
      return;

    final int smallRect = 10;
    final Point nodePoint = MapUtilities.retransform( panel, geom.getCentroid() );
    g.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
  }

  public static void removeFeature( CommandableWorkspace workspace, final MapPanel panel, Feature[] selectedFeatures ) throws Exception
  {
    if( !SWT_AWT_Utilities.showSwtMessageBoxConfirm( Messages.getString("org.kalypso.ogc.gml.util.MapUtils.0"), Messages.getString("org.kalypso.ogc.gml.util.MapUtils.1") ) ) //$NON-NLS-1$ //$NON-NLS-2$
      return;

    /* Select the feature */
    final IFeatureSelectionManager selectionManager = panel.getSelectionManager();

    final CompositeCommand compositeCommand = new CompositeCommand( Messages.getString("org.kalypso.ogc.gml.util.MapUtils.2") ); //$NON-NLS-1$
    for( final Feature featureToRemove : selectedFeatures )
    {
      selectionManager.changeSelection( new Feature[] { featureToRemove }, new EasyFeatureWrapper[] {} );

      final DeleteFeatureCommand command = new DeleteFeatureCommand( featureToRemove );
      compositeCommand.addCommand( command );
    }

    workspace.postCommand( compositeCommand );
  }

  @SuppressWarnings("unchecked") //$NON-NLS-1$
  public static void paintGrabbedFeature( final Graphics g, final MapPanel panel, final Feature feature, QName geomQName )
  {
    final Graphics2D g2 = (Graphics2D) g;
    final BasicStroke oldStroke = (BasicStroke) g2.getStroke();
    final Color oldColor = g2.getColor();

    final BasicStroke newStroke = new BasicStroke( 3 );

    g2.setStroke( newStroke );
    g2.setColor( new Color( 255, 0, 0 ) );

    if( feature == null )
      return;
    final GM_Object geom = (GM_Object) feature.getProperty( geomQName );
    if( geom == null )
      return;

    final int smallRect = 10;

    if( geom instanceof GM_Point )
    {
      final Point nodePoint = MapUtilities.retransform( panel, (GM_Point) geom );
      g2.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
    }
    else if( geom instanceof GM_Curve )
    {
      List<Point> pointList = new ArrayList<Point>();
      final GM_Curve curve = (GM_Curve) geom;
      try
      {
        final String crs = curve.getCoordinateSystem();
        GM_Position[] positions = curve.getAsLineString().getPositions();

        int[] xPoints = new int[positions.length];
        int[] yPoints = new int[positions.length];

        for( int i = 0; i < positions.length; i++ )
        {
          final GM_Point createGM_Point = GeometryFactory.createGM_Point( positions[i], crs );
          final Point nodePoint = MapUtilities.retransform( panel, createGM_Point );
          pointList.add( nodePoint );
          xPoints[i] = nodePoint.x;
          yPoints[i] = nodePoint.y;
        }

        g2.drawPolyline( xPoints, yPoints, pointList.size() );
      }
      catch( GM_Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }

    }
    else if( geom instanceof GM_Surface )
    {
      List<Point> pointList = new ArrayList<Point>();
      final GM_Surface<GM_SurfacePatch> surface = (GM_Surface<GM_SurfacePatch>) geom;
      final GM_SurfacePatch patch = surface.get( 0 );

      final String crs = surface.getCoordinateSystem();
      final GM_Position[] positions = patch.getExteriorRing();

      int[] xPoints = new int[positions.length];
      int[] yPoints = new int[positions.length];

      for( int i = 0; i < positions.length; i++ )
      {
        final GM_Point createGM_Point = GeometryFactory.createGM_Point( positions[i], crs );
        final Point nodePoint = MapUtilities.retransform( panel, createGM_Point );
        pointList.add( nodePoint );
        xPoints[i] = nodePoint.x;
        yPoints[i] = nodePoint.y;
      }

      g2.drawPolygon( xPoints, yPoints, pointList.size() );
    }
    else
    {
      final Point nodePoint = MapUtilities.retransform( panel, geom.getCentroid() );
      g2.drawRect( (int) nodePoint.getX() - smallRect, (int) nodePoint.getY() - smallRect, smallRect * 2, smallRect * 2 );
    }

    g2.setStroke( oldStroke );
    g2.setColor( oldColor );
  }
}
