/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
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
package org.kalypso.kalypsomodel1d2d.ui.map;

import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.util.PointSnapper;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.AbstractWidget;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * {@link IWidget} that provide the mechanism for edition the geometry of finite element concepts (Node, Edge, elements,
 * and Complex elements) This class decorate the {@link EditGeometryWidget} with the capability to :
 * <ul>
 * <li/>find all feature affected by a geometric change in the edited fe concepts; <li/>invalidate the envelops of the
 * found feature <li/> and fire feature change event holding the affected feature
 * </ul>
 * 
 * This widget rely on the assumption that the map to edit has layer holding feture with the QName
 * {@link Kalypso1D2DSchemaConstants#WB1D2D_F_NODE}
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * @author ThomaS Jung
 * 
 */
public class EditFEConceptGeometryWidget extends AbstractWidget
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 20;

  private IKalypsoFeatureTheme m_nodeTheme;

  private IKalypsoFeatureTheme m_polyElementTheme;

  private IKalypsoFeatureTheme m_edgeTheme;

  private IKalypsoFeatureTheme m_continuityLineTheme;

  private IKalypsoFeatureTheme m_flowRelationsTheme;

  private IMapModell m_mapModell;

  private IFEDiscretisationModel1d2d m_discModel;

  private Point m_currentMapPoint;

  private PointSnapper m_pointSnapper;

  @SuppressWarnings("unchecked")
  private IFE1D2DNode m_snapNode;

  private boolean m_snappingActive;

  private ElementGeometryEditor m_editor;

  public EditFEConceptGeometryWidget( )
  {
    super( "FE Model Geometrie Editieren", "FE Model Geometrie Editieren" );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#activate(org.kalypso.commons.command.ICommandTarget,
   *      org.kalypso.ogc.gml.map.MapPanel)
   */
  @Override
  public void activate( final ICommandTarget commandPoster, final MapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    m_mapModell = mapPanel.getMapModell();
    m_nodeTheme = UtilMap.findEditableTheme( m_mapModell, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    m_polyElementTheme = UtilMap.findEditableTheme( m_mapModell, IPolyElement.QNAME );
    m_edgeTheme = UtilMap.findEditableTheme( m_mapModell, IFE1D2DEdge.QNAME );
    m_continuityLineTheme = UtilMap.findEditableTheme( m_mapModell, IFELine.QNAME );
    m_flowRelationsTheme = UtilMap.findEditableTheme( m_mapModell, IBoundaryCondition.QNAME );

    m_discModel = UtilMap.findFEModelTheme( m_mapModell );
    m_pointSnapper = new PointSnapper( m_discModel, mapPanel );

    reinit();
    m_snappingActive = true;
  }

  private void reinit( )
  {
    m_editor = null;

    if( m_nodeTheme != null )
      m_editor = new ElementGeometryEditor( getMapPanel(), m_nodeTheme );

    m_snappingActive = true;
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#leftPressed(java.awt.Point)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void leftClicked( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    if( p == null )
      return;

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

    if( m_editor.getStartNode() == null )
    {
      final GM_Point currentPosition = MapUtilities.transform( panel, p );
      final double snapRadius = MapUtilities.calculateWorldDistance( panel, currentPosition, SNAPPING_RADIUS );
      m_editor.setStartNode( m_discModel.findNode( currentPosition, snapRadius ) );
    }
    else
    {
      if( m_editor.isValid() )
      {
        try
        {
          m_editor.finish();
          reinit();

        }
        catch( final Exception e )
        {
          e.printStackTrace();
          KalypsoModel1D2DPlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
        }
        finally
        {
          mapRepaint();
        }
      }
    }
    super.leftPressed( p );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.SnapToGeometryWidget#keyPressed(java.awt.event.KeyEvent)
   */
  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_ESCAPE )
      reinit();
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = false;
    else
      super.keyPressed( e );
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.AbstractWidget#keyReleased(java.awt.event.KeyEvent)
   */
  @Override
  public void keyReleased( final KeyEvent e )
  {
    if( e.getKeyCode() == KeyEvent.VK_SHIFT )
      m_snappingActive = true;

    super.keyReleased( e );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.SnapToGeometryWidget#paint(java.awt.Graphics)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void paint( final Graphics g )
  {
    GeoTransform projection = getMapPanel().getProjection();

    /* always paint a small rectangle of current position */
    if( m_currentMapPoint == null )
      return;

    final int[][] posPoints = getPointArrays( m_currentMapPoint );

    int[] arrayX = posPoints[0];
    int[] arrayY = posPoints[1];

    /* Paint as linestring. */
    g.drawPolygon( arrayX, arrayY, arrayX.length );
    drawHandles( g, arrayX, arrayY );

    /* paint the snap */
    if( m_pointSnapper != null )
      m_pointSnapper.paint( g );

    /* paint the preview */
    if( m_editor != null )
    {
      m_editor.paint( g, projection, m_currentMapPoint );
    }
    super.paint( g );
  }

  private static void drawHandles( final Graphics g, final int[] x, final int[] y )
  {
    final int sizeOuter = 6;
    for( int i = 0; i < y.length; i++ )
      g.drawRect( x[i] - sizeOuter / 2, y[i] - sizeOuter / 2, sizeOuter, sizeOuter );
  }

  /**
   * returns a point array for a given {@link IFE1D2DNode} and a {@link GM_Point}
   */
  @SuppressWarnings("unchecked")
  private int[][] getPointArrays( final Point currentPoint )
  {
    final List<Integer> xArray = new ArrayList<Integer>();
    final List<Integer> yArray = new ArrayList<Integer>();

    xArray.add( currentPoint.x );
    yArray.add( currentPoint.y );

    final int[] xs = ArrayUtils.toPrimitive( xArray.toArray( new Integer[xArray.size()] ) );
    final int[] ys = ArrayUtils.toPrimitive( yArray.toArray( new Integer[yArray.size()] ) );

    return new int[][] { xs, ys };
  }

  /**
   * @see org.kalypso.ogc.gml.map.widgets.EditGeometryWidget#moved(java.awt.Point)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void moved( final Point p )
  {
    final Object newNode = checkNewNode( p );
    if( newNode instanceof IFE1D2DNode )
      m_currentMapPoint = MapUtilities.retransform( getMapPanel(), ((IFE1D2DNode) newNode).getPoint() );
    else
      m_currentMapPoint = p;

    if( newNode == null )
      getMapPanel().setCursor( Cursor.getPredefinedCursor( Cursor.CROSSHAIR_CURSOR ) );
    else
      getMapPanel().setCursor( Cursor.getDefaultCursor() );

    if( p == null )
      return;

    final MapPanel panel = getMapPanel();
    if( panel != null )
      panel.repaint();

    final IKalypsoTheme activeTheme = getActiveTheme();
    if( activeTheme == null || !(activeTheme instanceof IKalypsoFeatureTheme) )
      return;

  }

  @SuppressWarnings("unchecked")
  private Object checkNewNode( final Point p )
  {
    final MapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return null;

    final GM_Point currentPoint = MapUtilities.transform( mapPanel, p );

    if( m_snappingActive )
      m_snapNode = m_pointSnapper == null ? null : m_pointSnapper.moved( currentPoint );

    final Object newNode = m_snapNode == null ? currentPoint : m_snapNode;

    if( m_editor == null )
      return null;

    if( m_editor.getStartNode() != null )
    {
      final IStatus status = m_editor.checkNewNode( newNode );
      if( status.isOK() )
        mapPanel.setMessage( "" );
      else
        mapPanel.setMessage( status.getMessage() );

      if( status.isOK() )
        return newNode;

    }
    else
      return newNode;

    return null;
  }
}
