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
package org.kalypso.kalypsomodel1d2d.ui.map.merge;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Stroke;
import java.awt.geom.Area;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFENetItem;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_AbstractSurfacePatch;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Polygon;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.SurfacePatchVisitableDisplayElement;

/**
 * Provides the mechanism to paint a calculation unit.
 * 
 * @author Patrice Congo
 */
public class CalUnitDisplayElement implements DisplayElementDecorator
{
  private final ICalculationUnit m_calculationUnit;

  private static final Color ELEMENT_FILL_COLOR = new Color( 0, 0, 255, 85 );

  private static final Color ELEMENT_BORDER_COLOR = Color.BLUE;

  private static final float ELEMENT_BORDER_WIDTH = 1;

  private static final Color CONTINUITY_LINE_COLOR = Color.BLUE;

  private static final float CONTINUITY_LINE_WIDTH = 5;

  public CalUnitDisplayElement( final ICalculationUnit calUnit )
  {
    m_calculationUnit = calUnit;
  }

  @Override
  public void setDecorated( final DisplayElement decorated )
  {
  }

  @Override
  public Feature getFeature( )
  {
    if( m_calculationUnit != null )
    {
      return m_calculationUnit;
    }
    else
    {
      return null;
    }
  }

  @Override
  public void paint( final Graphics g, final GeoTransform projection, final IProgressMonitor monitor )
  {
    if( m_calculationUnit == null )
      return;

    final GM_Envelope sourceRect = projection.getSourceRect();
    final List<IFENetItem> visibleElements = m_calculationUnit.query( sourceRect, null );
    for( final Feature element : visibleElements )
    {
      if( element instanceof IPolyElement )
      {
        try
        {
          final GM_Polygon surface = ((IPolyElement)element).getGeometry();
          paintSurface( surface, ELEMENT_FILL_COLOR, (Graphics2D)g, projection, ELEMENT_BORDER_WIDTH );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
        }
      }
      else if( element instanceof IElement1D )
      {
        try
        {
          final GM_Curve curve = ((IElement1D)element).getEdge().getGeometry();
          paintLineString( curve, ELEMENT_BORDER_COLOR, (Graphics2D)g, projection, CONTINUITY_LINE_WIDTH );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException( e );
        }
      }
      else if( element instanceof IFELine )
      {
        try
        {
          final GM_Curve curve = ((IFELine)element).getGeometry();
          paintLineString( curve, CONTINUITY_LINE_COLOR, (Graphics2D)g, projection, CONTINUITY_LINE_WIDTH );
        }
        catch( final Exception e )
        {
          e.printStackTrace();
          throw new RuntimeException( e );
        }
      }
      else
      {
        throw new RuntimeException( "Unexpected type of element:" + element ); //$NON-NLS-1$
      }
    }
  }

  public static final void paintSurface( final GM_Polygon surface, final Color color, final Graphics2D g2d, final GeoTransform projection, final float lineWidth )
  {
    try
    {
      final Area area = calcTargetCoordinates( projection, surface );
      g2d.setColor( color );
      g2d.fill( area );

      // shape drawing
      g2d.setColor( ELEMENT_BORDER_COLOR );
      final java.awt.Stroke bs2 = new BasicStroke( lineWidth );
      g2d.setStroke( bs2 );
      g2d.draw( area );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  public static final void paintLineString( final GM_Curve curve, final Color color, final Graphics2D g2d, final GeoTransform projection, final float lineWidth ) throws Exception
  {
    final int[][] linePointCoords = calcTargetCoordinates( projection, curve );

    final Stroke stroke = new BasicStroke( lineWidth );
    g2d.setStroke( stroke );
    g2d.setColor( color );
    g2d.drawPolyline( linePointCoords[0], linePointCoords[1], linePointCoords[2][0] );
  }

  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  public static final Area calcTargetCoordinates( final GeoTransform projection, final GM_Polygon surface ) throws Exception
  {
    final float width = 1;
    try
    {
      final GM_AbstractSurfacePatch patch = surface.get( 0 );
      final GM_Position[] ex = patch.getExteriorRing();

      final Area areaouter = new Area( SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, ex ) );

      return areaouter;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return null;
  }

  public static final CalUnitDisplayElement createDisplayElement( final ICalculationUnit calUnit )
  {
    Assert.throwIAEOnNullParam( calUnit, "calUnit" ); //$NON-NLS-1$
    final CalUnitDisplayElement calUnitDisplayElement = new CalUnitDisplayElement( calUnit );

    return calUnitDisplayElement;
  }

  /**
   * Copied and modified from {@link org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement} calculates the coordintes (image or screen coordinates) where to draw the curve.
   */
  public static final int[][] calcTargetCoordinates( final GeoTransform projection, final GM_Curve curve ) throws Exception
  {
    final GM_LineString lineString = curve.getAsLineString();
    final float width = 1;
    final int count = lineString.getNumberOfPoints();
    final int[][] pos = new int[3][];
    pos[0] = new int[count];
    pos[1] = new int[count];
    pos[2] = new int[1];

    int k = 0;
    for( int i = 0; i < count; i++ )
    {
      final GM_Position position = lineString.getPositionAt( i );
      final double tx = projection.getDestX( position.getX() );
      final double ty = projection.getDestY( position.getY() );
      if( i > 0 && i < count - 1 )
      {
        if( distance( tx, ty, pos[0][k - 1], pos[1][k - 1] ) > width )
        {
          pos[0][k] = (int)(tx + 0.5);
          pos[1][k] = (int)(ty + 0.5);
          k++;
        }
      }
      else
      {
        pos[0][k] = (int)(tx + 0.5);
        pos[1][k] = (int)(ty + 0.5);
        k++;
      }
    }
    pos[2][0] = k;

    return pos;
  }

  /**
   * Copied from {@link org.kalypsodeegree.graphics.displayelements.LineStringDisplayElement}
   */
  public static final double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

}
