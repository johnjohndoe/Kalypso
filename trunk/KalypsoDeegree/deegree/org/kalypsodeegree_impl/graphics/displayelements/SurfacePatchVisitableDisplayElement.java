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
package org.kalypsodeegree_impl.graphics.displayelements;

import java.awt.Graphics;
import java.awt.Polygon;

import org.eclipse.core.runtime.Assert;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitable;
import org.kalypsodeegree.model.geometry.ISurfacePatchVisitor;

/**
 * Provide display mechanism for terrain elevation models
 * 
 * @author Madanagopal
 * @author Patrice Congo
 */
public class SurfacePatchVisitableDisplayElement<P extends GM_SurfacePatch> implements DisplayElementDecorator
{
  public interface IVisitorFactory<P2 extends GM_SurfacePatch>
  {
    public ISurfacePatchVisitor<P2> createVisitor( final Graphics g, final GeoTransform projection, final IElevationColorModel model);
  }
  
  private final IElevationColorModel m_colorModel;

  private final ISurfacePatchVisitable<P> m_surfacePatchVisitable;

  private final Feature m_feature;

  private boolean m_isHighlighted = false;

  private boolean m_isSelected = false;

  private DisplayElement m_decorated;

  private final IVisitorFactory<P> m_visitorFactory;

  public SurfacePatchVisitableDisplayElement( final Feature feature, final ISurfacePatchVisitable<P> surfacePatchVisitable, final IElevationColorModel colorModel, final IVisitorFactory<P> visitorFactory )
  {
    m_visitorFactory = visitorFactory;
    Assert.isNotNull( surfacePatchVisitable, "surfacePatchVisitable" );

    m_feature = feature;
    
    // TODO: remove this color-model stuff
    m_colorModel = colorModel;

    m_surfacePatchVisitable = surfacePatchVisitable;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( final double scale )
  {
    if( m_decorated != null )
    {
      return m_decorated.doesScaleConstraintApply( scale );
    }
    else
    {
      return true;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getFeature()
   */
  public Feature getFeature( )
  {
    return m_feature;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isHighlighted()
   */
  public boolean isHighlighted( )
  {
    if( m_decorated != null )
    {
      return m_decorated.isHighlighted();
    }
    else
    {
      return this.m_isHighlighted;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isSelected()
   */
  public boolean isSelected( )
  {
    if( m_decorated != null )
    {
      return m_decorated.isSelected();
    }
    else
    {
      return this.m_isSelected;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setHighlighted(boolean)
   */
  public void setHighlighted( final boolean highlighted )
  {
    if( m_decorated != null )
    {
      m_decorated.setHighlighted( highlighted );
    }
    this.m_isHighlighted = highlighted;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setSelected(boolean)
   */
  public void setSelected( final boolean selected )
  {
    if( m_decorated != null )
    {
      m_decorated.setSelected( selected );
    }
    this.m_isSelected = selected;
  }

  public void paint( final Graphics g, final GeoTransform projection )
  {
    if( m_decorated != null )
    {
      m_decorated.paint( g, projection );
    }

    try
    {
      final ISurfacePatchVisitor<P> visitor = m_visitorFactory.createVisitor( g, projection, m_colorModel );
      m_surfacePatchVisitable.acceptSurfacePatches( projection.getSourceRect(), visitor );
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
  }

  public static final Polygon areaFromRing( final GeoTransform projection, final float width, final GM_Position[] ex )
  {
    final int[] x = new int[ex.length];
    final int[] y = new int[ex.length];

    int k = 0;
    for( final GM_Position element : ex )
    {
      final GM_Position position = projection.getDestPoint( element );
      final int xx = (int) (position.getX() + 0.5);
      final int yy = (int) (position.getY() + 0.5);

      if( k > 0 && k < ex.length - 1 )
      {
        if( distance( xx, yy, x[k - 1], y[k - 1] ) > width )
        {
          x[k] = xx;
          y[k] = yy;
          k++;
        }
      }
      else
      {
        x[k] = xx;
        y[k] = yy;
        k++;
      }
    }

    return new Polygon( x, y, k - 1 );
  }

  public static final double distance( final double x1, final double y1, final double x2, final double y2 )
  {
    return Math.sqrt( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) );
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#getDecorated()
   */
  public DisplayElement getDecorated( )
  {
    return m_decorated;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#setDecorated(org.kalypsodeegree.graphics.displayelements.DisplayElement)
   */
  public void setDecorated( final DisplayElement decorated )
  {
    m_decorated = decorated;
  }

  public GM_Position[] getGM_PositionForThisSurface( final GM_Surface<GM_SurfacePatch> surface )
  {
    final GM_SurfacePatch patch = surface.get( 0 );
    final GM_Position[] pos = patch.getExteriorRing();
    return pos;
  }

}
