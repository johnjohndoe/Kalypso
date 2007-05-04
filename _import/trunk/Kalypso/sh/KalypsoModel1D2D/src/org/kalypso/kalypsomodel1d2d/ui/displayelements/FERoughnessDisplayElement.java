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
package org.kalypso.kalypsomodel1d2d.ui.displayelements;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Area;


import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IStaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.StaticModel1D2D;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.SurfacePatchVisitableDisplayElement;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRoughnessPolygonCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * @author congo
 *
 */
public class FERoughnessDisplayElement implements DisplayElementDecorator
{
  private IStaticModel1D2D staticModel;
  private DisplayElement decorated;
  private boolean highlighted;
  private boolean selected;
  
  public FERoughnessDisplayElement( 
                      IStaticModel1D2D staticModel )
  {
    this.staticModel = staticModel;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#getDecorated()
   */
  public DisplayElement getDecorated( )
  {
    return decorated;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElementDecorator#setDecorated(org.kalypsodeegree.graphics.displayelements.DisplayElement)
   */
  public void setDecorated( DisplayElement decorated )
  {
   this.decorated = decorated; 
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( double scale )
  {
    if( decorated!= null )
    {
      return decorated.doesScaleConstraintApply( scale );
    }
    else
    {
      return true;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getAssociateFeatureId()
   */
  public String getAssociateFeatureId( )
  {
    if( decorated!= null )
    {
      return decorated.getAssociateFeatureId();
    }
    else
    {
      return staticModel.getGmlID();
    }
    
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getFeature()
   */
  public Feature getFeature( )
  {
    return staticModel.getWrappedFeature();
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isHighlighted()
   */
  public boolean isHighlighted( )
  {
    if ( decorated != null )
    {
      return decorated.isHighlighted();
    }
    else
    {
      return highlighted;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isSelected()
   */
  public boolean isSelected( )
  {
    if ( decorated != null )
    {
      return decorated.isSelected();
    }
    else
    {
      return selected;
    }
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#paint(java.awt.Graphics, org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( Graphics g, GeoTransform projection )
  {
    IFEDiscretisationModel1d2d model1d2d = staticModel.getDiscretisationModel();
    if( model1d2d == null )
    {
      return;
    }
    ITerrainModel terrainModel = staticModel.getTerrainModel();
    if( terrainModel == null )
    {
      return;
    }
    IRoughnessPolygonCollection rpc = terrainModel.getRoughnessPolygonCollection();
    if( rpc == null )
    {
      return;
    }
    IFeatureWrapperCollection<IFE1D2DElement> elements = model1d2d.getElements();
    for(IFE1D2DElement element: elements )
    {
      if( element instanceof IPolyElement )
      {
        try
        {
          GM_Surface surface = 
              (GM_Surface) element.recalculateElementGeometry();
          Area area = calcTargetCoordinates( projection, surface );
          
          Color color = Color.BLUE;
          g.setColor( color );
          ((Graphics2D) g ).fill( area );
          
//          java.awt.Stroke bs2 = new BasicStroke( 3 );
//          ((Graphics2D) graphics).setStroke( bs2 );
//          ((Graphics2D) graphics).draw( area );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    }
  }
  
  /**
   * calculates the Area (image or screen coordinates) where to draw the surface.
   */
  private Area calcTargetCoordinates( 
                      GeoTransform projection, 
                      GM_Surface surface ) 
                      throws Exception
  {
//    final PolygonSymbolizer sym = getSymbolizer();
//    final Stroke stroke = sym.getStroke();
    float width = 1;
//    if( stroke != null )
//    {
//      width = (float) stroke.getWidth( getFeature() );
//    }

    try
    {
      final GM_SurfacePatch patch = surface.getSurfacePatchAt( 0 );
      final GM_Position[] ex = patch.getExteriorRing();
      
      final Area areaouter = 
        SurfacePatchVisitableDisplayElement.areaFromRing( projection, width, ex );
     

      return areaouter;
    }
    catch( Exception e )
    {
      Debug.debugException( e, "" );
    }

    return null;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setHighlighted(boolean)
   */
  public void setHighlighted( boolean highlighted )
  {
    this.highlighted = highlighted;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setSelected(boolean)
   */
  public void setSelected( boolean selected )
  {
    this.selected = selected;
  }

  public static final FERoughnessDisplayElement createDisplayElement( Feature feature )
  {
    Assert.throwIAEOnNullParam( feature, "feature" );
    IStaticModel1D2D staticModel = 
      ( IStaticModel1D2D ) feature.getAdapter( IStaticModel1D2D.class );
    if( staticModel == null )
    {
      System.out.println("Could not adapt to static model 1d2d");
      return null;
    }
    else
    {
      return new FERoughnessDisplayElement(staticModel); 
    }
    
  }
}
