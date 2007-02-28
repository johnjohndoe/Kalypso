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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz;

import java.awt.Graphics;

import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ASCTerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelWrapper;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Envelope;

/**
 * Provide display mechanism for asc terrain elevation model
 *  
 * @author Madanagopal 
 * @author Patrice Congo
 */
public class ASCDisplayElement implements DisplayElement
{
  private NativeTerrainElevationModelWrapper elevationModel;
  
  private boolean isHighlighted;
 
  
  private boolean isSelected;

  private ASCTerrainElevationModel ascElevationModel;
 
  public ASCDisplayElement(
                NativeTerrainElevationModelWrapper elevationModel )
  {
    Assert.throwIAEOnNullParam( elevationModel, "elevationModel" );
    this.elevationModel=elevationModel;
    IElevationProvider elevationProvider = 
              elevationModel.getElevationProvider();
    //TODO continue
//    if(elevationProvider instanceof ASCTerrainElevationModel)
//    {
//      ascElevationModel=(ASCTerrainElevationModel)elevationProvider;
//    }
  }
  
  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#doesScaleConstraintApply(double)
   */
  public boolean doesScaleConstraintApply( double scale )
  {
    return false;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getAssociateFeatureId()
   */
  public String getAssociateFeatureId( )
  {
    return elevationModel.getGmlID(); 
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#getFeature()
   */
  public Feature getFeature( )
  {
    return elevationModel.getFeature();
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isHighlighted()
   */
  public boolean isHighlighted( )
  {
    return this.isHighlighted;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#isSelected()
   */
  public boolean isSelected( )
  {
    return this.isSelected;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#paint(java.awt.Graphics, org.kalypsodeegree.graphics.transformation.GeoTransform)
   */
  public void paint( Graphics g, GeoTransform projection )
  {
    
  }

  public void paint( 
                  Graphics g, 
                  GeoTransform p, 
                  double scale, 
                  GM_Envelope bbox, 
                  boolean selected )
  {
   
  }
  
  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setHighlighted(boolean)
   */
  public void setHighlighted( boolean highlighted )
  {
    this.isHighlighted=highlighted;
  }

  /**
   * @see org.kalypsodeegree.graphics.displayelements.DisplayElement#setSelected(boolean)
   */
  public void setSelected( boolean selected )
  {
    this.isSelected=selected;
  }

}
