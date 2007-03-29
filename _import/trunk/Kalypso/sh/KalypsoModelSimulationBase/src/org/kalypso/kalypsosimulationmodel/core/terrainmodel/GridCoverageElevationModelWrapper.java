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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import javax.xml.namespace.QName;

import org.kalypso.gis.doubleraster.DoubleRaster;
import org.kalypso.gis.doubleraster.RectifiedGridCoverageDoubleRaster;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactory;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;
import org.opengis.cs.CS_CoordinateSystem;



/**
 * @author Patrice Congo
 * @author Madanagopal
 *
 */
public class GridCoverageElevationModelWrapper extends AbstractFeatureBinder implements ITerrainElevationModel, IFeatureWrapper2
{
  private DoubleRaster doubleRaster; 
  
  

  public GridCoverageElevationModelWrapper( Feature featureToBind) throws Exception
  {
    this(
        featureToBind,
        KalypsoModelSimulationBaseConsts.SIM_BASE_F_GRID_COVERAGE_ELE_MODEL_WRAPPER);
  }
  
  public GridCoverageElevationModelWrapper( Feature featureToBind, QName qnameToBind ) throws Exception 
  {
    
    super(featureToBind, qnameToBind);
    Feature coverageFeature = (Feature)featureToBind.getProperty( KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_GRID_COVERAGE );
    doubleRaster = new RectifiedGridCoverageDoubleRaster(coverageFeature);
   
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {

    return super.getDescription();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
 
    return super.getName();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( String desc )
  {
    super.setDescription(desc);

  }
  

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( String name )
  {
    super.setName( name );

  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper#getGmlID()
   */
  public String getGmlID( )
  {

    return super.getGmlID();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    
    return super.getFeature();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    double xDouble=(location.getX()-doubleRaster.getOrigin().x);
    double yDouble=(location.getY()-doubleRaster.getOrigin().y);
    
    int x=(int)Math.floor(xDouble/doubleRaster.getSizeX());
    int y=(int)Math.floor(yDouble/doubleRaster.getSizeY());
    
    if(x>=doubleRaster.getSizeX() || y>=doubleRaster.getSizeY()||
        x<0|| y<0)
    {
      return Double.NaN;
    }
    
    return  doubleRaster.getValue( x, y );    
  }

   /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getBoundingBox()
   */
  public GM_Envelope getBoundingBox( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getCoordinateSystem()
   */
  public CS_CoordinateSystem getCoordinateSystem( )
  {
    //TODO Patrice get crs from grid coverate
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMaxElevation()
   */
  public double getMaxElevation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getMinElevation()
   */
  public double getMinElevation( )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#setCoordinateSystem(java.lang.String)
   */
  public void setCoordinateSystem( String coordinateSystem )
  {    
    throw new UnsupportedOperationException();
  }
}
