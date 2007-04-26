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
package org.kalypso.floodrisk.rasterize;

import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import org.kalypso.floodrisk.internationalize.Messages;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.model.cv.RangeSet;
import org.kalypsodeegree_impl.model.cv.RectifiedGridCoverage2;
import org.kalypsodeegree_impl.model.cv.RectifiedGridDomain;

/**
 * Class for converting Vectordata (featureList (Properties: "GEOM" and "RasterProperty")) to Rasterdata
 * (RectifiedGridCoverages)
 * 
 * @author N. Peiler
 */
public class VectorToGridConverter
{

  /**
   * converts a List of Features to a RectifiedGridCoverage
   * 
   * @param featureList
   *          List of Features with properties "GEOM" and "RasterProperty" (value of gridCell)
   * @param propertyTable
   *          Mapping of key and propertyValue (e.g. landuseTypeList)
   * @param baseGrid
   *          RectifiedGridCoverage that defines the origin, offset and gridRange of the new grid
   * @return new RectifiedGridCoverage
   * @throws Exception
   */
  public static RectifiedGridCoverage2 toGrid( List featureList, Hashtable propertyTable, RectifiedGridCoverage2 baseGrid, ISimulationMonitor monitor ) throws Exception
  {
    String propertyName = "RasterProperty"; //$NON-NLS-1$
    final RectifiedGridDomain gridDomain = baseGrid.getGridDomain();
    GM_Point origin = gridDomain.getOrigin( null );
    RectifiedGridDomain newGridDomain = new RectifiedGridDomain( origin, gridDomain.getOffsetX(), gridDomain.getOffsetY(), gridDomain.getGridRange() );
    Vector<Vector<Double>> newRangeSetData = new Vector<Vector<Double>>();
    
    // ovde treba da iscitam fajl
    Vector rangeSetData = null;//baseGrid.getRangeSet().getRangeSetData();
    
    for( int i = 0; i < rangeSetData.size(); i++ )
    {
      Vector rowData = (Vector) rangeSetData.get( i );
      Vector<Double> newRowData = new Vector<Double>();
      for( int j = 0; j < rowData.size(); j++ )
      {
        final GM_Position position = gridDomain.getPositionAt( j, i );
        
        Feature actualFeature = null;
        if( rowData.get( j ) != null )
        {
          Long key = null;
          for( int k = 0; k < featureList.size(); k++ )
          {
            actualFeature = (Feature) featureList.get( k );
            GM_Object gm_Object = actualFeature.getDefaultGeometryProperty();
            if( gm_Object.contains( position ) )
            {
              String property = actualFeature.getProperty( propertyName ).toString();
              key = (Long) propertyTable.get( property );
              break;
            }
          }
          if( key != null )
          {
            newRowData.addElement( new Double( key.doubleValue() ) );
          }
          else
          {
            newRowData.addElement( null );
          }
        }
        else
        {
          newRowData.addElement( null );
        }
      }
      newRangeSetData.addElement( newRowData );
      monitor.setMessage( i + 1 + " "+Messages.getString("rasterize.VectorToGridConverter.RowsOf")+" " + rangeSetData.size() + " "+Messages.getString("rasterize.VectorToGridConverter.Calculated") ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
      monitor.setProgress( 100 * i / rangeSetData.size() );
      // System.out.println(i + 1 + " rows of " + rangeSetData.size() + "
      // calculated"+ " Progress: "+100 * i / rangeSetData.size());
    }
    RangeSet newRangeSet = new RangeSet( newRangeSetData, null );
    RectifiedGridCoverage2 newGrid = null;//RectifiedGridCoverage2.createRectifiedGridCoverage( newGridDomain, newRangeSet );
    return newGrid;
  }
}