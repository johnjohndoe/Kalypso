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
package org.kalypso.kalypsosimulationmodel.core.wind;

import org.deegree.framework.util.Pair;
import org.kalypso.grid.IGeoGrid;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;

/**
 * Interface for classes that provide wind data information. The wind data can be get using
 * {@link #getWindAsVector(GM_Point)} or {@link #getWindAsSpeedAndDirection(GM_Point)}.
 * 
 * @author ig
 * 
 */
public interface IWindDataProvider extends IWindDataWrapper
{
  /**
   * Get the wind data as X and Y components provided by this model for the specified location [(x,y) position].
   * 
   * @param location
   *            the location for which an wind data is to be computed
   * @return the double array with "X" and "Y" components if the model covered this position or null if not
   */
  public Pair< Double, Double > getWindAsVector( final GM_Point location );
  /**
   * Get the wind data as speed and direction in degrees provided by this model for the specified location [(x,y) position].
   * 
   * @param location
   *            the location for which an wind data is to be computed
   * @return the double array with "Speed" and "Direction" components if the model covered this position or null if not
   */
  
  public Pair< Double, Double > getWindAsSpeedAndDirection( final GM_Point location );

  /**
   * To get the bounding box of this wind data provider
   * 
   * @return the bounding box as {@link GM_Envelope}
   */
  public GM_Envelope getBoundingBox( );
 
  /**
   * @return {@link IGeoGrid} of {@link Pair} with Double values representing wind as vector. If the data is unstructured null will be returned. 
   */
//  public Pair[][] getDataAsGrid();
  public IGeoGrid getDataAsGrid();
  
  /**
   * @return {@link RectifiedGridDomain} object with grid description
   */
  public RectifiedGridDomain getGridDescriptor() throws Exception;

}