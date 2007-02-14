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
package org.kalypso.kalypsosimulationmodel.core.terrainmodel;

import java.io.File;

import org.deegree.model.geometry.GM_Envelope;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * An elevation provider base on ASC file
 * 
 * @author Madanagopal
 * @author Patrice Congo 
 *
 */
public class ASCTerrainElevationModel implements IElevationProvider
{
  /**
   * the ascFile file containing the elevation info 
   */
  private File ascFile;
  
  //TODO check using polygons
  /**
   * The envelop if the reagion of interest
   */
  private GM_Envelope regionOfInterest;
  
  /**
   * Create an elevation provider based on the given asc file, in the specified 
   * region of interest. if the regionInterest is null the file elevation 
   * information is computed and therefore available
   * @param ascFile the asc file containing the native terrain model
   * @param regionOfInterest the {@link GM_Envelope} of region of interest
   * @throws IllegalArgumentException if asc file is null or is a directory or does not exist
   *        or is not accesible (cannot be read)
   * 
   */
  public ASCTerrainElevationModel(
              File ascFile,
              GM_Envelope regionOfInterest)
              throws IllegalArgumentException
  {
    Assert.throwIAEOnNulOrIsDirOrNotExistsOrNotReadable( ascFile );
//    Assert.throwIAEOnNullParam( regionOfInterest, "regionOfInterest" );
    this.regionOfInterest=regionOfInterest;
    this.ascFile=ascFile;
  }
  
  
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.IElevationProvider#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    //TODO implement me
    return 0;
  }

}
