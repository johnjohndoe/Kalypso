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
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import org.eclipse.core.runtime.FileLocator;
import org.kalypso.kalypsosimulationmodel.core.mpcoverage.TerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelSimulationBaseConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * Provide the implementaion of {@link ITerrainElevationModel} for 
 * simBase:NativeTerrainElevationModelWrapper model.
 * This class colaboratewith ... 
 * 
 * @author Madanagopal
 * @author Patrice Congo
 *
 */
public class NativeTerrainElevationModelWrapper extends TerrainElevationModel
{

  private IElevationProvider elevationProvider;
  private URL sourceURL;
  
  public NativeTerrainElevationModelWrapper( Feature featureToBind ) throws IllegalArgumentException, IOException, URISyntaxException
  {
    super( 
        featureToBind,
        KalypsoModelSimulationBaseConsts.SIM_BASE_F_NATIVE_TERRAIN_ELE_WRAPPER );
   String sourceName=
     (String)featureToBind.getProperty(KalypsoModelSimulationBaseConsts.SIM_BASE_PROP_FILE_NAME);
   if(sourceName==null)
   {
     throw new IllegalArgumentException("No native file property set");
   }
   sourceURL=makeSourceURL( sourceName, featureToBind.getWorkspace().getContext() );
   File nativeTerrainModelFile= new File(FileLocator.resolve( sourceURL).getFile());
   elevationProvider = NativeTerrainElevationModelFactory.getTerrainElevationModel(nativeTerrainModelFile);   
  }
  
  private final URL makeSourceURL(String sourceName, URL worspaceContex) throws URISyntaxException, MalformedURLException
  {    
    try
    {
      URL sourceUrl= new URL(sourceName);
      URI uri=sourceUrl.toURI();
      if(uri.isAbsolute())
      {
        return uri.toURL();
      }
      else
      {
        URL absURL=new URL(worspaceContex,sourceName);
        return absURL;
      }
    }
    catch( MalformedURLException e )
    {
//      e.printStackTrace();
      return new URL(worspaceContex,sourceName);
    }
  }
  /**
   * Return the file property of the terrain elevation model
   */
  private final File getFile()
  {    
    return null;
  }
  
  /**
   * @see org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel#getElevation(org.kalypsodeegree.model.geometry.GM_Point)
   */
  public double getElevation( GM_Point location )
  {
    return elevationProvider.getElevation( location );
  }

  
}
