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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.ele;

import java.io.File;
import java.net.URL;

import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.INativeTerrainElevationModelWrapper;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelFactory;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Command for deleting a native terrain elevation model wrapper
 * 
 * @author Patrice Congo
 *
 */
public class DeleteNativeTerrainElevationWrapper implements IFeatureChangeCommand
{
  public static final String DEFAULT_DESCRIPTION = "";
  
  private String description;

  private ITerrainElevationModelSystem terrainElevationModelSystem;

  private INativeTerrainElevationModelWrapper elevationModel ;

  private boolean deleteFile;
  
  private boolean done=false;

  public DeleteNativeTerrainElevationWrapper(
              ITerrainElevationModelSystem terrainElevationModelSystem,
              INativeTerrainElevationModelWrapper elevationModel,
              boolean deleteFile)
  {
    this.terrainElevationModelSystem = terrainElevationModelSystem;
    this.elevationModel = elevationModel;
    this.deleteFile = deleteFile; 
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if(done)
    {
      return new IFeatureWrapper2[]{elevationModel};
    }
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return description;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return !deleteFile;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    if(done)
    {
      return;
    }    
    boolean b = terrainElevationModelSystem.getTerrainElevationModels().remove( elevationModel.getWrappedFeature() );
    if(b)
    {
      URL sourceURL = elevationModel.getSourceURL();
      File file= new File(sourceURL.getFile());
      if(file.exists())
      {
        if(file.canWrite())
        {
          file.delete();
          done = true;
          NativeTerrainElevationModelFactory.removeFromCache( file );
        }
        else
        {
          System.out.println("Cannot write or delete file");
        }
      }
      else
      {
        System.out.println("File does not exits:"+file);
      }
    }
    else
    {
      System.out.println("could not delete elevation");
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(!done || deleteFile)
    {
      return;
    }
  }

}
