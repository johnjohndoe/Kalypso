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
package org.kalypso.ui.wizards.imports.elevationmodel;

import org.kalypso.commons.command.ICommand;

/**
 * Currently just a dummy command use to get the pool
 * dirty.
 * TODO Patrice implemts this command 
 * @author Patrice Congo
 *
 */
public class AddTerrainelevationModelCmd implements ICommand
{

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return "Terrain Elevation Model hinzufügen "; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
//    ITerrainElevationModel tem =
//      new NativeTerrainElevationModelWrapper(temSys,nativeTEMRelPath);
//
//    //TODO introduce in the first page a name imput field and gets the
//    //name from there
//    
//    String name=dstFileTif.getName();
//    tem.setName( name );
//    System.out.println("Workspace:"+workspace.getClass());
//    workspace.fireModellEvent( 
//       new FeatureStructureChangeModellEvent( 
//             workspace, 
//             temSys.getWrappedFeature(),
//             tem.getWrappedFeature(), 
//             FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );


  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    // TODO Auto-generated method stub

  }

}
