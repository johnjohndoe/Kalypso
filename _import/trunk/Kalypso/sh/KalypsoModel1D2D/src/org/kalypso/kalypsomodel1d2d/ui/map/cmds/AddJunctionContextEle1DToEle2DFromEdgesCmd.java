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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds;


import org.kalypso.kalypsomodel1d2d.ops.JunctionContextOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IJunctionContext1DTo2D;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Add junction context from 1D and 2D edge
 * 
 * 
 * @author Patrice Congo
 */
public class AddJunctionContextEle1DToEle2DFromEdgesCmd implements IDiscrModel1d2dChangeCommand
{
  private IFE1D2DEdge edge1D;
  
  private IFE1D2DEdge edge2D;
  
  private IFEDiscretisationModel1d2d model;
  
  
  private IJunctionContext1DTo2D addedJunction;
  
  /**
   * @param model
   * @param elementEdgeCmds an array the command used to create the edges of the element to be created
   *  by this command. the array must contains only {@link AddEdgeCommand} and 
   *    {@link AddEdgeInvCommand} commands
   */
  public AddJunctionContextEle1DToEle2DFromEdgesCmd(
              IFEDiscretisationModel1d2d model,
              IFE1D2DEdge edge1D,
              IFE1D2DEdge edge2D
              )
  {
    
    Assert.throwIAEOnNullParam( model, "model" );
    Assert.throwIAEOnNullParam( edge1D, "edge1D" );
    Assert.throwIAEOnNullParam( edge2D, "edge2D" );
    this.model = model;
    this.edge2D = edge2D;
    this.edge1D = edge1D;
    
  }
  
  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    return "Add Junction from Element 1D and Continuity Line Selection";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    //TODO implement undo using delete command
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    if(addedJunction==null)
    {
      try
      {
        addedJunction = 
          JunctionContextOps.createEdgeToEdgeJunctionContext( 
                                                model, edge1D, edge2D );
      }
      catch( Exception e )
      {
        e.printStackTrace();
        throw e;
      }
    }
  }

 

  
  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if(addedJunction==null)
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if(addedJunction!=null)
    {
      //TODO remove element and links to it edges
    }
  }

  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    final IFE1D2DContinuityLine continuityLine = 
                            addedJunction.getContinuityLine();    
    return new IFeatureWrapper2[]{addedJunction, continuityLine};
  }
  
  /**
   * @see xp.IDiscrMode1d2dlChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model;
  }
}
