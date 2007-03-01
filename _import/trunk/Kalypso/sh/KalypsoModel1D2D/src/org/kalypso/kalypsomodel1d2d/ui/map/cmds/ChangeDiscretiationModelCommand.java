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

import java.util.ArrayList;
import java.util.List;

import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Composite command used to change the discretisation command.
 * This composite takes the responsibility to nodifies the 
 * commandable workspace about the chnage introduced by its
 * sub command 
 * 
 * 
 * @author Patrice Congo
 *
 */
public class ChangeDiscretiationModelCommand implements ICommand
{
  public static final String DEFAULT_DESCRIPTION=
                            "Change Discretisation model";
  
  private String description;
  
  private IFEDiscretisationModel1d2d model1d2d;
  
  private CommandableWorkspace commandableWorkspace;
  
  private List<IDiscrModel1d2dChangeCommand> commands = 
                      new ArrayList<IDiscrModel1d2dChangeCommand>();
  private boolean isUndoable=true;


  
  public ChangeDiscretiationModelCommand( 
                          CommandableWorkspace commandableWorkspace,   
                          IFEDiscretisationModel1d2d model1d2d)
  {
    this(   
        commandableWorkspace,
        model1d2d,  
        DEFAULT_DESCRIPTION );
  }
  
  public ChangeDiscretiationModelCommand(
                      CommandableWorkspace commandableWorkspace,
                      IFEDiscretisationModel1d2d model1d2d,
                      String description)
  {
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    Assert.throwIAEOnNullParam( description, "description" );
    this.description=description;
    this.model1d2d=model1d2d;
    this.commandableWorkspace=commandableWorkspace;
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
    return isUndoable;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    List<Feature> changedFeatures= new ArrayList<Feature>();
    for(IDiscrModel1d2dChangeCommand command:commands)
    {
      try
      {
        command.process();
        for(IFeatureWrapper changedFeature :command.getChangedFeature())
        {
          if(changedFeature!=null)
          {
            Feature wrappedFeature=changedFeature.getWrappedFeature();
            if(wrappedFeature!=null)
            {
              changedFeatures.add( wrappedFeature );
              wrappedFeature.invalidEnvelope();
            }
          }
        }
      }
      catch (Exception e) 
      {
        e.printStackTrace();
      }      
    }
    
    model1d2d.getEdges().getWrappedList().invalidate();
    model1d2d.getElements().getWrappedList().invalidate();
    model1d2d.getNodes().getWrappedList().invalidate();
    fireStructureChange( changedFeatures );    
  }

  private final void fireStructureChange(List<Feature> changedFeatures)
  {
    Feature[] changedFeaturesArray=new Feature[changedFeatures.size()];
    changedFeatures.toArray( changedFeaturesArray );
    commandableWorkspace.fireModellEvent( 
        new FeatureStructureChangeModellEvent( 
                          commandableWorkspace, 
                          model1d2d.getWrappedFeature(), 
                          changedFeaturesArray, 
                          FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
  }
  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    for(IDiscrModel1d2dChangeCommand command:commands)
    {
      try
      {
        command.redo();
      }
      catch (Throwable th) 
      {
        th.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    
//  reverse order  is taken because of eventual dependencies
    IDiscrModel1d2dChangeCommand command;
    for(int index=commands.size()-1;index>=0;index-- )
    {
      command=commands.get( index );
      try
      {
        command.undo();
      }
      catch (Exception e) 
      {
        e.printStackTrace();
      }
    }
    
  }
  
  public void addCommand(IDiscrModel1d2dChangeCommand command)
  {
    Assert.throwIAEOnNullParam( command, "command" );
    commands.add( command );
    
    isUndoable=isUndoable && command.isUndoable();
    
  }
}
