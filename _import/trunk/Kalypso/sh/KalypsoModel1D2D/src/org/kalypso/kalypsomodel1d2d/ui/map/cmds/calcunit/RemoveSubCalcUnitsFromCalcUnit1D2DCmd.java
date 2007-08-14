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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit;

import java.util.ArrayList;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Madanagopal
 *
 */
public class RemoveSubCalcUnitsFromCalcUnit1D2DCmd implements IDiscrModel1d2dChangeCommand
{

  private List<ICalculationUnit> listCalculationUnits;
  //private IFE1D2DElement[] elementsToAdd;
  private IFEDiscretisationModel1d2d model1d2d;
    private boolean added = false;
    private ICalculationUnit1D2D parentCalculationUnit;

  public RemoveSubCalcUnitsFromCalcUnit1D2DCmd(
      List<ICalculationUnit> listCalculationUnits,
      ICalculationUnit1D2D parentCalculationUnit,
      IFEDiscretisationModel1d2d model1d2d )

  {    
      this.listCalculationUnits = listCalculationUnits;
      this.parentCalculationUnit = parentCalculationUnit;
      this.model1d2d = model1d2d;
    
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( added )
    {
      List<IFeatureWrapper2> changed = new ArrayList<IFeatureWrapper2>();
      changed.addAll(listCalculationUnits);
      changed.add( parentCalculationUnit);
      return changed.toArray( new IFeatureWrapper2[changed.size()] );
    }
    else
    {
      return new IFeatureWrapper2[]{};
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model1d2d;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  public String getDescription( )
  {
    
    return null;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    
    return false;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @SuppressWarnings({ "unchecked", "unchecked" })
  public void process( ) throws Exception
  {
    IFeatureWrapperCollection subUnits = parentCalculationUnit.getSubUnits();
       if(!added )
    {
      try
      {
        for( ICalculationUnit ele : listCalculationUnits )
        {
          subUnits.removeAllRefs(ele );
          
        }
        
        added = true;
        //fire change
        fireProcessChanges();
      }
      catch( Exception th )
      {        
        th.printStackTrace();
        throw th;
      }
      
    }

  }

  private void fireProcessChanges( )
  {    
    IFeatureWrapper2[] elementsToAdd = getChangedFeature();    
    List<Feature> features = new ArrayList<Feature>();   
    for( IFeatureWrapper2 ele: elementsToAdd )
    {
      features.add( ele.getWrappedFeature() );
    }
    
    GMLWorkspace workspace = parentCalculationUnit.getWrappedFeature().getWorkspace();
    FeatureStructureChangeModellEvent event = 
        new FeatureStructureChangeModellEvent(
            workspace,//final GMLWorkspace workspace, 
            model1d2d.getWrappedFeature(),// Feature parentFeature, 
            features.toArray( new Feature[features.size()] ),//final Feature[] changedFeature, 
            FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD//final int changeType
            );
    workspace.fireModellEvent( event );    
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    
  }

}
