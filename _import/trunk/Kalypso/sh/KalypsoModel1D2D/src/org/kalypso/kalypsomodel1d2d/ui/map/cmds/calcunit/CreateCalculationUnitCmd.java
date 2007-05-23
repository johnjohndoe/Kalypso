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

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;



/**
 * Command to create a new Calculation unit
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings("unchecked")
public class CreateCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  /**
   *QName of the calculation unit to create 
   */
  private QName cuFeatureQName = null;
  
  /**
   * the created calculation unit
   */
  private ICalculationUnit createdCU;
  
  /**
   * the discretisation model holding the calculation unit
   */
  private IFEDiscretisationModel1d2d model1d2d;
  
  /**
   * the name the calculation unit will be assigned to
   */
  private String name;
  
  /**
   * the description for the calculation unit
   */
  private String description;
  
  /**
   * Creates a Calculation unit of the given q-name
   * @param cuFeatureQName the q-name of the calculation unit to create
   * @param model1d2d the model that should hold the new calculation unit
   * @param name a name for the calculation unit if one has to be set or null
   * @param description text describing the calculation unit or null
   * @throws IllegalArgumentException if cuFeatureQName or model1d2d is null
   */
  @SuppressWarnings("hiding")
  public CreateCalculationUnitCmd(
              QName cuFeatureQName, 
              IFEDiscretisationModel1d2d model1d2d,
              String name, 
              String decription)
  {
    Assert.throwIAEOnNullParam( cuFeatureQName, "cuFeatureQName" );
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    this.cuFeatureQName = cuFeatureQName;
    this.model1d2d = model1d2d;
    this.name = name;
    this.description = decription;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( createdCU!=null )
    {
      return new IFeatureWrapper2[]{model1d2d, createdCU };
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
    return "Neues Berechnungseinheit kreieren";
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  public boolean isUndoable( )
  {
    return true;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    try
    {
      IFeatureWrapperCollection<IFE1D2DComplexElement> ce = 
                                          model1d2d.getComplexElements();
      createdCU = ce.addNew( cuFeatureQName, ICalculationUnit.class );
      if( name != null )
      {
        createdCU.setName( name );
      }
      
      if( description != null )
      {
        createdCU.setDescription( description );
      }
      fireProcessChanges( createdCU, true );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }
  
  /**
   * 
   * @param calculationUnit the added or removed calculation unit
   * @param added true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( 
                            ICalculationUnit calculationUnit, 
                            boolean added )
  {
    final int changedType;
    if( added ) 
    {
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD; 
    }
    else
    {
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;
    }
    GMLWorkspace workspace = calculationUnit.getWrappedFeature().getWorkspace();
    FeatureStructureChangeModellEvent event = 
        new FeatureStructureChangeModellEvent(
            workspace,//final GMLWorkspace workspace, 
            model1d2d.getWrappedFeature(),// Feature parentFeature, 
            new Feature[]{calculationUnit.getWrappedFeature()},//final Feature[] changedFeature, 
            changedType//final int changeType
            );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if( createdCU != null )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    IFeatureWrapperCollection<IFE1D2DComplexElement> ce = 
                                  model1d2d.getComplexElements();
    ce.remove( createdCU );
    final ICalculationUnit deletedCreatedCU = createdCU;
    createdCU = null;
    fireProcessChanges( deletedCreatedCU, true );
    
  }

}
