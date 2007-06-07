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
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
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
public class DeleteCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  /**
   * the discretisation model holding the calculation unit
   */
  private IFEDiscretisationModel1d2d model1d2d;
  
  /**
   * the calculation unit to delete
   */
  private ICalculationUnit cuToDel;
  
  /** 
   * the QName of the deleted calculation unit
   */
  private QName oldQName;
  
//  /**
//   * the upstream boundary line of the deleted calculation unit
//   */
//  private IBoundaryLine oldBLineUpStream;
//  
//  /**
//   * the downstream boundary line of the deleted calculation unit
//   */
//  private IBoundaryLine oldBLineDownStream;
  
  /**
   * the parent/container units of the deleted calculation unit
   */
  private ICalculationUnit1D2D[] oldParentUnits;
  
  /**
   * the child units of the deleted calculation unit
   */
  private ICalculationUnit[] oldChildUnits;
  
  /**
   * the elements of the deleted calculation unit
   */
  private IFE1D2DElement[] oldElements;
  
  /**
   * the name the deleted calculation unit
   */
  private String oldName;
  
  /**
   * the description of the deleted calculation unit
   */
  private String oldDesc;

  private boolean deleted = false;
  
  /**
   * Creates a Calculation unit of the given q-name
   * @param cuFeatureQName the q-name of the calculation unit to create
   * @param model1d2d the model that should hold the new calculation unit
   * @param name a name for the calculation unit if one has to be set or null
   * @param description text describing the calculation unit or null
   * @throws IllegalArgumentException if cuFeatureQName or model1d2d is null
   */
  @SuppressWarnings("hiding")
  public DeleteCalculationUnitCmd(
              IFEDiscretisationModel1d2d model1d2d,
              ICalculationUnit cuToDel )
  {
    Assert.throwIAEOnNullParam( cuToDel, "cuToDel" );
    Assert.throwIAEOnNullParam( model1d2d, "model1d2d" );
    this.model1d2d = model1d2d;
    this.cuToDel = cuToDel;
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( deleted )
    {
      
      return new IFeatureWrapper2[]{model1d2d };
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
  
  
  private final void cacheStateOfCuToDel()
  {
    oldName = cuToDel.getName();
    oldDesc = cuToDel.getDescription();
//    oldBLineDownStream = cuToDel.getDownStreamBoundaryLine();
//    oldBLineUpStream = cuToDel.getUpStreamBoundaryLine();
    if( cuToDel instanceof ICalculationUnit1D2D )
    {
      IFeatureWrapperCollection subUnits = 
        ((ICalculationUnit1D2D )cuToDel).getSubUnits();
      oldChildUnits =
        (ICalculationUnit[]) subUnits.toArray( new ICalculationUnit[0] );
    }
    
    final Collection<ICalculationUnit1D2D> parentUnits = 
                                CalUnitOps.getParentUnit( cuToDel, model1d2d );
    oldParentUnits = parentUnits.toArray( new ICalculationUnit1D2D[0] );
    oldQName = cuToDel.getWrappedFeature().getFeatureType().getQName();
    final IFeatureWrapperCollection elements = cuToDel.getElements();
    oldElements = (IFE1D2DElement[]) elements.toArray( new IFE1D2DElement[]{} );
    
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  public void process( ) throws Exception
  {
    try
    {
      //cache state for undo
      cacheStateOfCuToDel();
      
      //delete links to parent units
      for( ICalculationUnit1D2D parentUnit: oldParentUnits )
      {
        parentUnit.getSubUnits().removeAllRefs( cuToDel );
      }
      //delete links to child units
      if( cuToDel instanceof ICalculationUnit1D2D )
      {
        IFeatureWrapperCollection subUnits = 
          ((ICalculationUnit1D2D )cuToDel).getSubUnits();
        subUnits.clear();
      }
      //delete links to elements 
      for( IFE1D2DElement ele : oldElements )
      {
        ele.getContainers().remove( cuToDel );
      }
      cuToDel.getElements().clear();
      
      //delete unit from model
      model1d2d.getComplexElements().remove( cuToDel );
      ICalculationUnit cuToDeleted = cuToDel;
      cuToDel = null;
      final Feature[] changedFeatureArray = 
                        getChangedFeatureArray( cuToDeleted );
      
      fireProcessChanges( changedFeatureArray, false );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }
  
  private final Feature[] getChangedFeatureArray( ICalculationUnit deleted )
  {
    final List<Feature> changedFeatures = new ArrayList<Feature>( );
    
    final Feature[] changedFeaturesArray = 
        changedFeatures.toArray( new Feature[ changedFeatures.size( ) ] );
    //deleted
    if( deleted != null )
    {
      changedFeatures.add( deleted.getWrappedFeature() );
    }
    //cu elements
    for(IFE1D2DElement ele : oldElements )
    {
      changedFeatures.add( ele.getWrappedFeature() );
    }
    
    //parent units
    for( ICalculationUnit1D2D parent: oldParentUnits )
    {
      changedFeatures.add( parent.getWrappedFeature() );
    }
    
    // child unit not needed since there is no back reference
    
    return changedFeatures.toArray( new Feature[changedFeatures.size()] );
  }
  
  /**
   * 
   * @param calculationUnit the added or removed calculation unit
   * @param added true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( 
                            final Feature[] changedFeatures, 
                            final boolean added )
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
    GMLWorkspace workspace = model1d2d.getWrappedFeature().getWorkspace();
    FeatureStructureChangeModellEvent event = 
        new FeatureStructureChangeModellEvent(
            workspace,//final GMLWorkspace workspace, 
            model1d2d.getWrappedFeature(),// Feature parentFeature, 
            changedFeatures,//final Feature[] changedFeature, 
            changedType//final int changeType
            );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  public void redo( ) throws Exception
  {
    if( !deleted )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    //create row c-unit
    cuToDel = 
      model1d2d.getComplexElements().addNew( oldQName, ICalculationUnit.class );
    
    //set name
    if( oldName != null )
    {
      cuToDel.setName( oldName );
    }
    
    //set description
    if( oldDesc != null )
    {
      cuToDel.setDescription( oldDesc );
    }
    
    //set elements
    for(IFE1D2DElement ele :oldElements )
    {
      cuToDel.addElementAsRef( ele );
      ele.getContainers().addRef( cuToDel );
    }
    
    //set subunits
    if( cuToDel instanceof ICalculationUnit1D2D )
    {
      IFeatureWrapperCollection subUnits = ((ICalculationUnit1D2D)cuToDel).getSubUnits();
      for( ICalculationUnit subUnit :oldChildUnits )
      {
        subUnits.addRef( subUnit );
      }
    }
    
    //set parent units
    for( ICalculationUnit1D2D parentUnit : oldParentUnits )
    {
      parentUnit.getSubUnits().addRef( cuToDel );
    }
    
    final Feature[] changedFeatureArray = getChangedFeatureArray( cuToDel );
    fireProcessChanges( changedFeatureArray, true );
  }

}
