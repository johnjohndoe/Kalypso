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
package org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit;

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

import org.kalypso.contribs.java.lang.MultiException;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Command to add element to calculation unit
 * 
 * @author Patrice Congo
 *
 */
@SuppressWarnings("unchecked")
public class AddElementToCalculationUnit implements IDiscrModel1d2dChangeCommand
{

  private final IFE1D2DElement[] elementsToAdd ;
  
  private boolean added = false;
  
  private final ICalculationUnit calculationUnit;

  private final IFEDiscretisationModel1d2d model1d2d;
  
  @SuppressWarnings("hiding")
  public AddElementToCalculationUnit(
                      ICalculationUnit1D calculationUnit,
                      IElement1D[] elementsToAdd,
                      IFEDiscretisationModel1d2d model1d2d )
  {
    this.calculationUnit = calculationUnit;
    this.elementsToAdd = elementsToAdd;
    this.model1d2d = model1d2d;
  }
  
  public AddElementToCalculationUnit(
      ICalculationUnit calculationUnit,
      IFE1D2DElement[] elementsToAdd,
      IFEDiscretisationModel1d2d model1d2d )
  {
    this.calculationUnit = calculationUnit;
    this.elementsToAdd = elementsToAdd;
    this.model1d2d = model1d2d;
  }
  public AddElementToCalculationUnit(
      ICalculationUnit2D<IElement2D> calculationUnit,
      IElement2D[] elementsToAdd,
      IFEDiscretisationModel1d2d model1d2d )
  {
    this.calculationUnit = calculationUnit;
    this.elementsToAdd = elementsToAdd;
    this.model1d2d = model1d2d;
  }
  
  public AddElementToCalculationUnit(
      ICalculationUnit1D2D calculationUnit,
      IFE1D2DElement[] elementsToAdd,
      IFEDiscretisationModel1d2d model1d2d )
  {
    this.calculationUnit = calculationUnit;
    this.elementsToAdd = elementsToAdd;
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
      changed.addAll( Arrays.asList( elementsToAdd ) );
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
    return "Elemente einer Berechnungseinheit hinzufügen";
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
    if(!added )
    {
      try
      {
        for( IFE1D2DElement ele : elementsToAdd )
        {
          ele.getContainers().add( calculationUnit );
          calculationUnit.addElementAsRef( ele );
        }
        
        added = true;
        //fire change
        fireProcessChanges();
      }
      catch( Exception th )
      {
        for( IFE1D2DElement ele : elementsToAdd )
        {
          try
          {
            ele.getContainers().add( calculationUnit );
          }
          catch( Throwable e)
          {
            
          }
          try
          {
            calculationUnit.addElementAsRef( ele );
          }
          catch ( Throwable e ) 
          {
            
          }
        }
        th.printStackTrace();
        throw th;
      }
      
    }
  }
  private final void fireProcessChanges()
  {
    List<Feature> features = new ArrayList<Feature>( elementsToAdd.length * 2 );
    features.add( calculationUnit.getWrappedFeature() );
    for( IFE1D2DElement ele: elementsToAdd )
    {
      features.add( ele.getWrappedFeature() );
    }
    
    GMLWorkspace workspace = calculationUnit.getWrappedFeature().getWorkspace();
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
    if( !added )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  public void undo( ) throws Exception
  {
    if( added )
    {
     
      MultiException multiException = null;
      for( IFE1D2DElement ele : elementsToAdd )
      {
        try
        {
          ele.getContainers().add( calculationUnit );
        }
        catch( Exception e)
        {
          e.printStackTrace();
          if( multiException == null )
          {
            multiException = new MultiException();
          }
          multiException.addException( e );
        }
        try
        {
          calculationUnit.addElementAsRef( ele );
        }
        catch ( Exception e ) 
        {
          if( multiException == null )
          {
            multiException = new MultiException();
          }
          multiException.addException( e );
        }
      }
      if( multiException != null )
      {
        throw multiException;
      }
      else
      {
        added = false;
        fireProcessChanges();
      }
    }
  }

}
