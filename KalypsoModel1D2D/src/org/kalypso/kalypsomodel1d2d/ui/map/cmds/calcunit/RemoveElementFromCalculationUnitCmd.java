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

import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.ops.LinksOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Madanagopal
 */
public class RemoveElementFromCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  private final IFE1D2DElement[] elementsToRemove;

  private final ICalculationUnit calculationUnit;

  private final IFEDiscretisationModel1d2d model1d2d;

  public RemoveElementFromCalculationUnitCmd( ICalculationUnit calculationUnit, Feature[] elementsToRemove, IFEDiscretisationModel1d2d model1d2d )
  {
    this.calculationUnit = calculationUnit;
    this.elementsToRemove = CalcUnitOps.toAddableElements( calculationUnit, elementsToRemove );
    this.model1d2d = model1d2d;
  }

  public RemoveElementFromCalculationUnitCmd( ICalculationUnit calculationUnit, IFE1D2DElement[] elementsToRemove, IFEDiscretisationModel1d2d model1d2d )
  {
    this.calculationUnit = calculationUnit;
    this.elementsToRemove = CalcUnitOps.toAddableElements( calculationUnit, elementsToRemove );
    this.model1d2d = model1d2d;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return model1d2d;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return null;
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

    for( IFE1D2DElement element : elementsToRemove )
    {
      LinksOps.delRelationshipElementAndComplexElement( element, calculationUnit );
    }
    fireProcessChanges();
  }

  private final void fireProcessChanges( )
  {
    List<Feature> features = new ArrayList<Feature>( elementsToRemove.length * 2 );
    features.add( calculationUnit.getFeature() );
    for( IFE1D2DElement ele : elementsToRemove )
    {
      features.remove( ele.getFeature() );
    }

    GMLWorkspace workspace = calculationUnit.getFeature().getWorkspace();
    FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace,// final GMLWorkspace
    // workspace,
    model1d2d.getFeature(),// Feature parentFeature,
    features.toArray( new Feature[features.size()] ),// final Feature[] changedFeature,
    FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE// final int changeType
    );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {

  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {

  }
}
