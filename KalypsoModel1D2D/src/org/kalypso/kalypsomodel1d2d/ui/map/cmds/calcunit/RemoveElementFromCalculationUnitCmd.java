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
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * @author Madanagopal
 */
public class RemoveElementFromCalculationUnitCmd implements IFeatureChangeCommand
{
  private final IFE1D2DElement[] m_elementsToRemove;

  private final ICalculationUnit m_calculationUnit;

  private final IFEDiscretisationModel1d2d m_model1d2d;

  public RemoveElementFromCalculationUnitCmd( final ICalculationUnit calculationUnit, final Feature[] elementsToRemove, final IFEDiscretisationModel1d2d model1d2d )
  {
    m_calculationUnit = calculationUnit;
    m_elementsToRemove = CalcUnitOps.toAddableElements( calculationUnit, elementsToRemove );
    m_model1d2d = model1d2d;
  }

  public RemoveElementFromCalculationUnitCmd( final ICalculationUnit calculationUnit, final IFE1D2DElement[] elementsToRemove, final IFEDiscretisationModel1d2d model1d2d )
  {
    m_calculationUnit = calculationUnit;
    m_elementsToRemove = CalcUnitOps.toAddableElements( calculationUnit, elementsToRemove );
    m_model1d2d = model1d2d;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return null;
  }

  @Override
  public String getDescription( )
  {
    return null;
  }

  @Override
  public boolean isUndoable( )
  {
    return false;
  }

  @Override
  public void process( ) throws Exception
  {
    for( final IFE1D2DElement element : m_elementsToRemove )
    {
      LinksOps.delRelationshipElementAndComplexElement( element, m_calculationUnit );
    }
    fireProcessChanges();
  }

  private final void fireProcessChanges( )
  {
    final List<Feature> features = new ArrayList<>( m_elementsToRemove.length * 2 );
    features.add( m_calculationUnit );
    for( final IFE1D2DElement ele : m_elementsToRemove )
    {
      features.remove( ele );
    }

    final GMLWorkspace workspace = m_calculationUnit.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace,// final GMLWorkspace
    // workspace,
    m_model1d2d,// Feature parentFeature,
    features.toArray( new Feature[features.size()] ),// final Feature[] changedFeature,
    FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE// final int changeType
    );
    workspace.fireModellEvent( event );
  }

  @Override
  public void redo( ) throws Exception
  {

  }

  @Override
  public void undo( ) throws Exception
  {

  }
}
