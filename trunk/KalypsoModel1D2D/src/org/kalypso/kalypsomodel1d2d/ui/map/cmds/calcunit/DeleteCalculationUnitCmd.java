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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;

import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFENetItem;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.modeling.IControlModel;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.IScenarioDataProvider;

/**
 * Command to delete calculation unit
 * 
 * @author Patrice Congo
 */
public class DeleteCalculationUnitCmd implements IFeatureChangeCommand
{
  private final IFEDiscretisationModel1d2d m_model1d2d;

  private ICalculationUnit m_calcUnitToDelete;

  private final IControlModelGroup m_modelGroup;

  /**
   * Deletes the calculation unit
   * 
   * @param cuFeatureQName
   *          the q-name of the calculation unit to create
   * @param model1d2d
   *          the model that should hold the new calculation unit
   * @param name
   *          a name for the calculation unit if one has to be set or null
   * @param description
   *          text describing the calculation unit or null
   * @throws IllegalArgumentException
   *           if cuFeatureQName or model1d2d is null
   */
  public DeleteCalculationUnitCmd( final IFEDiscretisationModel1d2d model1d2d, final IControlModelGroup modelGroup, final ICalculationUnit calcUnit )
  {
    m_model1d2d = model1d2d;
    m_modelGroup = modelGroup;
    m_calcUnitToDelete = calcUnit;
  }

  @Override
  public Feature[] getChangedFeatures( )
  {
    return new Feature[] { m_model1d2d };
  }

  @Override
  public String getDescription( )
  {
    return "Command for deleting calculation unit"; //$NON-NLS-1$
  }

  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  @Override
  public void process( ) throws Exception
  {
    final Collection<Feature> changedFeatures = new ArrayList<>();

    final boolean isLinkedTo = checkIsLinkedTo( m_model1d2d, m_calcUnitToDelete );
    if( isLinkedTo )
      throw new IllegalStateException( "Cannot delete calculation unit that is still referenced elsewhere" ); //$NON-NLS-1$

    // delete links to child units
    if( m_calcUnitToDelete instanceof ICalculationUnit1D2D )
      ((ICalculationUnit1D2D)m_calcUnitToDelete).getSubCalculationUnits().clear();

    // delete links to elements
    for( final IFENetItem element : m_calcUnitToDelete.getElements() )
      m_calcUnitToDelete.removeLinkedItem( element );

    /* delete control model, if present */
    deleteControlModel( m_calcUnitToDelete );

    // delete unit from the model
    m_model1d2d.removeComplexElement( m_calcUnitToDelete );
    changedFeatures.add( m_calcUnitToDelete );

    /* fire events */
    final GMLWorkspace workspace = m_model1d2d.getWorkspace();

    final Feature[] allChanged = changedFeatures.toArray( new Feature[changedFeatures.size()] );

    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_model1d2d, allChanged, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE );
    workspace.fireModellEvent( event );

    m_calcUnitToDelete = null;
  }

  public static boolean checkIsLinkedTo( final IFEDiscretisationModel1d2d model1d2d, final ICalculationUnit calcUnitToDelete )
  {
    final IFE1D2DComplexElement<IFENetItem>[] complexElements = model1d2d.getComplexElements();
    for( final IFE1D2DComplexElement<IFENetItem> complexElement : complexElements )
    {
      if( complexElement instanceof ICalculationUnit1D2D )
      {
        final IFeatureBindingCollection<ICalculationUnit> subCalculationUnits = ((ICalculationUnit1D2D)complexElement).getSubCalculationUnits();
        for( final ICalculationUnit subUnit : subCalculationUnits )
        {
          if( subUnit == calcUnitToDelete )
            return true;
        }
      }
    }

    return false;
  }

  private void deleteControlModel( final ICalculationUnit calcUnitToDelete ) throws InvocationTargetException
  {
    final IControlModel1D2DCollection controlModels = m_modelGroup.getModel1D2DCollection();

    final IControlModel controlModelToDelete = controlModels.findControlModel( calcUnitToDelete );
    if( controlModelToDelete == null )
    {
      /* nothing to do */
      return;
    }

    /* determine the new active control model to be set */
    final IControlModel1D2D newActiveControlModel = determinNewActiveControlModel( controlModels, controlModelToDelete );

    /* delete control model if it was present */
    final DeleteFeatureCommand delControlCmd = new DeleteFeatureCommand( controlModelToDelete );
    final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    szenarioDataProvider.postCommand( IControlModelGroup.class.getName(), delControlCmd );

    /* reset active control model, if it was the deleted one */
    controlModels.setActiveControlModel( newActiveControlModel );
  }

  private IControlModel1D2D determinNewActiveControlModel( final IControlModel1D2DCollection controlModels, final IControlModel controlModelToDelete )
  {
    final IControlModel1D2D oldActiveControlModel = controlModels.getActiveControlModel();
    if( oldActiveControlModel != controlModelToDelete )
      return oldActiveControlModel;

    /* activate first one that is not the deleted one */
    final IFeatureBindingCollection<IControlModel1D2D> allControlModels = controlModels.getControlModels();
    for( final IControlModel1D2D controlModel : allControlModels )
    {
      if( controlModel != controlModelToDelete )
        return controlModel;
    }

    return null;
  }

  @Override
  public void redo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }

  @Override
  public void undo( ) throws Exception
  {
    throw new UnsupportedOperationException();
  }
}
