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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFENetItem;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IFeatureChangeCommand;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
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
  public DeleteCalculationUnitCmd( final IFEDiscretisationModel1d2d model1d2d, final ICalculationUnit calcUnit )
  {
    m_model1d2d = model1d2d;
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

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    try
    {
//      if( m_undoParentUnits != null && m_undoParentUnits.length > 0 )
//      {
//        final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
//        final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd.1" ); //$NON-NLS-1$
//        final MessageDialog dialog = new MessageDialog( activeShell, "Info", null, message, MessageDialog.INFORMATION, new String[] { "Ok" }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$
//        dialog.open();
//        return;
//      }

      // // delete links to parent units
      // for( final ICalculationUnit1D2D parentUnit : m_undoParentUnits )
      // parentUnit.getSubUnits().removeAllRefs( m_calcUnitToDelete );

      // delete links to child units
      if( m_calcUnitToDelete instanceof ICalculationUnit1D2D )
        ((ICalculationUnit1D2D)m_calcUnitToDelete).getSubCalculationUnits().clear();

      // delete links to elements
      for( final IFENetItem element : m_calcUnitToDelete.getElements() )
      {
        m_calcUnitToDelete.removeLinkedItem( element );
      }
      deleteControlModel( m_calcUnitToDelete.getId() );

      // delete unit from the model
      m_model1d2d.removeComplexElement( m_calcUnitToDelete );
      final Feature[] changedFeatureArray = new Feature[] { m_calcUnitToDelete };
      m_calcUnitToDelete = null;
      fireProcessChanges( changedFeatureArray, false );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }

  private void deleteControlModel( final String calcUnitToDeleteGmlID )
  {
    final IScenarioDataProvider szenarioDataProvider = KalypsoAFGUIFrameworkPlugin.getDataProvider();
    IControlModelGroup modelGroup = null;
    try
    {
      modelGroup = szenarioDataProvider.getModel( IControlModelGroup.class.getName() );
    }
    catch( final CoreException e )
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd.4" ), e ); //$NON-NLS-1$
    }
    final IControlModel1D2DCollection controlModel1D2DCollection = modelGroup.getModel1D2DCollection();
    IControlModel1D2D controlModel1D2D = null;

    final IControlModel1D2D activeControlModel = controlModel1D2DCollection.getActiveControlModel();
    IControlModel1D2D controlModelToActivate = null;
    boolean invalidActiveModel = activeControlModel == null || activeControlModel.getCalculationUnit() == null || activeControlModel.getCalculationUnit().getId().equals( calcUnitToDeleteGmlID );
    for( final IControlModel1D2D controlModel : controlModel1D2DCollection.getControlModels() )
    {
      final ICalculationUnit cmCalcUnit = controlModel.getCalculationUnit();
      if( cmCalcUnit != null )
      {
        if( calcUnitToDeleteGmlID.equals( cmCalcUnit.getId() ) )
        {
          controlModel1D2D = controlModel;
          if( !invalidActiveModel )
            break;
        }
        else if( invalidActiveModel )
        {
          controlModelToActivate = controlModel;
          controlModel1D2DCollection.setActiveControlModel( controlModelToActivate );
          invalidActiveModel = false;
        }
      }
      else if( controlModel1D2D == null )
      {
        controlModel1D2D = controlModel;

        // control model doesn't exists, actually we have control model without reference to any existing calculation
        // unit
        // so this one is invalid and should be deleted. In this case it is a needed control model
      }
    }

    final Feature parentFeature = controlModel1D2DCollection;
    final IFeatureType parentFT = parentFeature.getFeatureType();

    final IPropertyType propType = parentFT.getProperty( ControlModel1D2DCollection.WB1D2DCONTROL_PROP_CONTROL_MODEL_MEMBER );
    if( !(propType instanceof IRelationType) )
      return;
    final CommandableWorkspace cmdWorkspace = new CommandableWorkspace( controlModel1D2D.getWorkspace() );
    final DeleteFeatureCommand delControlCmd = new DeleteFeatureCommand( controlModel1D2D );
    try
    {
      cmdWorkspace.postCommand( delControlCmd );
      ((ICommandPoster)szenarioDataProvider).postCommand( IControlModelGroup.class.getName(), new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @param calculationUnit
   *          the added or removed calculation unit
   * @param added
   *          true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( final Feature[] changedFeatures, final boolean added )
  {
    final int changedType;
    if( added )
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD;
    else
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;
    final GMLWorkspace workspace = m_model1d2d.getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_model1d2d, changedFeatures, changedType );
    workspace.fireModellEvent( event );
  }

  @Override
  public void redo( ) throws Exception
  {
    process();
  }

  @Override
  public void undo( ) throws Exception
  {
  }
}
