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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.model.ICommandPoster;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsosimulationmodel.core.discr.IFENetItem;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * Command to delete calculation unit
 * 
 * @author Patrice Congo
 * 
 */
public class DeleteCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  private final IFEDiscretisationModel1d2d m_model1d2d;

  private ICalculationUnit m_calcUnitToDelete;

  /**
   * the QName of the deleted calculation unit
   */
  private QName m_undoQName;

  /**
   * the parent/container units of the deleted calculation unit
   */
  private ICalculationUnit1D2D[] m_undoParentUnits;

  /**
   * the child units of the deleted calculation unit
   */
  private ICalculationUnit[] m_undoChildUnits;

  /**
   * the elements of the deleted calculation unit
   */
  private IFENetItem[] m_undoElements;

  /**
   * the name the deleted calculation unit
   */
  private String m_undoName;

  /**
   * the description of the deleted calculation unit
   */
  private String m_undoDesc;

  private final boolean m_calcUnitDeleted = false;

  /**
   * Deletes the calculation unit
   * 
   * @param cuFeatureQName
   *            the q-name of the calculation unit to create
   * @param model1d2d
   *            the model that should hold the new calculation unit
   * @param name
   *            a name for the calculation unit if one has to be set or null
   * @param description
   *            text describing the calculation unit or null
   * @throws IllegalArgumentException
   *             if cuFeatureQName or model1d2d is null
   */
  public DeleteCalculationUnitCmd( final IFEDiscretisationModel1d2d model1d2d, final ICalculationUnit calcUnit )
  {
    m_model1d2d = model1d2d;
    m_calcUnitToDelete = calcUnit;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( m_calcUnitDeleted )
      return new IFeatureWrapper2[] { m_model1d2d };
    else
      return new IFeatureWrapper2[] {};
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getDiscretisationModel1d2d()
   */
  @Override
  public IFEDiscretisationModel1d2d getDiscretisationModel1d2d( )
  {
    return m_model1d2d;
  }

  /**
   * @see org.kalypso.commons.command.ICommand#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return "Command for deleting calculation unit"; //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
  @Override
  public boolean isUndoable( )
  {
    return true;
  }

  private final void cacheState( )
  {
    m_undoName = m_calcUnitToDelete.getName();
    m_undoDesc = m_calcUnitToDelete.getDescription();
    if( m_calcUnitToDelete instanceof ICalculationUnit1D2D )
    {
      final IFeatureWrapperCollection<ICalculationUnit> subUnits = ((ICalculationUnit1D2D) m_calcUnitToDelete).getChangedSubUnits();
      m_undoChildUnits = subUnits.toArray( new ICalculationUnit[0] );
    }
    final Collection<ICalculationUnit1D2D> parentUnits = CalcUnitOps.getParentUnit( m_calcUnitToDelete, m_model1d2d );
    m_undoParentUnits = parentUnits.toArray( new ICalculationUnit1D2D[0] );
    m_undoQName = m_calcUnitToDelete.getFeature().getFeatureType().getQName();
    final IFeatureWrapperCollection<IFENetItem> elements = m_calcUnitToDelete.getElements();
    m_undoElements = elements.toArray( new IFENetItem[] {} );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#process()
   */
  @Override
  public void process( ) throws Exception
  {
    try
    {

      // cache state for undo
      cacheState();

      if( m_undoParentUnits != null && m_undoParentUnits.length > 0 )
      {
        final Shell activeShell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
        final String message = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd.1" ); //$NON-NLS-1$
        final MessageDialog dialog = new MessageDialog( activeShell, "Info", null, message, MessageDialog.INFORMATION, new String[] { "Ok" }, 0 ); //$NON-NLS-1$ //$NON-NLS-2$
        dialog.open();
        return;
      }

      // // delete links to parent units
      // for( final ICalculationUnit1D2D parentUnit : m_undoParentUnits )
      // parentUnit.getSubUnits().removeAllRefs( m_calcUnitToDelete );

      // delete links to child units
      if( m_calcUnitToDelete instanceof ICalculationUnit1D2D )
        ((ICalculationUnit1D2D) m_calcUnitToDelete).getChangedSubUnits().clear();

      // delete links to elements
      for( final IFeatureWrapper2 element : m_undoElements )
      {
        if( element == null )
          continue;
        if( element instanceof IFE1D2DElement )
          ((IFE1D2DElement) element).getContainers().remove( m_calcUnitToDelete );
        else if( element instanceof IFELine )
          ((IFELine) element).getContainers().remove( m_calcUnitToDelete );
      }
      m_calcUnitToDelete.getElements().clear();

      // delete unit from the model
      m_model1d2d.getComplexElements().remove( m_calcUnitToDelete );
      deleteControlModel( m_calcUnitToDelete.getGmlID() );
      m_calcUnitToDelete = null;
      final Feature[] changedFeatureArray = getChangedFeatureArray();
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
    final IWorkbench workbench = PlatformUI.getWorkbench();
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    final ICaseDataProvider<IFeatureWrapper2> szenarioDataProvider = (ICaseDataProvider<IFeatureWrapper2>) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
    IControlModelGroup modelGroup = null;
    try
    {
      modelGroup = szenarioDataProvider.getModel( IControlModelGroup.class );
    }
    catch( final CoreException e )
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.DeleteCalculationUnitCmd.4" ), e ); //$NON-NLS-1$
    }
    final IControlModel1D2DCollection controlModel1D2DCollection = modelGroup.getModel1D2DCollection();
    IControlModel1D2D controlModel1D2D = null;

    final IControlModel1D2D activeControlModel = controlModel1D2DCollection.getActiveControlModel();
    IControlModel1D2D controlModelToActivate = null;
    final boolean invalidActiveModel = activeControlModel == null || activeControlModel.getCalculationUnit() == null
        || activeControlModel.getCalculationUnit().getGmlID().equals( calcUnitToDeleteGmlID );
    for( final IControlModel1D2D controlModel : controlModel1D2DCollection )
    {
      final ICalculationUnit cmCalcUnit = controlModel.getCalculationUnit();
      if( cmCalcUnit != null )
      {
        if( calcUnitToDeleteGmlID.equals( cmCalcUnit.getGmlID() ) )
          controlModel1D2D = controlModel;
        else if( invalidActiveModel )
          controlModelToActivate = controlModel;
      }
    }
    if( invalidActiveModel && controlModelToActivate != null )
      controlModel1D2DCollection.setActiveControlModel( controlModelToActivate );

    if( controlModel1D2D == null )
      // throw new RuntimeException( "Cannot find control model for the calculation unit." );
      // control model doesn't exists, so nothing will happen
      return;

    final Feature parentFeature = controlModel1D2DCollection.getFeature();
    final IFeatureType parentFT = parentFeature.getFeatureType();

    final IPropertyType propType = parentFT.getProperty( ControlModel1D2DCollection.WB1D2DCONTROL_PROP_CONTROL_MODEL_MEMBER );
    if( !(propType instanceof IRelationType) )
      return;
    final CommandableWorkspace cmdWorkspace = new CommandableWorkspace( controlModel1D2D.getFeature().getWorkspace() );
    final DeleteFeatureCommand delControlCmd = new DeleteFeatureCommand( controlModel1D2D.getFeature() );
    try
    {
      cmdWorkspace.postCommand( delControlCmd );
      ((ICommandPoster) szenarioDataProvider).postCommand( IControlModelGroup.class, new EmptyCommand( "Get dirty!", false ) ); //$NON-NLS-1$
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private final Feature[] getChangedFeatureArray( )
  {
    final List<Feature> changedFeatures = new ArrayList<Feature>();

    if( m_calcUnitToDelete != null )
      changedFeatures.add( m_calcUnitToDelete.getFeature() );

    for( final IFeatureWrapper2 element : m_undoElements )
    {
      if( element == null )
        continue;
      changedFeatures.add( element.getFeature() );
    }

    // parent units
    for( final ICalculationUnit1D2D parent : m_undoParentUnits )
      changedFeatures.add( parent.getFeature() );

    // child unit not needed since there is no back reference

    return changedFeatures.toArray( new Feature[changedFeatures.size()] );
  }

  /**
   * 
   * @param calculationUnit
   *            the added or removed calculation unit
   * @param added
   *            true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( final Feature[] changedFeatures, final boolean added )
  {
    final int changedType;
    if( added )
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD;
    else
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;
    final GMLWorkspace workspace = m_model1d2d.getFeature().getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_model1d2d.getFeature(), changedFeatures, changedType );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( !m_calcUnitDeleted )
      process();
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    // create row c-unit
    m_calcUnitToDelete = m_model1d2d.getComplexElements().addNew( m_undoQName, ICalculationUnit.class );

    if( m_undoName != null )
      m_calcUnitToDelete.setName( m_undoName );

    if( m_undoDesc != null )
      m_calcUnitToDelete.setDescription( m_undoDesc );

    // set elements
    for( final IFeatureWrapper2 element : m_undoElements )
    {
      m_calcUnitToDelete.addElementAsRef( (IFENetItem) element );
      if( element instanceof IFE1D2DElement )
        ((IFE1D2DElement) element).getContainers().addRef( m_calcUnitToDelete );
    }

    // set subunits
    if( m_calcUnitToDelete instanceof ICalculationUnit1D2D )
    {
      final IFeatureWrapperCollection subUnits = ((ICalculationUnit1D2D) m_calcUnitToDelete).getChangedSubUnits();
      for( final ICalculationUnit subUnit : m_undoChildUnits )
        subUnits.addRef( subUnit );
    }

    // set parent units
    for( final ICalculationUnit1D2D parentUnit : m_undoParentUnits )
      parentUnit.getChangedSubUnits().addRef( m_calcUnitToDelete );

    final Feature[] changedFeatureArray = getChangedFeatureArray();
    fireProcessChanges( changedFeatureArray, true );
  }

}
