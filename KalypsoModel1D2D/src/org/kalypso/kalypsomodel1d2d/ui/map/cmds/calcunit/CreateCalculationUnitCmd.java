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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.afgui.model.Util;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DComplexElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * Command to create new calculation unit
 *
 * @author Patrice Congo
 *
 */
public class CreateCalculationUnitCmd implements IDiscrModel1d2dChangeCommand
{
  /**
   * QName of the calculation unit to create
   */
  private QName m_calcUnitFeatureQName = null;

  /**
   * the created calculation unit
   */
  private ICalculationUnit m_calculationUnit;

  /**
   * the discretisation model holding the calculation unit
   */
  private final IFEDiscretisationModel1d2d m_model1d2d;

  /**
   * the name the calculation unit will be assigned to
   */
  private final String m_calcUnitName;

  /**
   * the description for the calculation unit
   */
  private final String m_calcUnitDescription;

  /**
   * Creates a Calculation unit of the given q-name
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
  public CreateCalculationUnitCmd( final QName calcUnitFeatureQName, final IFEDiscretisationModel1d2d model1d2d, final String name, final String decription )
  {
    m_calcUnitFeatureQName = calcUnitFeatureQName;
    m_model1d2d = model1d2d;
    m_calcUnitName = name;
    m_calcUnitDescription = decription;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand#getChangedFeature()
   */
  @Override
  public IFeatureWrapper2[] getChangedFeature( )
  {
    if( m_calculationUnit != null )
      return new IFeatureWrapper2[] { m_model1d2d, m_calculationUnit };
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
    return Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd.0" ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.commons.command.ICommand#isUndoable()
   */
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
      final IFeatureWrapperCollection<IFE1D2DComplexElement> ce = m_model1d2d.getComplexElements();
      m_calculationUnit = ce.addNew( m_calcUnitFeatureQName, ICalculationUnit.class );
      if( m_calcUnitName != null )
        m_calculationUnit.setName( m_calcUnitName );
      if( m_calcUnitDescription != null )
        m_calculationUnit.setDescription( m_calcUnitDescription );
      m_calculationUnit.getElements().clear();
      createControlModel();
      fireProcessChanges( m_calculationUnit, true );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw e;
    }

  }

  /**
   *
   * @param calculationUnit
   *            the added or removed calculation unit
   * @param added
   *            true if the calculation unit was added false otherwise
   */
  private final void fireProcessChanges( final ICalculationUnit calculationUnit, final boolean added )
  {
    final int changedType;
    if( added )
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD;
    else
      changedType = FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_DELETE;
    final GMLWorkspace workspace = calculationUnit.getFeature().getWorkspace();
    final FeatureStructureChangeModellEvent event = new FeatureStructureChangeModellEvent( workspace, m_model1d2d.getFeature(), new Feature[] { calculationUnit.getFeature() }, changedType );
    workspace.fireModellEvent( event );
  }

  /**
   * @see org.kalypso.commons.command.ICommand#redo()
   */
  @Override
  public void redo( ) throws Exception
  {
    if( m_calculationUnit != null )
    {
      process();
    }
  }

  /**
   * @see org.kalypso.commons.command.ICommand#undo()
   */
  @Override
  public void undo( ) throws Exception
  {
    final IFeatureWrapperCollection<IFE1D2DComplexElement> ce = m_model1d2d.getComplexElements();
    ce.remove( m_calculationUnit );
    final ICalculationUnit deletedCreatedCU = m_calculationUnit;
    m_calculationUnit = null;
    fireProcessChanges( deletedCreatedCU, true );

  }

  public ICalculationUnit getCreatedCalculationUnit( )
  {
    return m_calculationUnit;
  }

  private void createControlModel( )
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
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd.1" ), e ); //$NON-NLS-1$
    }
    final IControlModel1D2DCollection model1D2DCollection = modelGroup.getModel1D2DCollection();
    final Feature parentFeature = model1D2DCollection.getFeature();
    final IRelationType relationType = (IRelationType) parentFeature.getFeatureType().getProperty( ControlModel1D2DCollection.WB1D2DCONTROL_PROP_CONTROL_MODEL_MEMBER );
    final CommandableWorkspace commandableWorkspace = Util.getCommandableWorkspace( IControlModelGroup.class );
    final int pos = 0;
    final IGMLSchema schema = parentFeature.getFeatureType().getGMLSchema();
    final IFeatureType controlModelFeatureType = schema.getFeatureType( ControlModel1D2D.WB1D2DCONTROL_F_MODEL );
    final AddFeatureCommand command = new AddFeatureCommand( commandableWorkspace, controlModelFeatureType, parentFeature, relationType, pos, null, null, -1 )
    {
      /**
       * @see org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand#process()
       */
      @Override
      public void process( ) throws Exception
      {
        super.process();
        final Feature newControlFeature = getNewFeature();
        final IControlModel1D2D newControlModel = (IControlModel1D2D) newControlFeature.getAdapter( IControlModel1D2D.class );

        // newControlModel.setName( Messages.getString( "CreateCalculationUnitCmd.2" ) + m_calcUnitName ); //$NON-NLS-1$
        newControlModel.setCalculationUnit( m_calculationUnit );
        model1D2DCollection.setActiveControlModel( newControlModel );

        final Feature obsFeature = (Feature) newControlFeature.getProperty( ControlModel1D2D.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );

        /*
         * If observation does not exist, create it. final Feature obsFeature; if( obsFeatureIfPresent == null ) { final
         * Feature feature = getFeature(); final GMLWorkspace workspace = feature.getWorkspace(); final IRelationType
         * parentRelation = (IRelationType) feature.getFeatureType().getProperty( QNAME_P_OBSERVATION ); obsFeature =
         * workspace.createFeature( feature, parentRelation, parentRelation.getTargetFeatureType(), -1 );
         * feature.setProperty( QNAME_P_OBSERVATION, obsFeature ); } else obsFeature = getObservationFeature();
         */
        /* Create an observation from it. */
        final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
        final TupleResult result = obs.getResult();
        /* If not yet initialized, create components and write obs back to feature. */
        if( result.getComponents().length == 0 )
        {
          obs.setName( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd.3" ) ); //$NON-NLS-1$

          // TODO put this inside c1d2d:TimestepsObservation
          /**
           * <om:observedProperty xmlns:om="http://www.opengis.net/om"
           * xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>
           *
           *
           *
           * IPhenomenon phenomenon = new
           * DictionaryPhenomenon("urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D",
           * "", ""); obs.setPhenomenon( phenomenon );
           */

          final String[] componentUrns = new String[] { Kalypso1D2DDictConstants.DICT_COMPONENT_ORDINAL_NUMBER, Kalypso1D2DDictConstants.DICT_COMPONENT_TIME,
              Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR };
          final IComponent[] components = new IComponent[componentUrns.length];

          for( int i = 0; i < components.length; i++ )
            components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );

          for( final IComponent component : components )
            result.addComponent( component );

          result.setSortComponents( new IComponent[] { components[1] } );
          result.setOrdinalNumberComponent( components[0] );

          ObservationFeatureFactory.toFeature( obs, obsFeature );
        }
      }
    };
    try
    {
      commandableWorkspace.postCommand( command );
    }
    catch( final Exception e1 )
    {
      e1.printStackTrace();
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd.4" ), e1 ); //$NON-NLS-1$
    }
  }
}
