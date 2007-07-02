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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards;

import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.internal.UIPlugin;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.ControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModel1D2DCollection;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IControlModelGroup;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainModel;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.om.ObservationFeatureFactory;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * @author Madanagopal
 * 
 */
public class CreateCalculationUnitWizard extends Wizard implements INewWizard
{
  private CreateCalculationUnitWizardPage firstPage;

  private IStructuredSelection initialSelection;

  private KeyBasedDataModel dataModel;

  private static final String QNAME_KEY_1D2D = "1D/2D";

  private static final String QNAME_KEY_2D = "2D";

  private static final String QNAME_KEY_1D = "1D";

  private ICalculationUnit createdCalculationUnit;

  public CreateCalculationUnitWizard( KeyBasedDataModel dataModel )
  {
    this.dataModel = dataModel;
  }

  public void addPages( )
  {

    firstPage = new CreateCalculationUnitWizardPage( "windowTitle", dataModel );
    addPage( firstPage );
    firstPage.init( initialSelection );
  }

  @Override
  public boolean performFinish( )
  {
    final String name = firstPage.getNameField();
    final String qNameKey = firstPage.getTypeCombo();
    final String desc = firstPage.getDescriptionText();
    if( name.trim().equals( "" ) )
    {
      firstPage.setMessage( null );
      firstPage.setErrorMessage( "Provide a Name" );
      firstPage.setPageComplete( false );
      return false;
    }

    if( qNameKey.trim().equals( "" ) )
    {
      firstPage.setMessage( null );
      firstPage.setErrorMessage( "Select the Calculation Unit Type" );
      firstPage.setPageComplete( false );
      return false;
    }

    CreateCalculationUnitCmd cmd = new CreateCalculationUnitCmd( getCUnitQName( qNameKey ), Util.getModel( IFEDiscretisationModel1d2d.class ), name, desc )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.CreateCalculationUnitCmd#process()
       */
      @SuppressWarnings( { "unchecked", "synthetic-access" })
      @Override
      public void process( ) throws Exception
      {
        super.process();
        // create control model for this unit
        try
        {
          createControlModel( getCreatedCalculationUnit() );
        }
        catch (Exception e) {
          e.printStackTrace();
        }
          // reset list of calculation units
          IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
          List<ICalculationUnit> calUnits = CalUnitOps.getModelCalculationUnits( model1d2d );
          dataModel.setData( ICommonKeys.KEY_FEATURE_WRAPPER_LIST, calUnits );
  
          // set the create unit as selected
          dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, getCreatedCalculationUnit() );
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, cmd );

    this.createdCalculationUnit = cmd.getCreatedCalculationUnit();
    return true;
  }

  void createControlModel( final ICalculationUnit createdCalculationUnit2 )
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
    catch( CoreException e )
    {
      throw new RuntimeException( "Error while creating control model for calculation unit. ", e );
    }
//    IControlModelGroup modelGroup = Util.getModel( IControlModelGroup.class );
//    if(modelGroup == null) {
//      final CommandableWorkspace workspace = getModelWorkspace( IControlModelGroup.class );
//      return (T) workspace.getRootFeature().getAdapter( modelClass );
//    }
    final IControlModel1D2DCollection model1D2DCollection = modelGroup.getModel1D2DCollection();
    final Feature parentFeature = model1D2DCollection.getWrappedFeature();// getFeature();
//    final Object property = parentFeature.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONTROL_MODEL_MEMBER );
//    if( property instanceof XLinkedFeature_Impl )
//    {
//      final Feature f = ((XLinkedFeature_Impl) property).getFeature();
      final IRelationType relationType = (IRelationType) parentFeature.getFeatureType().getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_CONTROL_MODEL_MEMBER );//ParentRelation();
      final CommandableWorkspace commandableWorkspace = new CommandableWorkspace( parentFeature.getWorkspace() );
      final int pos = 0;
      final IGMLSchema schema = parentFeature.getFeatureType().getGMLSchema();
      final IFeatureType controlModelFeatureType = 
        schema.getFeatureType( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_F_MODEL );
      final AddFeatureCommand command = 
            new AddFeatureCommand( 
                commandableWorkspace, 
                controlModelFeatureType, 
                parentFeature, 
                relationType, 
                pos, null, null, -1 )
      {
        /**
         * @see org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand#process()
         */
        @Override
        public void process( ) throws Exception
        {
          super.process();
          final Feature newControlFeature = getNewFeature();
          IControlModel1D2D newControlModel = (IControlModel1D2D) newControlFeature.getAdapter( IControlModel1D2D.class );
          String calUnitName = createdCalculationUnit2.getName();
          if( calUnitName == null )
          {
            calUnitName = createdCalculationUnit2.getGmlID();
          }
          
          newControlModel.setName( "Control modell für " + calUnitName );
          newControlModel.setCalculationUnit( createdCalculationUnit2 );
          model1D2DCollection.setActiveControlModel(newControlModel);
          
          
          
          
          final Feature obsFeature = (Feature) newControlFeature.getProperty( Kalypso1D2DSchemaConstants.WB1D2DCONTROL_PROP_TIMESTEPS_MEMBER );

          /* If observation does not exist, create it.
          final Feature obsFeature;
          if( obsFeatureIfPresent == null )
          {
            final Feature feature = getFeature();
            final GMLWorkspace workspace = feature.getWorkspace();
            final IRelationType parentRelation = (IRelationType) feature.getFeatureType().getProperty( QNAME_P_OBSERVATION );
            obsFeature = workspace.createFeature( feature, parentRelation, parentRelation.getTargetFeatureType(), -1 );
            feature.setProperty( QNAME_P_OBSERVATION, obsFeature );
          }
          else
            obsFeature = getObservationFeature();
          */
          /* Create an observation from it. */
          final IObservation<TupleResult> obs = ObservationFeatureFactory.toObservation( obsFeature );
          final TupleResult result = obs.getResult();
          /* If not yet initialized, create components and write obs back to feature. */
          if( result.getComponents().length == 0 )
          {
            obs.setName( "Zeitschritt Definition" );
            
            // TODO put this inside c1d2d:TimestepsObservation
            /**
             <om:observedProperty xmlns:om="http://www.opengis.net/om" xlink:href="urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D"/>

            
            
            IPhenomenon phenomenon = new DictionaryPhenomenon("urn:ogc:gml:dict:kalypso:model:1d2d:timeserie:phenomenons#TimeserieBorderCondition1D", "", "");
            obs.setPhenomenon( phenomenon );
            */
            
            final String[] componentUrns = new String[] { 
                Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, 
                Kalypso1D2DDictConstants.DICT_COMPONENT_UNDER_RELAXATION_FACTOR
                };
            final IComponent[] components = new IComponent[componentUrns.length];
  
            for( int i = 0; i < components.length; i++ )
              components[i] = ObservationFeatureFactory.createDictionaryComponent( obsFeature, componentUrns[i] );
  
            for( final IComponent component : components )
              result.addComponent( component );
  
            result.setSortComponents( new IComponent[] {components[0]} );
            
            ObservationFeatureFactory.toFeature( obs, obsFeature );
          }
        }
      };
      try
      {
        commandableWorkspace.postCommand( command );
      }
      catch( Exception e1 )
      {
        e1.printStackTrace();
        throw new RuntimeException( "Error while creating control for cal unit", e1 );
      }
//    }

  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection selection )
  {
    initialSelection = selection;
  }

  private static final QName getCUnitQName( String qNameKey ) throws RuntimeException
  {
    if( QNAME_KEY_1D.equals( qNameKey ) )
    {
      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D;
    }
    else if( QNAME_KEY_2D.equals( qNameKey ) )
    {

      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_2D;
    }
    else if( QNAME_KEY_1D2D.equals( qNameKey ) )
    {
      return Kalypso1D2DSchemaConstants.WB1D2D_F_CALC_UNIT_1D2D;
    }
    else
    {
      throw new RuntimeException( "Unknown qNameKey:" + qNameKey );
    }
  }
}
