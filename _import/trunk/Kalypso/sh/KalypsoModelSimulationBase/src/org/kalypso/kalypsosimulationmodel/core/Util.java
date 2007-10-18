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
package org.kalypso.kalypsosimulationmodel.core;

import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.commons.command.ICommand;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.simulation.core.simspec.Modeldata;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.FeatureWrapperCollection;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapperCollection;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.renew.workflow.connector.cases.ICaseDataProvider;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * TODO: most of the methods should be moved into {@link FeatureHelper}.
 * 
 * Holds utility methods
 * 
 * @author Patrice Congo
 * 
 */
public class Util
{

  /**
   * Gets the scenario folder
   */
  public static final IFolder getScenarioFolder( )
  {
    try
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();

      final IFolder scenarioFolder = (IFolder) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

      // / scenario
      return scenarioFolder;
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  /**
   * Gets the szenario model
   */
  @SuppressWarnings("unchecked")
  public static final ICaseDataProvider<IModel> getScenarioDataProvider( )
  {
    try
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final ICaseDataProvider<IModel> caseDataProvider = (ICaseDataProvider<IModel>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      return caseDataProvider;
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  @SuppressWarnings("unchecked")
  public static final CommandableWorkspace getCommandableWorkspace( final Class< ? extends IModel> modelClass )
  {
    try
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final ICaseDataProvider<IFeatureWrapper2> caseDataProvider = (ICaseDataProvider<IFeatureWrapper2>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      if( caseDataProvider instanceof ICommandPoster )
      {
        CommandableWorkspace commandableWorkSpace = ((ICommandPoster) caseDataProvider).getCommandableWorkSpace( modelClass );
        return commandableWorkSpace;
      }
      else
      {
        throw new RuntimeException( Messages.getString( "Util.0" ) ); //$NON-NLS-1$
      }

    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  @SuppressWarnings("unchecked")
  public static final void postCommand( final Class< ? extends IModel> modelClass, ICommand command )
  {
    try
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final ICaseDataProvider<IModel> caseDataProvider = (ICaseDataProvider<IModel>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      if( caseDataProvider instanceof ICommandPoster )
      {
        ((ICommandPoster) caseDataProvider).postCommand( modelClass, command );
      }
      else
      {
        throw new RuntimeException( Messages.getString( "Util.1" ) ); //$NON-NLS-1$
      }

    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  /**
   * Gets the szenario model
   */
  @SuppressWarnings("unchecked")
  public static final <T extends IModel> T getModel( final Class<T> modelClass )
  {
    try
    {
      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final ICaseDataProvider<IModel> caseDataProvider = (ICaseDataProvider<IModel>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final T model = caseDataProvider.getModel( modelClass );

      return model;
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  public static final void addModelInputSpec( final Modeldata modelSpec, final String id, final Class< ? extends IModel> modelClass )
  {
    final List<Modeldata.Input> input = modelSpec.getInput();
    final Modeldata.Input controlModelInput = new Modeldata.Input();
    controlModelInput.setPath( Util.getWorkspaceSpec( modelClass ) );
    controlModelInput.setId( id );
    controlModelInput.setRelativeToCalcCase( false );
    input.add( controlModelInput );
  }

  public static final String getWorkspaceSpec( final Class< ? extends IModel> modelClass )
  {
    try
    {
      final IFeatureWrapper2 model = getModel( modelClass );
      final URL context = model.getWrappedFeature().getWorkspace().getContext();

      // String path2 = context.getPath();
      // IResource resource = new Path(path2);

      final URL resolvedUrl = FileLocator.resolve( context );
      return resolvedUrl.getFile();
    }
    catch( final Exception e )
    {
      throw new RuntimeException( Messages.getString( "Util.2" ) + modelClass, e ); //$NON-NLS-1$
    }
  }

  /**
   * Saves all dirty submodels in the current scenario. A workbench and an active workbench window is required.
   * 
   */
  @SuppressWarnings("unchecked")
  public static final void saveAllModel( )
  {
    try
    {

      final IWorkbench workbench = PlatformUI.getWorkbench();
      final IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
      final IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      final IEvaluationContext currentState = service.getCurrentState();
      final ICaseDataProvider<IModel> caseDataProvider = (ICaseDataProvider<IModel>) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_DATA_PROVIDER_NAME );
      final IRunnableWithProgress rwp = new IRunnableWithProgress()
      {

        public void run( IProgressMonitor monitor ) throws InvocationTargetException
        {
          try
          {
            caseDataProvider.saveModel( null );
          }
          catch( CoreException e )
          {
            e.printStackTrace();
            throw new InvocationTargetException( e );
          }
        }

      };

      activeWorkbenchWindow.run( true, false, rwp );
    }
    catch( final Throwable th )
    {
      th.printStackTrace();
      throw new RuntimeException( th );
    }
  }

  /**
   * Test whether the given feature is an elmenent of the type specified by the q-name.
   * 
   * @param feature
   *            the feature instance, which type is to be access
   * @param typeQname --
   *            the required qname for the feature type
   * @return true if qname of the given feature match the one passed otherwise false
   */
  public static final boolean directInstanceOf( final Feature feature, final QName typeQname )
  {
    if( feature == null || typeQname == null )
    {
      throw new IllegalArgumentException( Messages.getString( "Util.3" ) + "\tfeature=" + feature + "\ttypeQname=" + typeQname ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }

    return typeQname.equals( feature.getFeatureType().getQName() );
  }

  /**
   * Create a feature of the given type and link it to the given parentFeature as a property of the specified q-name
   * 
   * @param parentFeature
   *            the parent feature
   * @param propQName
   *            the q-name of the property linking the parent and the newly created child
   * @param featureQName
   *            the q-name denoting the type of the feature
   */
  public static final Feature createFeatureAsProperty( final Feature parentFeature, final QName propQName, final QName featureQName ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( propQName, Messages.getString( "Util.15" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( parentFeature, Messages.getString( "Util.16" ) ); //$NON-NLS-1$

    try
    {
      final IPropertyType property = parentFeature.getFeatureType().getProperty( propQName );
      if( property.isList() )
      {
        final Feature feature = FeatureHelper.addFeature( parentFeature, propQName, featureQName );

        return feature;
      }
      else
      {
        final GMLWorkspace workspace = parentFeature.getWorkspace();
        final IFeatureType newFeatureType = workspace.getGMLSchema().getFeatureType( featureQName );
        final Feature feature = workspace.createFeature( parentFeature, (IRelationType) property, newFeatureType );
        parentFeature.setProperty( property, feature );
        return feature;
      }

    }
    catch( final GMLSchemaException ex )
    {
      throw new IllegalArgumentException( Messages.getString( "Util.17" ) + propQName + Messages.getString( "Util.18" ) + featureQName, ex ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  public static final Feature createFeatureAsProperty( final Feature parentFeature, final QName propQName, final QName featureQName, final Object[] featureProperties, final QName[] featurePropQNames ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( propQName, Messages.getString( "Util.19" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( parentFeature, Messages.getString( "Util.20" ) ); //$NON-NLS-1$

    try
    {
      final IPropertyType property = parentFeature.getFeatureType().getProperty( propQName );
      if( property.isList() )
      {
        final Feature feature = FeatureHelper.addFeature( parentFeature, propQName, featureQName, featureProperties, featurePropQNames );

        return feature;
      }
      else
      {
        final GMLWorkspace workspace = parentFeature.getWorkspace();
        final IFeatureType newFeatureType = workspace.getGMLSchema().getFeatureType( featureQName );
        final Feature feature = workspace.createFeature( parentFeature, (IRelationType) property, newFeatureType );
        for( int i = featureProperties.length - 1; i >= 0; i-- )
        {
          feature.setProperty( featurePropQNames[i], featureProperties[i] );
        }

        parentFeature.setProperty( property, feature );
        return feature;
      }

    }
    catch( final GMLSchemaException ex )
    {
      throw new IllegalArgumentException( Messages.getString( "Util.21" ) + propQName + Messages.getString( "Util.22" ) + featureQName, ex ); //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  /**
   * TODO complete and test this method
   */

  /**
   * Create a feature of the given type and link it to the given parentFeature as a property of the specified q-name
   * 
   * @param parentFeature
   *            the parent feature
   * @param propQName
   *            the q-name of the property linking the parent and the newly created child
   * @param featureQName
   *            the q-name denoting the type of the feature
   * @param throws
   *            {@link IllegalArgumentException} if parentFeature is null or propQName is null, or featureQName is null
   *            or featureID is null or empty or there is a feature in the workspace with the same id
   */
  public static final Feature createFeatureAsProperty( final Feature parentFeature, final QName propQName, final QName featureQName, String featureID ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( propQName, Messages.getString( "Util.23" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( parentFeature, Messages.getString( "Util.24" ) ); //$NON-NLS-1$
    featureID = Assert.throwIAEOnNullOrEmpty( featureID );

    // try
    // {
    final IGMLSchema schema = parentFeature.getFeatureType().getGMLSchema();
    final IFeatureType featureType = schema.getFeatureType( featureQName );
    final IPropertyType propertyType = featureType.getProperty( propQName );
    if( !(propertyType instanceof IRelationType) )
    {
      throw new RuntimeException( Messages.getString( "Util.25" ) ); //$NON-NLS-1$
    }
    return FeatureFactory.createFeature( parentFeature, (IRelationType) propertyType,// parentRelation,
    featureID, featureType, true,// initializeWithDefaults,
    1// depth
    );
    // return FeatureHelper.addFeature(
    // parentFeature,
    // propQName,
    // featureQName);
    // }
    // catch(GMLSchemaException ex)
    // {
    // throw new IllegalArgumentException(
    // "Property "+propQName+
    // " does not accept element of type"+
    // featureQName,
    // ex);
    // }
  }

  /**
   * Get an {@link IFeatureWrapperCollection} from a feature list property. The feature type, the property type and the
   * type of the collection elements can be return
   * 
   * @param feature
   *            the feature whose property is to be wrapped in a {@link IFeatureWrapperCollection}
   * @param listPropQName
   *            the Q Name of the property
   * @param bindingInterface
   *            the class of the collection elements
   * @param doCreate
   *            a boolean controling the handling of the property creation. if true a listProperty is created if its not
   *            allready availayble
   * 
   * 
   */
  public static final <T extends IFeatureWrapper2> IFeatureWrapperCollection<T> get( final Feature feature, final QName featureQName, final QName listPropQName, final Class<T> bindingInterface, final boolean doCreate )
  {
    Assert.throwIAEOnNull( feature, Messages.getString( "Util.26" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( featureQName, Messages.getString( "Util.27" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( listPropQName, Messages.getString( "Util.28" ) ); //$NON-NLS-1$
    Assert.throwIAEOnNull( bindingInterface, Messages.getString( "Util.29" ) ); //$NON-NLS-1$

    final Object prop = feature.getProperty( listPropQName );

    FeatureWrapperCollection<T> col = null;

    if( prop == null )
    {
      // create the property tha is still missing
      if( doCreate )
      {
        col = new FeatureWrapperCollection<T>( feature, featureQName, listPropQName, bindingInterface );
      }
    }
    else
    {
      // just wrapped the existing one
      col = new FeatureWrapperCollection<T>( feature, bindingInterface, listPropQName );
    }

    return col;
  }
}
