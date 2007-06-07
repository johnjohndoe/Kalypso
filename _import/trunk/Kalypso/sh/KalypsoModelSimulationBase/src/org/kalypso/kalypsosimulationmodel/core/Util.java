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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.namespace.QName;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.schema.KalypsoModelRoughnessConsts;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

import de.renew.workflow.base.ISzenarioSourceProvider;
import de.renew.workflow.cases.ICaseDataProvider;

/**
 * Holds utility methods
 * 
 * @author Patrice Congo
 *
 */
public class Util
{

  /**
   * Gets the szenario model
   */
  public static final <T extends IFeatureWrapper2> T getModel( Class<T> modelClass )
  {
    try
    {
      IWorkbench workbench = PlatformUI.getWorkbench();
      IHandlerService service = (IHandlerService) workbench.getService( IHandlerService.class );
      IEvaluationContext currentState = service.getCurrentState();
      ICaseDataProvider<IFeatureWrapper2> caseDataProvider = (ICaseDataProvider<IFeatureWrapper2>) currentState.getVariable( ISzenarioSourceProvider.ACTIVE_SZENARIO_DATA_PROVIDER_NAME );
      T model = caseDataProvider.getModel( modelClass );

      return model;
    }
    catch( Throwable th )
    {
      th.printStackTrace();
      return null;
    }
  }

  /**
   * Test whether the given feature is an elmenent of the type specified by the q-name.
   * 
   * @param feature
   *          the feature instance, which type is to be access
   * @param typeQname --
   *          the required qname for the feature type
   * @return true if qname of the given feature match the one passed otherwise false
   */
  public static final boolean directInstanceOf( Feature feature, QName typeQname )
  {
    if( feature == null || typeQname == null )
    {
      throw new IllegalArgumentException( "Argument feature and typeQname must not be null:" + "\tfeature=" + feature + "\ttypeQname=" + typeQname );
    }

    return typeQname.equals( feature.getFeatureType().getQName() );
  }

  public static Feature createFeatureForListProp( final FeatureList list, final QName listProperty, final QName newFeatureName ) throws GMLSchemaException
  {

    final Feature parentFeature = list.getParentFeature();
    final GMLWorkspace workspace = parentFeature.getWorkspace();

    final IRelationType parentRelation = list.getParentFeatureTypeProperty();
    final IFeatureType targetFeatureType = parentRelation.getTargetFeatureType();

    final IFeatureType newFeatureType;
    if( newFeatureName == null )
    {
      newFeatureType = targetFeatureType;
    }
    else
    {
      newFeatureType = workspace.getGMLSchema().getFeatureType( newFeatureName );
    }

    if( newFeatureName != null && !GMLSchemaUtilities.substitutes( newFeatureType, targetFeatureType.getQName() ) )
    {
      throw new GMLSchemaException( "Type of new feature (" + newFeatureName + ") does not substitutes target feature type of the list: " + targetFeatureType.getQName() );
    }

    final Feature newFeature = workspace.createFeature( parentFeature, parentRelation, newFeatureType );
    try
    {
      workspace.addFeatureAsComposition( parentFeature,// parent,
      parentRelation,// linkProperty,
      list.size(),// pos,
      newFeature );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    return newFeature;
  }

  public static final Feature createFeatureWithId( QName newFeatureQName, Feature parentFeature, QName propQName, String gmlID ) throws IllegalArgumentException
  {

    Assert.throwIAEOnNullParam( parentFeature, "parentFeature" );
    Assert.throwIAEOnNullParam( propQName, "propQName" );
    Assert.throwIAEOnNullParam( newFeatureQName, "newFeatureQName" );
    gmlID = Assert.throwIAEOnNullOrEmpty( gmlID );

    GMLWorkspace workspace = parentFeature.getWorkspace();
    IGMLSchema schema = workspace.getGMLSchema();
    IFeatureType featureType = schema.getFeatureType( newFeatureQName );
    IPropertyType parentPT = parentFeature.getFeatureType().getProperty( propQName );
    if( !(parentPT instanceof IRelationType) )
    {
      throw new IllegalArgumentException( "Property not a IRelationType=" + parentPT + " propQname=" + propQName );
    }

    // TOASK does not include the feature into any workspace

    Feature created = FeatureFactory.createFeature( parentFeature, (IRelationType) parentPT, gmlID, featureType, true );

    try
    {
      if( parentPT.isList() )
      {
        // workspace.addFeatureAsAggregation(
        // parentFeature,//srcFE,
        // (IRelationType)parentPT,//linkProperty,
        // -1,//pos,
        // gmlID//featureID
        // );

        // FeatureList propList=
        // (FeatureList)parentFeature.getProperty( parentPT );
        // propList.add( created );

        workspace.addFeatureAsComposition( parentFeature, (IRelationType) parentPT, -1, created );
      }
      else
      {
        // TODO test this case
        parentFeature.setProperty( parentPT, created );
      }
    }
    catch( Exception e )
    {
      throw new RuntimeException( "Could not add to the workspace", e );
    }

    return created;
  }

  public static final List<Feature> toFeatureList( Collection< ? extends IFeatureWrapper2> c )
  {
    List<Feature> fl = new ArrayList<Feature>();
    if( c != null )
    {
      Feature f;
      for( IFeatureWrapper2 fw : c )
      {
        f = fw.getWrappedFeature();
        if( f == null )
        {
          throw new IllegalArgumentException( "All feature wrapper must wrapp a non null feature:" + c );
        }
        fl.add( f );
      }
    }
    return fl;
  }

  /**
   * Create a feature of the given type and link it to the given parentFeature as a property of the specified q-name
   * 
   * @param parentFeature
   *          the parent feature
   * @param propQName
   *          the q-name of the property linking the parent and the newly created child
   * @param featureQName
   *          the q-name denoting the type of the feature
   */
  public static final Feature createFeatureAsProperty( Feature parentFeature, QName propQName, QName featureQName ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( propQName, "Argument propQName must not be null" );
    Assert.throwIAEOnNull( parentFeature, "Argument roughnessCollection must not be null" );

    try
    {
      IPropertyType property = parentFeature.getFeatureType().getProperty( propQName );
      if( property.isList() )
      {
        Feature feature = FeatureHelper.addFeature( parentFeature, propQName, featureQName );

        return feature;
      }
      else
      {
        GMLWorkspace workspace = parentFeature.getWorkspace();
        IFeatureType newFeatureType = workspace.getGMLSchema().getFeatureType( featureQName );
        Feature feature = workspace.createFeature( parentFeature, (IRelationType) property, newFeatureType );
        parentFeature.setProperty( property, feature );
        return feature;
      }

    }
    catch( GMLSchemaException ex )
    {
      throw new IllegalArgumentException( "Property " + propQName + " does not accept element of type" + featureQName, ex );
    }
  }

  public static final Feature createFeatureAsProperty( Feature parentFeature, QName propQName, QName featureQName, Object[] featureProperties, QName[] featurePropQNames ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( propQName, "Argument propQName must not be null" );
    Assert.throwIAEOnNull( parentFeature, "Argument roughnessCollection must not be null" );

    try
    {
      IPropertyType property = parentFeature.getFeatureType().getProperty( propQName );
      if( property.isList() )
      {
        Feature feature = FeatureHelper.addFeature( parentFeature, propQName, featureQName, featureProperties, featurePropQNames );

        return feature;
      }
      else
      {
        GMLWorkspace workspace = parentFeature.getWorkspace();
        IFeatureType newFeatureType = workspace.getGMLSchema().getFeatureType( featureQName );
        Feature feature = workspace.createFeature( parentFeature, (IRelationType) property, newFeatureType );
        for( int i = featureProperties.length - 1; i >= 0; i-- )
        {
          feature.setProperty( featurePropQNames[i], featureProperties[i] );
        }

        parentFeature.setProperty( property, feature );
        return feature;
      }

    }
    catch( GMLSchemaException ex )
    {
      throw new IllegalArgumentException( "Property " + propQName + " does not accept element of type" + featureQName, ex );
    }
  }

  /**
   * TODO complete and test this method
   */

  /**
   * Create a feature of the given type and link it to the given parentFeature as a property of the specified q-name
   * 
   * @param parentFeature
   *          the parent feature
   * @param propQName
   *          the q-name of the property linking the parent and the newly created child
   * @param featureQName
   *          the q-name denoting the type of the feature
   * @param throws
   *          {@link IllegalArgumentException} if parentFeature is null or propQName is null, or featureQName is null or
   *          featureID is null or empty or there is a feature in the workspace with the same id
   */
  public static final Feature createFeatureAsProperty( Feature parentFeature, QName propQName, QName featureQName, String featureID ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNull( propQName, "Argument propQName must not be null" );
    Assert.throwIAEOnNull( parentFeature, "Argument roughnessCollection must not be null" );
    featureID = Assert.throwIAEOnNullOrEmpty( featureID );

    // try
    // {
    IGMLSchema schema = parentFeature.getFeatureType().getGMLSchema();
    IFeatureType featureType = schema.getFeatureType( featureQName );
    IPropertyType propertyType = featureType.getProperty( propQName );
    if( !(propertyType instanceof IRelationType) )
    {
      throw new RuntimeException( "UPS I DID IT AGAIN" );
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

  public static final String getFirstName( Feature feature )
  {
    Object obj = feature.getProperty( KalypsoModelRoughnessConsts.GML_PROP_NAME );
    if( obj instanceof String )
    {
      return (String) obj;
    }
    else if( obj instanceof List )
    {
      if( ((List) obj).size() > 0 )
      {
        return (String) ((List) obj).get( 0 );
      }
      else
      {
        return null;
      }
    }
    else
    {
      return null;
    }
  }
  /**
   * Get an {@link IFeatureWrapperCollection} from a feature list
   * property.
   * The feature type, the property type and the type of the collection 
   * elements can be return
   * @param feature the feature whose property is to be wrapped in a
   *        {@link IFeatureWrapperCollection} 
   * @param listPropQName the Q Name of the property
   * @param bindingInterface the class of the collection elements
   * @param doCreate a boolean controling the handling of the property 
   *            creation. if true a listProperty is created if its not 
   *            allready availayble
   *             
   * 
   */
  public static final  <T extends IFeatureWrapper2> IFeatureWrapperCollection<T> 
                              get(
                                    Feature feature,
                                    QName featureQName,
                                    QName listPropQName,
                                    Class<T> bindingInterface,
                                    boolean doCreate
                                    )
  {
    Assert.throwIAEOnNull( 
        feature, "Param feature must not be null" );
    Assert.throwIAEOnNull( 
        featureQName, "Param listPropQName must not be null" );
    Assert.throwIAEOnNull( 
        listPropQName, "Param feature must not be null" );
    Assert.throwIAEOnNull( 
        bindingInterface, "Param bindingInterface must not be null" );
    
    Object prop=
        feature.getProperty(listPropQName);
    
    FeatureWrapperCollection<T> col=null;
    
    if(prop==null)
    {
      //create the property tha is still missing
      if(doCreate)
      {
        col= 
          new FeatureWrapperCollection<T>(
                              feature,
                              featureQName,
                              listPropQName,
                              bindingInterface);
      }     
    }
    else
    {      
      //just wrapped the existing one
      col= 
        new FeatureWrapperCollection<T>(
                            feature,
                            bindingInterface,
                            listPropQName);
    }
    
    return col;
  }
}
