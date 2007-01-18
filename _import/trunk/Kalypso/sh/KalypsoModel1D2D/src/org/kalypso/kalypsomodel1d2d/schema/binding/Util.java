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
package org.kalypso.kalypsomodel1d2d.schema.binding;

import javax.xml.namespace.QName;

import org.deegree.gml.GMLSchema;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.core.FeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapper;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * @author Patrice Congo
 *
 */
public class Util
{
  /**
   * Create a feature of the given type and link
   * it to the given parentFeature as a property of the
   * specified q-name
   * @param parentFeature the parent feature
   * @param propQName the q-name of the property linking the
   *    parent and the newly created child
   * @param featureQName the q-name denoting the type of the feature
   */
  public static final  Feature createFeatureAsProperty(
      Feature parentFeature,
      QName propQName, 
      QName featureQName)
      throws IllegalArgumentException
  {
    Assert.throwIAEOnNull(
    propQName, "Argument propQName must not be null");
    Assert.throwIAEOnNull(
        parentFeature, 
        "Argument roughnessCollection must not be null");
    try
    {
       return FeatureHelper.addFeature(
          parentFeature, 
          propQName, 
          featureQName);
    }
    catch(GMLSchemaException ex)
    {
      throw new IllegalArgumentException(
          "Property "+propQName+
              " does not accept element of type"+
          featureQName,
          ex);
    }   
  }
  
  /**
   * TODO complete and test this method
   */
  
  /**
   * Create a feature of the given type and link
   * it to the given parentFeature as a property of the
   * specified q-name
   * @param parentFeature the parent feature
   * @param propQName the q-name of the property linking the
   *    parent and the newly created child
   * @param featureQName the q-name denoting the type of the feature
   * @param throws {@link IllegalArgumentException} if parentFeature
   *    is null or propQName is null, or featureQName is null or 
   *    featureID is null or empty or there is a feature in the workspace
   *    with the same id
   *    
   */
  public static final  Feature createFeatureAsProperty(
            Feature parentFeature,
            QName propQName, 
            QName featureQName,
            String featureID)
            throws IllegalArgumentException
  {
    Assert.throwIAEOnNull(
    propQName, "Argument propQName must not be null");
    Assert.throwIAEOnNull(
        parentFeature, 
        "Argument roughnessCollection must not be null");
    featureID=Assert.throwIAEOnNullOrEmpty( featureID );
    
    
    
//    try
//    {
      IGMLSchema schema = 
        parentFeature.getFeatureType().getGMLSchema();
      IFeatureType featureType=
          schema.getFeatureType( featureQName );
      IPropertyType propertyType=
              featureType.getProperty( propQName );
      if(!(propertyType instanceof IRelationType))
      {
        throw new RuntimeException("UPS I DID IT AGAIN");
      }
      return FeatureFactory.createFeature( 
                        parentFeature, 
                        (IRelationType)propertyType,//parentRelation, 
                        featureID, 
                        featureType, 
                        true,//initializeWithDefaults, 
                        1//depth 
                        );
//       return FeatureHelper.addFeature(
//          parentFeature, 
//          propQName, 
//          featureQName);
//    }
//    catch(GMLSchemaException ex)
//    {
//      throw new IllegalArgumentException(
//          "Property "+propQName+
//              " does not accept element of type"+
//          featureQName,
//          ex);
//    }   
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
  public static final <T extends IFeatureWrapper> 
            IFeatureWrapperCollection<T>get(
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
