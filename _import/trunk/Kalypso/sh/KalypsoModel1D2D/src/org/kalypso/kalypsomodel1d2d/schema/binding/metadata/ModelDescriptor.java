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
package org.kalypso.kalypsomodel1d2d.schema.binding.metadata;

import javax.xml.namespace.QName;

import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree_impl.model.feature.binding.AbstractFeatureBinder;

/**
 * Default implementation of {@link IModelDescriptor}
 * 
 * @author Patrice Congo
 * 
 */
public class ModelDescriptor extends AbstractFeatureBinder implements IModelDescriptor
{

  public ModelDescriptor( Feature featureToBind )
  {
    this( featureToBind, Kalypso1D2DSchemaConstants.SIMMETA_F_MODELDESCRIPTOR );
  }

  /**
   * Creates a model descriptor object binding the given descriptor feature.
   * 
   * @param featureToBind
   *            the descriptor feature to bind
   * @param qnameToBind
   *            the qname of the feature to bind
   */
  public ModelDescriptor( Feature featureToBind, QName qnameToBind )
  {
    super( featureToBind, qnameToBind );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#getModelID()
   */
  public String getModelID( )
  {
    String modelName = getProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODEL_ID, String.class );
    return modelName;

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#getModelName()
   */
  public String getModelName( )
  {
    String modelName = getProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODEL_NAME, String.class );
    return modelName;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#getModelType()
   */
  public String getModelType( )
  {
    String modelName = getProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODEL_TYPE, String.class );
    return modelName;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#getWorkspacePath()
   */
  public String getWorkspacePath( )
  {
    String path = getProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_WORKSPACE_PATH, String.class );
    return path;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#setModelID(java.lang.String)
   */
  public void setModelID( String modelID )
  {
    setProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODEL_ID, modelID );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#setModelName(java.lang.String)
   */
  public void setModelName( String model )
  {
    setProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODEL_NAME, model );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#setModelType(java.lang.String)
   */
  public void setModelType( String modelType )
  {
    setProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_MODEL_TYPE, modelType );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#setWorkspacePath(java.lang.String)
   */
  public void setWorkspacePath( String workspacePath )
  {
    setProperty( Kalypso1D2DSchemaConstants.SIMMETA_PROP_WORKSPACE_PATH, workspacePath );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.schema.binding.metadata.IModelDescriptor#isDescribing(org.kalypsodeegree.model.feature.binding.IFeatureWrapper2)
   */
  public boolean isDescribing( IFeatureWrapper2 featureWrapper2 )
  {
    Assert.throwIAEOnNullParam( featureWrapper2, "featureWrapper2" );
    Feature feature = featureWrapper2.getWrappedFeature();
    String testeeContext = feature.getWorkspace().getContext().toString();
    String testeeGmlID = feature.getId();

    return /* testeeContext.equals( getWorkspacePath() ) && */testeeGmlID.equals( getModelID() );
  }

}
