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

import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

/**
 * Interface for classes representing the model an simMeta1d2d:ModelDescriptor.
 * 
 * Classed implementing this interface provides all necessary data to resolve the describing model
 * 
 * @author Patrice Congo
 * @author Dejan Antanaskovic
 * 
 */
public interface IModelDescriptor extends IFeatureWrapper2
{
  /**
   * To get the GML ID of the described model.
   * 
   * @return a String representing the gml ID of the described model.
   * 
   */
  public String getModelID( );

  public void setModelID( String modelID );

  /**
   * To get a Human readable name of the described model
   * 
   * @return a string which represents the human readable name of the described model
   */
  public String getModelName( );

  /**
   * To set a new model name for this descriptor
   * 
   * @param a
   *            string representing the name of the model the string may be null.
   */
  public void setModelName( String model );

  /**
   * To get type of the model being described
   * 
   * @return a string representing the type of the model described
   */
  public String getModelType( );

  /**
   * To set a new model type for this descriptor. This type represents the type of the model. E.g. typically a qualified
   * name
   * 
   * @param modelType
   *            the new model type to set
   * @throws IllegalArgumentException
   *             if modelType is null
   */
  public void setModelType( String modelType );

  /**
   * To get the path of the workspace containing the model being described by this descriptor.
   * 
   * @return a string representing the path of the workspace containg the described model
   */
  public String getWorkspacePath( );

  /**
   * To set the model workspace for this descriptor. Note that the path will not be check here.
   * 
   * @param workspacePath
   *            a non null string representing the workspace path
   * @throws IllegalArgumentException
   *             if workspace path is null
   */
  public void setWorkspacePath( String workspacePath );

  /**
   * Answer whether or not this descriptor is describing the feature represented by the given {@link IFeatureWrapper2}
   * 
   * @param featureWrapper2
   *            the feature wrapper describing the feature to check
   * @return true if this descriptor is describing this feature otherwise fasle
   * @throws IllegalArgumentException
   *             if featureWrapper2 is null
   */
  public boolean isDescribing( IFeatureWrapper2 featureWrapper2 );

}
