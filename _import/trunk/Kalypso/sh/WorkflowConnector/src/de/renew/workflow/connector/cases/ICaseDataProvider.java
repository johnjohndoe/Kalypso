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
package de.renew.workflow.connector.cases;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * The case data provider functions as a bridge between the abstract case data model and actual data objects. Generics
 * provide a way to create data providers that return data objects of a type more specific than {@link Object}.
 * 
 * @author Gernot Belger
 * @author Stefan Kurzbach
 */
public interface ICaseDataProvider<T extends Object>
{
  /**
   * Returns the data object corresponding to the given case data key.
   * 
   * @deprecated Use {@link #getModel(String)} instead.
   */
  @Deprecated
  public <D extends T> D getModel( final Class<D> modelClass ) throws CoreException;

  /**
   * Returns the data object corresponding to the given case data key.
   * 
   * @param id
   *          Id of the queried data (probably the extension-id with which this data was registered)
   * @param modelClass
   *          The root feature of the gml-workspace must either adapt to or inherit from this class.
   */
  public <D extends T> D getModel( final String id, final Class<D> modelClass ) throws CoreException;

  /**
   * Saves all model data
   */
  public void saveModel( final IProgressMonitor monitor ) throws CoreException;

  /**
   * @deprecated Use {@link #saveModel(String, IProgressMonitor)} instead.
   */
  @Deprecated
  public void saveModel( final Class< ? extends T> modelClass, final IProgressMonitor monitor ) throws CoreException;

  /**
   * Saves the data object corresponding to the given case data key
   * 
   * @param id
   *          Id of the queried data (probably the extension-id with which this data was registered)
   */
  public void saveModel( final String id, final IProgressMonitor monitor ) throws CoreException;

  /**
   * Returns <code>true</code> if the data object corresponding to the given case data key has been modified since it
   * was retrieved.
   * 
   * @deprecated Use {@link #isDirty(String)} instead.
   */
  @Deprecated
  public boolean isDirty( final Class< ? extends T> modelClass );

  /**
   * Returns <code>true</code> if the data object corresponding to the given case data key has been modified since it
   * was retrieved.
   * 
   * @param id
   *          Id of the queried data (probably the extension-id with which this data was registered)
   */
  public boolean isDirty( final String id );

  public boolean isDirty( );

  public void reloadModel( );

  public void setCurrent( final IContainer container );
}