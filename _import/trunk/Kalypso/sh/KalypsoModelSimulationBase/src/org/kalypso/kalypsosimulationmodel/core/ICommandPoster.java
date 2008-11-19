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

import org.eclipse.core.runtime.CoreException;
import org.kalypso.commons.command.ICommand;
import org.kalypso.kalypsosimulationmodel.core.modeling.IModel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;

/**
 * Interface use to make some workflow specific stuff accessible to class in simulation base and model1d2d plug-in.
 * 
 * @author Patrice Congo
 */
public interface ICommandPoster
{
  /**
   * Post the command to a commandable workspace identified by the wrapperClass.<br>
   * 
   * @deprecated Use {@link #postCommand(String, ICommand)} instead.
   */
  @Deprecated
  public void postCommand( final Class< ? extends IModel> wrapperClass, final ICommand command ) throws InvocationTargetException;

  /**
   * Post the command to a commandable workspace identified by the wrapperClass.<br>
   * 
   * @param id
   *          Id of the queried data (probably the extension-id with which this data was registered)
   */
  public void postCommand( final String id, final ICommand command ) throws InvocationTargetException;

  /**
   * gets the a commandable workspace for the given wrapper class.
   * 
   * @deprecated Use {@link #getCommandableWorkSpace(String)} instead.
   */
  @Deprecated
  public CommandableWorkspace getCommandableWorkSpace( final Class< ? extends IModel> wrapperClass ) throws IllegalArgumentException, CoreException;

  /**
   * gets the a commandable workspace for the given wrapper class.
   * 
   * @param id
   *          Id of the queried data (probably the extension-id with which this data was registered)
   */
  public CommandableWorkspace getCommandableWorkSpace( final String id ) throws IllegalArgumentException, CoreException;
}
