/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/

/*******************************************************************************
 * Copyright (c) 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.kalypso.ogc.gml.map.widgets;

import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.Parameterization;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.ui.commands.ICommandService;
import org.kalypso.ui.GenericCommandActionDelegate;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * This action delegate can be used to specify a widget and a context to call and update a {@link Command}. Handlers
 * that wish to handle the command must register for the command id
 * "org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand#context" and subclass {@link SelectWidgetHandler}. The
 * specified widget will then be called from the command. The context can be used to set the active state of the
 * handler. <br>
 * <br>
 * Example:<br>
 * <blockquote> <br>
 * <action id="xp.CreateGridDelegate"> <br>
 * <class class="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommandActionDelegate"> <br>
 * <parameter name="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand.widget" value="xp.CreateGitterWidget"/>
 * <parameter name="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand.context" value="MyContext"/> <br>
 * <parameter name="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand.plugin" value="myPlugin"/> <br>
 * </class> </action> <br>
 * <handler class="MySelectWidgetHandler" <br>
 * commandId="org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand#MyContext"> <br>
 * <activeWhen> <with variable="activeContexts"> <iterate operator="or"> <equals value="MyContext"/> </iterate> </with>
 * </activeWhen> </handler> </blockcode>
 */
public class SelectWidgetCommandActionDelegate extends GenericCommandActionDelegate
{
  private static final String COMMAND_ID = "org.kalypso.ogc.gml.map.widgets.SelectWidgetCommand";

  private static final String PARAM_CONTEXT = COMMAND_ID + ".context";

  public static final String PARAM_WIDGET_CLASS = COMMAND_ID + ".widget";

  public static final String PARAM_PLUGIN_ID = COMMAND_ID + ".plugin";

  private String m_context = null;

  private String m_widgetClass = null;

  private String m_pluginId;

  /**
   * @see org.kalypso.ui.GenericCommandActionDelegate#setInitializationData(org.eclipse.core.runtime.IConfigurationElement,
   *      java.lang.String, java.lang.Object)
   */
  @SuppressWarnings("unchecked")
  @Override
  public void setInitializationData( final IConfigurationElement config, final String propertyName, final Object data ) throws CoreException 
  {
    super.setInitializationData( config, propertyName, data );

    if( data instanceof Map )
    {
      final Map<String, String> parameterMap = (Map<String, String>) data;
      m_widgetClass = parameterMap.get( PARAM_WIDGET_CLASS );
      m_context = parameterMap.get( PARAM_CONTEXT );
      m_pluginId = parameterMap.get( PARAM_PLUGIN_ID );
    }

    if( m_widgetClass == null || m_context == null || m_pluginId == null )
    {
      final String id = config.getAttribute( "id" );
      final Status status = new Status( IStatus.ERROR, KalypsoGisPlugin.getId(), SWT.OK, "The '" + id + "' action won't work without a context, a widget class and a plugin id.", null );
      throw new CoreException( status );
    }
  }

  /**
   * TODO: makes almost the same as in GenericCommandActionDelegat: better code reuse is needed
   * 
   * @see org.kalypso.ui.GenericCommandActionDelegate#createCommand(org.eclipse.ui.commands.ICommandService)
   */
  @Override
  protected ParameterizedCommand createCommand( final ICommandService commandService )
  {
    final String cmdName = COMMAND_ID + "#" + m_context;
    final Command command = commandService.getCommand( cmdName );
    try
    {
      if( !command.isDefined() )
      {
        final Command selectWidgetCmd = commandService.getCommand( COMMAND_ID );
        command.define( cmdName, cmdName, selectWidgetCmd.getCategory(), selectWidgetCmd.getParameters() );
      }
      
      return new ParameterizedCommand( command, new Parameterization[] { new Parameterization( command.getParameter( PARAM_WIDGET_CLASS ), m_widgetClass ),
          new Parameterization( command.getParameter( PARAM_PLUGIN_ID ), m_pluginId ) } );
    }
    catch( final NotDefinedException e )
    {
      e.printStackTrace();
    }
    return null;
  }
}
