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
package de.renew.workflow.contexts;

import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Bundle;

import de.renew.workflow.base.Workflow;
import de.renew.workflow.base.WorkflowModelPlugin;

/**
 * Helper class to read and cache workflow systems from extension point.
 * 
 * @author Stefan Kurzbach
 */
public class WorkflowSystemExtension
{

  private static Map<String, Workflow> THE_WORKFLOW_MAP = null;

  private final static String WORKFLOW_SYSTEM_EXTENSION_POINT = "de.renew.workflow.model.workflowSystem";

  private final static JAXBContext JC = createJAXBContext();

  public static Workflow getWorkflow( final String id )
  {
    final Map<String, Workflow> map = getWorkflowMap();
    if( map == null )
      return null;

    return map.get( id );
  }

  private static Map<String, Workflow> getWorkflowMap( )
  {
    if( THE_WORKFLOW_MAP == null )
    {
      final IExtensionRegistry registry = Platform.getExtensionRegistry();
      final IExtensionPoint extensionPoint = registry.getExtensionPoint( WORKFLOW_SYSTEM_EXTENSION_POINT );
      final IConfigurationElement[] configurationElements = extensionPoint.getConfigurationElements();
      THE_WORKFLOW_MAP = new HashMap<String, Workflow>( configurationElements.length );

      for( final IConfigurationElement element : configurationElements )
      {
        final String id = element.getAttribute( "id" );
        try
        {
          final String filePath = element.getAttribute( "definition" );
          final IContributor contributor = element.getContributor();
          final Bundle bundle = Platform.getBundle( contributor.getName() );
          final URL entry = bundle.getEntry( filePath );
          if( entry != null )
          {
            final Workflow workflow = loadModel( entry );
            THE_WORKFLOW_MAP.put( id, workflow );
          }
          else
          {
            throw new CoreException( new Status( Status.ERROR, "de.renew.workflow.model", "Invalid path " + filePath ) );
          }
        }
        catch( final CoreException e )
        {
          final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", "Failed to create workflowSystem extension for id: " + id, e );
          WorkflowModelPlugin.getInstance().getLog().log( status );
        }
      }
    }

    return THE_WORKFLOW_MAP;
  }

  private static JAXBContext createJAXBContext( )
  {
    try
    {
      return JAXBContext.newInstance( de.renew.workflow.base.ObjectFactory.class, de.renew.workflow.contexts.ObjectFactory.class, de.renew.workflow.cases.ObjectFactory.class );
    }
    catch( final JAXBException e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private static Workflow loadModel( final URL entry ) throws CoreException
  {
    try
    {
      return (Workflow) JC.createUnmarshaller().unmarshal( entry );
    }
    catch( final Throwable e )
    {
      final IStatus status = new Status( Status.ERROR, "de.renew.workflow.model", "", e );
      throw new CoreException( status );
    }
  }

}
