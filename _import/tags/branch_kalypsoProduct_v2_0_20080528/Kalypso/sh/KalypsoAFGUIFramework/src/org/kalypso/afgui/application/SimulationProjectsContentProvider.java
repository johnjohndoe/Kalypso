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
package org.kalypso.afgui.application;

import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Formatter;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.IResourceChangeListener;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.intro.config.IIntroContentProviderSite;
import org.eclipse.ui.intro.config.IIntroXHTMLContentProvider;
import org.kalypso.afgui.KalypsoAFGUIFrameworkPlugin;
import org.kalypso.contribs.eclipse.EclipsePlatformContributionsPlugin;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.ui.intro.config.DeleteProjectIntroAction;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import de.renew.workflow.connector.cases.CaseHandlingProjectNature;

/**
 * @author Gernot Belger
 * @author Stefan Kurzbach
 */
public abstract class SimulationProjectsContentProvider implements IIntroXHTMLContentProvider
{
  private final class WorkspaceChangeListener implements IResourceChangeListener
  {
    WorkspaceChangeListener( )
    {
    }

    public void resourceChanged( final IResourceChangeEvent event )
    {
      handleResourceChanged();
    }
  }

  private IIntroContentProviderSite m_site;

  private IResourceChangeListener m_resourceListener;

  /**
   * Override in order to determine which projects should be shown by this content provider.
   */
  protected abstract boolean appliesToProject( IProject project ) throws CoreException;

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#init(org.eclipse.ui.intro.config.IIntroContentProviderSite)
   */
  public void init( final IIntroContentProviderSite site )
  {
    m_site = site;
  }

  public void handleResourceChanged( )
  {
    final IWorkbench workbench = PlatformUI.getWorkbench();
    // Do not refresh if workbench is closing as welcome page is disposed in that case
    if( workbench.isClosing() )
      return;

    final IIntroContentProviderSite site = m_site;
    workbench.getDisplay().asyncExec( new Runnable()
    {
      public void run( )
      {
        site.reflow( SimulationProjectsContentProvider.this, false );
      }
    } );
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroXHTMLContentProvider#createContent(java.lang.String, org.w3c.dom.Element)
   */
  public void createContent( final String id, final Element parent )
  {
    final Document dom = parent.getOwnerDocument();

    final IWorkspace workspace = ResourcesPlugin.getWorkspace();

    if( m_resourceListener == null )
    {
      m_resourceListener = new WorkspaceChangeListener();
      workspace.addResourceChangeListener( m_resourceListener, IResourceChangeEvent.POST_CHANGE );
    }

    final IWorkspaceRoot root = workspace.getRoot();
    final IProject[] projects = root.getProjects();

    /* Table Header */
    final Element tableElement = dom.createElement( "table" );
    parent.appendChild( tableElement );

    /* Table Data */
    for( final IProject project : projects )
    {
      if( !project.isOpen() )
        continue; // ignore closed projects

      try
      {
        if( appliesToProject( project ) )
        {
          final Element rowElement = dom.createElement( "tr" );
          tableElement.appendChild( rowElement );

          final Element openData = addTableData( rowElement );
          final Element deleteData = addTableData( rowElement );

          openData.appendChild( createOpenLink( dom, project ) );
          deleteData.appendChild( createDeleteLink( dom, project ) );
        }
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoAFGUIFrameworkPlugin.getDefault().getLog().log( status );
      }
    }
  }

  private Element addTableData( final Element tableElement )
  {
    final Document dom = tableElement.getOwnerDocument();

    final Element tdElement = dom.createElement( "td" );
    tableElement.appendChild( tdElement );

    return tdElement;
  }

  private Element createOpenLink( final Document dom, final IProject project )
  {
    final CaseHandlingProjectNature currentNature = KalypsoAFGUIFrameworkPlugin.getDefault().getActiveWorkContext().getCurrentProject();
    final IProject currentProject = currentNature == null ? null : currentNature.getProject();
    final boolean isActive = project.equals( currentProject );

    final String pname = project.getName();

    final Element a = dom.createElement( "a" ); //$NON-NLS-1$
    a.setAttribute( "id", "projectLinkId_" + pname ); //$NON-NLS-1$ //$NON-NLS-2$

    if( isActive )
      a.setAttribute( "class", "projectTableLinkActive" ); //$NON-NLS-1$ //$NON-NLS-2$
    else
      a.setAttribute( "class", "projectTableLink" ); //$NON-NLS-1$ //$NON-NLS-2$

    final Formatter href = new Formatter();
    href.format( "http://org.eclipse.ui.intro/runAction?pluginId=%s&class=%s", PluginUtilities.id( KalypsoAFGUIFrameworkPlugin.getDefault() ), ActivateWorkflowProjectIntroAction.class.getName() ); //$NON-NLS-1$
    href.format( "&project=%s", pname ); //$NON-NLS-1$

    a.setAttribute( "href", href.toString() ); //$NON-NLS-1$

    final String baseUri = dom.getBaseURI();
    String fileUri = "css/link_obj.gif"; //$NON-NLS-1$
    if( baseUri != null )
    {
      try
      {
        final URL baseUrl = new URL( baseUri );
        final URL url = new URL( baseUrl, "css/link_obj.gif" ); //$NON-NLS-1$
        fileUri = url.toExternalForm();
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
        fileUri = "css/link_obj.gif"; //$NON-NLS-1$
      }
    }

    final Element img = dom.createElement( "img" ); //$NON-NLS-1$
    img.setAttribute( "border", "0" ); //$NON-NLS-1$ //$NON-NLS-2$
    img.setAttribute( "src", fileUri ); //$NON-NLS-1$
    img.setAttribute( "alt", pname + Messages.getString( "SimulationProjectsContentProvider.37" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    a.appendChild( img );

    final Element span = dom.createElement( "span" );
    a.appendChild( span );
    span.appendChild( dom.createTextNode( pname ) );

    return a;
  }

  private Element createDeleteLink( final Document dom, final IProject project )
  {
    final String pname = project.getName();

    final Element a = dom.createElement( "a" ); //$NON-NLS-1$
    a.setAttribute( "id", "projectLinkId_" + pname ); //$NON-NLS-1$ //$NON-NLS-2$
    a.setAttribute( "class", "projectTableLink" ); //$NON-NLS-1$ //$NON-NLS-2$

    final Formatter href = new Formatter();
    href.format( "http://org.eclipse.ui.intro/runAction?pluginId=%s&class=%s", EclipsePlatformContributionsPlugin.getID(), DeleteProjectIntroAction.class.getName() ); //$NON-NLS-1$
    href.format( "&project=%s", pname ); //$NON-NLS-1$

    a.setAttribute( "href", href.toString() ); //$NON-NLS-1$

    final String baseUri = dom.getBaseURI();
    String fileUri = "css/redx.gif"; //$NON-NLS-1$
    if( baseUri != null )
    {
      try
      {
        final URL baseUrl = new URL( baseUri );
        final URL url = new URL( baseUrl, "css/redx.gif" ); //$NON-NLS-1$
        fileUri = url.toExternalForm();
      }
      catch( final MalformedURLException e )
      {
        e.printStackTrace();
        fileUri = "css/redx.gif"; //$NON-NLS-1$
      }
    }

    final Element img = dom.createElement( "img" ); //$NON-NLS-1$
    img.setAttribute( "border", "0" ); //$NON-NLS-1$ //$NON-NLS-2$
    img.setAttribute( "src", fileUri ); //$NON-NLS-1$
    img.setAttribute( "alt", Messages.getString( "SimulationProjectsContentProvider.38" ) ); //$NON-NLS-1$ //$NON-NLS-2$

    a.appendChild( img );
    return a;
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#createContent(java.lang.String, java.io.PrintWriter)
   */
  public void createContent( final String id, final PrintWriter out )
  {
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#createContent(java.lang.String,
   *      org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  public void createContent( final String id, final Composite parent, final FormToolkit toolkit )
  {
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#dispose()
   */
  public void dispose( )
  {
    if( m_resourceListener != null )
    {
      final IWorkspace workspace = ResourcesPlugin.getWorkspace();
      workspace.removeResourceChangeListener( m_resourceListener );
    }
  }
}