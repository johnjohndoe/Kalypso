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
package org.kalypso.model.product.application;

import java.io.PrintWriter;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.intro.config.IIntroContentProviderSite;
import org.eclipse.ui.intro.config.IIntroXHTMLContentProvider;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.model.product.KalypsoModelProductPlugin;
import org.kalypso.model.product.i18n.Messages;
import org.kalypso.simulation.ui.calccase.ModelNature;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author Gernot Belger
 */
public class SimulationProjectsContentProvider implements IIntroXHTMLContentProvider
{
  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#init(org.eclipse.ui.intro.config.IIntroContentProviderSite)
   */
  @Override
  public void init( final IIntroContentProviderSite site )
  {
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroXHTMLContentProvider#createContent(java.lang.String, org.w3c.dom.Element)
   */
  @Override
  public void createContent( final String id, final Element parent )
  {
    final Document dom = parent.getOwnerDocument();

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
    final IProject[] projects = root.getProjects();
    for( final IProject project : projects )
    {
      try
      {
        final ModelNature nature = (ModelNature) project.getNature( ModelNature.ID );
        if( nature == null )
          continue;

        // TODO: filter project according to model
        
        final String pname = project.getName();

        final Element a = dom.createElement( "a" ); //$NON-NLS-1$
        a.setAttribute( "id", "projectLinkId_" + pname ); //$NON-NLS-1$ //$NON-NLS-2$
        a.setAttribute( "class", "link" ); //$NON-NLS-1$ //$NON-NLS-2$

        final StringBuffer href = new StringBuffer();
        href.append( "http://org.eclipse.ui.intro/runAction?pluginId=org.kalypso.contribs.eclipseplatform&class=org.kalypso.contribs.eclipse.ui.intro.config.OpenPerspectiveAction" ); //$NON-NLS-1$
        href.append( "&perspectiveId=org.kalypso.model.wspm.ui.product.WspmPerspectiveFactory" ); //$NON-NLS-1$
        href.append( "&file=/" ); //$NON-NLS-1$
        href.append( pname );
        href.append( "/modell.gml" ); //$NON-NLS-1$
        href.append( "&editorId=" ); //$NON-NLS-1$
        href.append( "org.kalypso.ui.editor.GisEditor" ); //$NON-NLS-1$

        a.setAttribute( "href", href.toString() ); //$NON-NLS-1$

        final Element img = dom.createElement( "img" ); //$NON-NLS-1$
        img.setAttribute( "class", "link" ); //$NON-NLS-1$ //$NON-NLS-2$
        img.setAttribute( "border", "0" ); //$NON-NLS-1$ //$NON-NLS-2$
        img.setAttribute( "src", "css/link_obj.gif" ); //$NON-NLS-1$ //$NON-NLS-2$
        img.setAttribute( "alt", pname + Messages.getString("org.kalypso.model.product.application.SimulationProjectsContentProvider.20") ); //$NON-NLS-1$ //$NON-NLS-2$

        a.appendChild( img );
        a.appendChild( dom.createTextNode( pname ) );
        parent.appendChild( a );

        final Element br = dom.createElement( "br" ); //$NON-NLS-1$
        parent.appendChild( br );
      }
      catch( final Exception e )
      {
        final IStatus status = StatusUtilities.statusFromThrowable( e );
        KalypsoModelProductPlugin.getDefault().getLog().log( status );
      }
    }
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#createContent(java.lang.String, java.io.PrintWriter)
   */
  @Override
  public void createContent( final String id, final PrintWriter out )
  {
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#createContent(java.lang.String,
   *      org.eclipse.swt.widgets.Composite, org.eclipse.ui.forms.widgets.FormToolkit)
   */
  @Override
  public void createContent( final String id, final Composite parent, final FormToolkit toolkit )
  {
  }

  /**
   * @see org.eclipse.ui.intro.config.IIntroContentProvider#dispose()
   */
  @Override
  public void dispose( )
  {
  }

}
