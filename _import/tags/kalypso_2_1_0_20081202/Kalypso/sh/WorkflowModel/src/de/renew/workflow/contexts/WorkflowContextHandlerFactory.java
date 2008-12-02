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

import org.eclipse.core.commands.IHandler;

/**
 * Creates the context handlers for the basic workflow contexts. These are {@link PerspectiveContext},
 * {@link ViewContext}, {@link EditorContext}, {@link EditorInputContext} and {@link WizardContext}.
 * 
 * @author Stefan Kurzbach
 */
public class WorkflowContextHandlerFactory implements IContextHandlerFactory
{
  /**
   * @see org.kalypso.afgui.workflow.IContextHandlerFactory#getHandler(org.kalypso.afgui.workflow.ContextType)
   */
  public IHandler getHandler( final ContextType context )
  {
    if( context instanceof PerspectiveContextType )
    {
      final PerspectiveContextType perspectiveContext = (PerspectiveContextType) context;
      final String perspectiveId = perspectiveContext.getPerspectiveId();
      final PerspectiveContextHandler perspectiveContextHandler = new PerspectiveContextHandler( perspectiveId );
      return perspectiveContextHandler;
    }
    else if( context instanceof ViewContext )
    {
      final ViewContext viewContext = (ViewContext) context;
      final String viewId = viewContext.getPartId();
      final ViewContextHandler viewContextHandler = new ViewContextHandler( viewId );
      return viewContextHandler;
    }
    else if( context instanceof EditorContext )
    {
      final EditorContext editorContext = (EditorContext) context;
      final String editorId = editorContext.getPartId();
      final String input = editorContext.getInput();
      return new EditorContextHandler(editorId, input);
    }    
    else if( context instanceof WizardContext )
    {
      final WizardContext wizardContext = (WizardContext) context;
      final String wizardId = wizardContext.getWizardId();
      final EWizardType wizardType = wizardContext.getWizardType();
      return new WizardContextHandler( wizardId, wizardType );
    }
    else if( context instanceof WorkbenchSiteContext )
    {
      final WorkbenchSiteContext multiContext = (WorkbenchSiteContext) context;
      final MultiContextHandler contextHandler = new MultiContextHandler( multiContext, this );
      return contextHandler;
    }
    else if( context instanceof ExtensionContext )
    {
      final ExtensionContext multiContext = (ExtensionContext) context;
      final IContextHandlerFactory factory = ContextHandlerFactoryExtension.getFactory( multiContext );
      return factory.getHandler( multiContext );
    }
    else
      return null;
  }
}
