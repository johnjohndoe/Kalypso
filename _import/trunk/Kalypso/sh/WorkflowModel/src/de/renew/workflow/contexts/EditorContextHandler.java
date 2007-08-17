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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IPersistableElement;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;

/**
 * @author Stefan Kurzbach
 * 
 */
public class EditorContextHandler extends AbstractHandler implements IHandler
{

  private final String m_editorId;

  private final String m_input;

  public EditorContextHandler( final String editorId, final String input )
  {
    m_editorId = editorId;
    m_input = input;
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( final ExecutionEvent event ) throws ExecutionException
  {
    final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();

    final IWorkbenchWindow activeWorkbenchWindow = (IWorkbenchWindow) context.getVariable( ISources.ACTIVE_WORKBENCH_WINDOW_NAME );
    if( activeWorkbenchWindow != null && m_editorId != null )
    {
      final IWorkbenchPage workbenchPage = activeWorkbenchWindow.getActivePage();
      try
      {
        IEditorInput input = null;
        final Path path = new Path( m_input );
        final IContainer caseFolder = (IContainer) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );
        final IFile ifile = caseFolder.getFile( path );
        if( ifile != null )
        { // The file is in the workspace
          input = new FileEditorInput(ifile);
        }
        if( input != null )
        {
          IDE.openEditor( workbenchPage, input, m_editorId );
        }
      }
      catch( final PartInitException e )
      {
        throw new ExecutionException( Messages.getString("EditorContextHandler.0") + m_editorId + Messages.getString("EditorContextHandler.1") + m_input, e ); //$NON-NLS-1$ //$NON-NLS-2$
      }
      return Status.OK_STATUS;
    }
    else
    {
      return Status.CANCEL_STATUS;
    }

  }

}
