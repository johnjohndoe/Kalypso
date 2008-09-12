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
package org.kalypso.workflow.ui.browser.urlaction;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.FileEditorInput;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;

/**
 * example<br>
 * kalypso://closeEditor?input=project:file.gmt&doSave=true
 * 
 * @author doemming
 */
public class URLActionCloseEditor extends AbstractURLAction
{
  /**
   * optional
   */
  private final static String PARAM_INPUT = "input";

  /**
   * optional boolean<br>
   * default: <code>false</code>
   */
  private final static String PARAM_DO_SAVE = "doSave";

  /**
   * @see org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURLAction#run(org.kalypso.contribs.eclipse.ui.browser.commandable.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String testInputLocation = commandURL.getParameter( PARAM_INPUT );
    final String doSaveAsString = commandURL.getParameter( PARAM_DO_SAVE );
    final boolean doSave = Boolean.parseBoolean( doSaveAsString );
    final IWorkbenchPage activePage = getActivePage();
    final IEditorReference[] editorReferences = activePage.getEditorReferences();
    final FileEditorInput testInput;
    if( testInputLocation == null || testInputLocation.length() < 1 )
      testInput = null;
    else
    {
      try
      {
        final URL inputURL = getWorkFlowContext().resolveURL( testInputLocation );
        final IFile file = ResourceUtilities.findFileFromURL( inputURL );
        testInput = new FileEditorInput( file );
      }
      catch( MalformedURLException e )
      {
        e.printStackTrace();
        MessageDialog.openError( getShell(), "Error WorkflowBrowser Command", "The requrested Editor could not be closed. Please contact your Administrator. Error:" + e.getMessage() );
        return false;
      }
    }
    final List<IEditorReference> editorsToClose = new ArrayList<IEditorReference>();
    for( IEditorReference ref : editorReferences )
    {
      boolean inputFlag = false;
      if( testInput == null )
        inputFlag = true;
      else
      {
        final IEditorInput editorInput;
        try
        {
          editorInput = ref.getEditorInput();
          inputFlag = testInput.equals( editorInput );
        }
        catch( PartInitException e )
        {
          e.printStackTrace();
          inputFlag = false;
        }
      }
      if( inputFlag )
        editorsToClose.add( ref );
    }
    final IEditorReference[] editorsToCloseArray = editorsToClose.toArray( new IEditorReference[editorsToClose.size()] );
    getActivePage().closeEditors( editorsToCloseArray, doSave );
    return true;
  }

}
