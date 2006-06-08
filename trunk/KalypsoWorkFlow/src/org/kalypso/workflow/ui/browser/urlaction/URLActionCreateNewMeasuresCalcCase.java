/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.workflow.WorkflowContext;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.CommandURLFactory;
import org.kalypso.workflow.ui.browser.ICommandURL;
import org.kalypso.workflow.ui.browser.IURLAction;
import org.kalypso.workflow.ui.dialog.CreateNewMeasureCalcCaseDialog;

/**
 * @author kuepfer
 */
public class URLActionCreateNewMeasuresCalcCase extends AbstractURLAction
{
  private final static String COMMAND_NAME = "createMeasureCalcCase";

  public final static URLActionRegistry m_defaultActionRegistry = URLActionRegistry.getDefault();

  private final static String PARAM_SOURCE = "sourceURL";

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#getActionName()
   */
  public String getActionName( )
  {
    return COMMAND_NAME;
  }

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String sourceURLAsString = commandURL.getParameter( PARAM_SOURCE );
    final WorkflowContext context = getWorkFlowContext();
    IFolder calcCaseFolder = null;
    final IFolder contextCalcDir = context.getContextCalcDir();
    final CreateNewMeasureCalcCaseDialog dialog = new CreateNewMeasureCalcCaseDialog( getShell(), contextCalcDir );
    if( dialog.open() == Window.OK )
    {
      try
      {
        calcCaseFolder = dialog.getCalcCaseFolter();
      }
      catch( CoreException e )
      {
        e.printStackTrace();
        MessageDialog.openError( getShell(), "FLOWS Planer Client Fehlermeldung", "Fehler beim erstellen eines neuen Rechenfalls. Kein gültiger Ordner erzeugt!" );
        return false;
      }
    }
    URL sourceURL = null;
    String calcCasePathAsString = null;
    try
    {
      sourceURL = context.resolveURL( sourceURLAsString );
      calcCasePathAsString = ResourceUtilities.createURL( calcCaseFolder ).toString();
    }
    catch( MalformedURLException e )
    {
      e.printStackTrace();
      MessageDialog.openError( getShell(), "FLOWS Planer Client Fehlermeldung", "Fehler beim erstellen eines neuen Rechenfalls. Zielordner nicht gefunden!" );
      return false;
    }

    final IURLAction extractAction = m_defaultActionRegistry.getURLAction( "extract" );
    final ICommandURL extractCommandURL = CommandURLFactory.createCommandURL( "kalypso://extract?sourceURL=" + sourceURL.toString() + "&target=" + calcCasePathAsString );
    extractAction.init( getWorkFlowContext() );
    return extractAction.run( extractCommandURL );
  }

}
