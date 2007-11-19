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

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.model.WorkbenchAdapter;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import de.renew.workflow.cases.Case;
import de.renew.workflow.connector.WorkflowConnectorPlugin;
import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * TODO: is this really the right place? Shouldn't it better be moved to the AfgUi plug-in which already does all
 * ui-stuff for the workflow?
 * 
 * @author Stefan Kurzbach
 */
public class CaseTreeContentAdapter extends WorkbenchAdapter
{
  private final ImageDescriptor m_caseImage;

  private final FontData m_activeFont;

  public CaseTreeContentAdapter( )
  {
    m_caseImage = AbstractUIPlugin.imageDescriptorFromPlugin( WorkflowConnectorPlugin.PLUGIN_ID, "icons/blue.png" );
    final FontData[] fontData = JFaceResources.getFontRegistry().getFontData( JFaceResources.DIALOG_FONT );
    final String dialogFontName = fontData[0].getName();
    final int dialogFontHeight = fontData[0].getHeight();
    m_activeFont = new FontData( dialogFontName, dialogFontHeight, SWT.BOLD );
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getChildren(java.lang.Object)
   */
  @Override
  public Object[] getChildren( final Object o )
  {
    return NO_CHILDREN;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getImageDescriptor(java.lang.Object)
   */
  @Override
  public ImageDescriptor getImageDescriptor( final Object object )
  {
    return m_caseImage;
  }

  /**
   * @see org.eclipse.ui.model.IWorkbenchAdapter#getLabel(java.lang.Object)
   */
  @Override
  public String getLabel( final Object o )
  {
    if( o instanceof Case )
    {
      return ((Case) o).getName();
    }
    return null;
  }

  /**
   * @see org.eclipse.ui.model.WorkbenchAdapter#getFont(java.lang.Object)
   */
  @Override
  public FontData getFont( final Object o )
  {
    if( o instanceof Case )
    {
      final Case caze = (Case) o;
      final IWorkbench workbench = PlatformUI.getWorkbench();
      if( !workbench.isClosing() )
      {
        final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
        final IEvaluationContext currentState = handlerService.getCurrentState();
        final String activeCaseURI = (String) currentState.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_URI_NAME );
        if( caze.getURI().equals( activeCaseURI ) )
          return m_activeFont;
      }
    }
    return null;
  }
}
