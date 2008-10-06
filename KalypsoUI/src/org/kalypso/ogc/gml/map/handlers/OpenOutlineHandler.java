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
package org.kalypso.ogc.gml.map.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.ISources;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartSite;
import org.kalypso.ogc.gml.outline.GisMapOutlineView;

/**
 * This handler opens the outline.
 * 
 * @author Holger Albert
 */
public class OpenOutlineHandler extends AbstractHandler
{
  /**
   * The constructor.
   */
  public OpenOutlineHandler( )
  {
  }

  /**
   * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
   */
  @Override
  public Object execute( ExecutionEvent event ) throws ExecutionException
  {
    IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
    IWorkbenchPart part = (IWorkbenchPart) context.getVariable( ISources.ACTIVE_PART_NAME );
    if( part == null )
      throw new ExecutionException( "No active part." );

    IWorkbenchPartSite site = part.getSite();
    if( site == null )
      throw new ExecutionException( "No active site." );

    IWorkbenchPage page = site.getPage();
    if( page == null )
      throw new ExecutionException( "No active page." );

    try
    {
      IViewPart view = page.findView( GisMapOutlineView.ID );
      if( view != null )
      {
        page.hideView( view );

        return Status.OK_STATUS;
      }

      /* Open the outline. */
      page.showView( GisMapOutlineView.ID );

      /* Focus the part. If it is a map view, the outline will be filled. */
      part.setFocus();

      return Status.OK_STATUS;
    }
    catch( Exception e )
    {
      throw new ExecutionException( "Could not change the outline view ...", e );
    }
  }
}