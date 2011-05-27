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
package org.kalypso.model.km.internal.ui.kmupdate;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultCalculationNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultEclipseNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultFolderNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultPolynomeCalculationNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultQIntervalNode;

/**
 * Filters all elements, which are not needed to browse to WSPM results.
 * 
 * @author Holger Albert
 */
public class WspmResultViewerFilter extends ViewerFilter
{
  /**
   * The constructor.
   */
  public WspmResultViewerFilter( )
  {
  }

  /**
   * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public boolean select( Viewer viewer, Object parentElement, Object element )
  {
    if( element instanceof WspmResultEclipseNode )
      return true;

    if( element instanceof WspmResultCalculationNode )
    {
      WspmResultCalculationNode resultNode = (WspmResultCalculationNode) element;
      IWspmResultNode[] childResults = resultNode.getChildResults();
      if( childResults.length == 0 )
        return false;

      for( IWspmResultNode childResult : childResults )
      {
        if( !(childResult instanceof WspmResultFolderNode) )
          continue;

        /* Folder nodes must have children, in order to have results. */
        WspmResultFolderNode folderNode = (WspmResultFolderNode) childResult;
        if( folderNode.getChildResults().length > 0 )
          return true;
      }

      return false;
    }

    if( element instanceof WspmResultPolynomeCalculationNode )
      return true;

    if( element instanceof WspmResultFolderNode )
    {
      WspmResultFolderNode folderNode = (WspmResultFolderNode) element;
      if( folderNode.getChildResults().length > 0 )
      {
        for( IWspmResultNode resultNode : folderNode.getChildResults() )
        {
          if( resultNode instanceof WspmResultQIntervalNode )
            return true;
        }
      }

      return false;
    }

    if( element instanceof WspmResultQIntervalNode )
      return true;

    return false;
  }
}