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
package org.kalypso.ui.wizards.results.filters;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta;
import org.kalypso.kalypsomodel1d2d.schema.binding.result.IDocumentResultMeta.DOCUMENTTYPE;

/**
 * filter for hiding the non-georeference-able data in the {@link AddResultThemeWizard}. <br>
 * <br>
 * non-georeference-able data are logs, coreDataZip, length-sections and for the moment the fem-terrain-tin.
 * 
 * @author Thomas Jung
 * 
 */
public class NonTinDocumentResultViewerFilter extends ViewerFilter
{
  /**
   * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object,
   *      java.lang.Object)
   */
  @Override
  public boolean select( Viewer viewer, Object parent, Object element )
  {
    if( element instanceof IDocumentResultMeta )
    {
      IDocumentResultMeta docResult = (IDocumentResultMeta) element;
      DOCUMENTTYPE documentType = docResult.getDocumentType();
      if( documentType == DOCUMENTTYPE.log || documentType == DOCUMENTTYPE.lengthSection || documentType == DOCUMENTTYPE.coreDataZip || documentType == DOCUMENTTYPE.nodes )
        return false;
      else
        return true;
    }
    else
      return true;
  }
}
