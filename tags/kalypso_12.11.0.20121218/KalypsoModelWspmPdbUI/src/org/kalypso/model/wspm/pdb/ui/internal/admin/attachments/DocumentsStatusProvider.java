/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

/**
 * @author Gernot Belger
 */
public class DocumentsStatusProvider extends ColumnLabelProvider
{
  private final AbstractAttachmentsDocumentsData m_documentData;

  public DocumentsStatusProvider( final AbstractAttachmentsDocumentsData documentData )
  {
    m_documentData = documentData;
  }

  @Override
  public String getText( final Object element )
  {
    final IStatus status = getStatus( element );
    if( status == null )
      return null;

    return status.getMessage();
  }

  @Override
  public Image getImage( final Object element )
  {
    final IStatus status = getStatus( element );
    if( status == null )
      return null;

    return StatusComposite.getStatusImage( status );
  }

  private IStatus getStatus( final Object element )
  {
    if( element instanceof Document )
    {
      final AbstractDocumentInfo info = m_documentData.getInfo( (Document)element );
      return info.getStatus();
    }

    return null;
  }
}
