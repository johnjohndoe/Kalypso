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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

/**
 * @author Gernot Belger
 */
public class DocumentsStatusComparator extends ViewerComparator
{
  private final AbstractAttachmentsDocumentsData m_documentData;

  public DocumentsStatusComparator( final AbstractAttachmentsDocumentsData documentData )
  {
    m_documentData = documentData;
  }

  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    final AbstractDocumentInfo info1 = m_documentData.getInfo( (Document)e1 );
    final AbstractDocumentInfo info2 = m_documentData.getInfo( (Document)e2 );

    final IStatus s1 = info1.getStatus();
    final IStatus s2 = info2.getStatus();

    final int sev1 = s1.getSeverity();
    final int sev2 = s2.getSeverity();

    if( sev1 != sev2 )
      return sev1 - sev2;

    return s1.getMessage().compareTo( s2.getMessage() );
  }
}