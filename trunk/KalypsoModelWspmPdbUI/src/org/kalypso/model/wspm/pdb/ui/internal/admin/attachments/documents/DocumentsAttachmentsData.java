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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.documents;

import java.io.File;

import org.apache.commons.io.FileUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.IDocumentContainer;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractAttachmentsData;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractAttachmentsDocumentsData;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class DocumentsAttachmentsData extends AbstractAttachmentsData
{
  private IDocumentContainer m_documentContainer;

  public DocumentsAttachmentsData( final IPdbConnection connection )
  {
    super( connection );

    m_documentContainer = null;
  }

  public void init( final IStructuredSelection selection, final IDialogSettings settings )
  {
    load( settings );

    m_documentContainer = findDocumentContainer( selection );
    Assert.isNotNull( m_documentContainer );

    /* Propose a zip file name, based on state name. */
    final File zipFile2 = getZipFile();
    final File zipDir = zipFile2 != null ? zipFile2.getParentFile() : FileUtils.getUserDirectory();
    final String zipName = m_documentContainer.getName() + ".zip"; //$NON-NLS-1$
    final File zipFile = new File( zipDir, zipName );
    setZipFile( zipFile );
  }

  private IDocumentContainer findDocumentContainer( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      return null;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof IDocumentContainer )
      return (IDocumentContainer)firstElement;

    return null;
  }

  @Override
  protected AbstractAttachmentsDocumentsData createDocumentData( )
  {
    return new DocumentsAttachmentsDocumentsData( m_documentContainer );
  }

  public IDocumentContainer getDocumentContainer( )
  {
    return m_documentContainer;
  }
}