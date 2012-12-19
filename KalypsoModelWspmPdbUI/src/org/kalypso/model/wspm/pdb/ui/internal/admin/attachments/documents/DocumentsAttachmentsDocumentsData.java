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
import java.util.Date;

import javax.activation.MimeType;

import org.apache.commons.lang3.StringUtils;
import org.apache.sanselan.formats.tiff.TiffImageMetadata;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.IDocumentContainer;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractAttachmentsDocumentsData;

/**
 * @author Gernot Belger
 * @author Holger Albert
 */
public class DocumentsAttachmentsDocumentsData extends AbstractAttachmentsDocumentsData
{
  private final IDocumentContainer m_documentContainer;

  public DocumentsAttachmentsDocumentsData( final IDocumentContainer documentContainer )
  {
    m_documentContainer = documentContainer;
  }

  public Document addDocument( final File file )
  {
    /* Create the new document. */
    final Document document = new Document();
    document.setName( file.getName() );
    document.setDescription( StringUtils.EMPTY );
    document.setMeasurementDate( new Date( file.lastModified() ) );
    document.setFilename( getFilePath( m_documentContainer, file.getName() ) );

    /* Mime type. */
    final MimeType mimeType = getMimeType( file );
    if( mimeType != null )
      document.setMimetype( mimeType.toString() );

    /* Apply the image metadata. */
    final TiffImageMetadata exif = readImageMetadata( document, file );
    if( exif != null )
      applyImageMetadata( document, exif );

    /* Set the document container reference. */
    document.setState( null );
    document.setWaterBody( null );
    document.setCrossSection( null );

    if( m_documentContainer instanceof WaterBody )
      document.setWaterBody( (WaterBody)m_documentContainer );

    if( m_documentContainer instanceof State )
      document.setState( (State)m_documentContainer );

    if( m_documentContainer instanceof CrossSection )
      document.setCrossSection( (CrossSection)m_documentContainer );

    /* Create and hash the import document info. */
    addInfo( new DocumentsDocumentInfo( document, file, m_documentContainer ) );

    return document;
  }

  private String getFilePath( final IDocumentContainer cs, final String fileName )
  {
    if( cs == null )
      return null;

    final String stateName = m_documentContainer.getName();

    return String.format( "%s/%s", stateName, fileName ); //$NON-NLS-1$
  }
}