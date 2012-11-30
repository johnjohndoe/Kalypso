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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Date;
import java.util.zip.ZipOutputStream;

import org.apache.commons.io.IOUtils;
import org.hibernate.Session;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ImportAttachmentsOperation implements IPdbOperation
{
  private final AbstractAttachmentsData m_data;

  private ZipOutputStream m_zipStream;

  private final Date m_creationDate;

  private final String m_username;

  public ImportAttachmentsOperation( final AbstractAttachmentsData data )
  {
    m_data = data;
    m_creationDate = new Date();
    m_username = m_data.getUsername();
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "ImportAttachmentsOperation.0" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    try
    {
      final Document[] documents = m_data.getImportDocuments();

      createZip();

      for( final Document document : documents )
        addDocument( session, document );

      closeZip();
    }
    finally
    {
      IOUtils.closeQuietly( m_zipStream );
    }
  }

  private void addDocument( final Session session, final Document document ) throws PdbConnectException
  {
    addToZip( document );

    final AbstractAttachmentsDocumentsData documentData = m_data.getDocumentData();
    final AbstractDocumentInfo info = documentData.getInfo( document );

    /* Delete any documents with the same file -> we overwrite them all with the new information */
    final Document[] existingDocuments = info.getExistingDocuments();
    for( final Document existingDocument : existingDocuments )
    {
      session.delete( existingDocument );
    }

    /* Save the new document */
    document.setEditingDate( m_creationDate );
    document.setEditingUser( m_username );
    document.setCreationDate( m_creationDate );

    session.save( document );
  }

  private void createZip( ) throws PdbConnectException
  {
    final File zipFile = m_data.getZipFile();
    if( zipFile == null )
      return;

    try
    {
      m_zipStream = new ZipOutputStream( new BufferedOutputStream( new FileOutputStream( zipFile ) ) );
    }
    catch( final FileNotFoundException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( Messages.getString( "ImportAttachmentsOperation.1" ), e ); //$NON-NLS-1$
    }
  }

  private void addToZip( final Document document ) throws PdbConnectException
  {
    if( m_zipStream == null )
      return;

    final AbstractAttachmentsDocumentsData documentData = m_data.getDocumentData();
    final AbstractDocumentInfo info = documentData.getInfo( document );
    final File file = info.getFile();
    try
    {
      final String path = document.getFilename();
      ZipUtilities.writeZipEntry( m_zipStream, file, path );
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      final String msg = String.format( Messages.getString( "ImportAttachmentsOperation.2" ), file.getName() ); //$NON-NLS-1$
      throw new PdbConnectException( msg, e );
    }
  }

  private void closeZip( ) throws PdbConnectException
  {
    if( m_zipStream == null )
      return;

    try
    {
      m_zipStream.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( Messages.getString( "ImportAttachmentsOperation.3" ), e ); //$NON-NLS-1$
    }
  }
}