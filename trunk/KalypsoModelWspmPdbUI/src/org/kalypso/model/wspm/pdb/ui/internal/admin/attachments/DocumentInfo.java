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

import java.io.File;
import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

import javax.activation.MimeType;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class DocumentInfo
{
  private final IStatus m_status;

  private final BigDecimal m_station;

  private boolean m_importable = true;

  private final File m_file;

  /**
   * All documents that have a counterpart in the database (i.e. a document on the same cross section with the same
   * filename).<br/>
   * New document -> existing document
   */
  private final Set<Document> m_existingDocuments = new HashSet<Document>();

  public DocumentInfo( final Document document, final BigDecimal station, final File file )
  {
    m_station = station;
    m_file = file;

    /* Find if document already exists in database */
    checkDocumentExists( document );

    m_status = validate( document, station );
  }

  private void checkDocumentExists( final Document newDocument )
  {
    final CrossSection crossSection = newDocument.getCrossSection();
    if( crossSection == null )
    {
      /* We cannot import anyways, so no counterpart can exist */
      return;
    }

    final String newFilename = newDocument.getFilenameName();

    final Set<Document> documents = crossSection.getDocuments();
    for( final Document document : documents )
    {
      final String existingFilename = document.getFilenameName();
      if( existingFilename != null )
      {
        if( existingFilename.equals( newFilename ) )
          m_existingDocuments.add( document );
      }
    }
  }

  private IStatus validate( final Document document, final BigDecimal station )
  {
    final IStatusCollector stati = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

    if( station == null )
    {
      stati.add( IStatus.ERROR, Messages.getString( "ImportAttachmentsDocumentsData.3" ) ); //$NON-NLS-1$
      m_importable = false;
    }
    else if( document.getCrossSection() == null )
    {
      stati.add( IStatus.ERROR, Messages.getString( "ImportAttachmentsDocumentsData.4" ) ); //$NON-NLS-1$
      m_importable = false;
    }

    final MimeType mimetype = MimeTypeUtils.createQuit( document.getMimetype() );
    if( mimetype == null )
      stati.add( IStatus.WARNING, Messages.getString( "ImportAttachmentsDocumentsData.5" ) ); //$NON-NLS-1$

    /* Already in database ? */
    if( hasExistingInDatabase() )
      stati.add( IStatus.INFO, Messages.getString( "ImportAttachmentsDocumentsData.6" ) ); //$NON-NLS-1$

    if( stati.size() == 1 )
      return stati.getAllStati()[0];
    else
      return stati.asMultiStatusOrOK( Messages.getString( "ImportAttachmentsDocumentsData.7" ) ); //$NON-NLS-1$
  }

  public boolean hasExistingInDatabase( )
  {
    return !m_existingDocuments.isEmpty();
  }

  public boolean isImportable( )
  {
    return m_importable;
  }

  public IStatus getStatus( )
  {
    return m_status;
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public File getFile( )
  {
    return m_file;
  }

  public Document[] getExistingDcouments( )
  {
    return m_existingDocuments.toArray( new Document[m_existingDocuments.size()] );
  }
}