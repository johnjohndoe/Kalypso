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
package org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.profiles;

import java.io.File;
import java.math.BigDecimal;

import javax.activation.MimeType;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.java.activation.MimeTypeUtils;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractDocumentInfo;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class ProfilesDocumentInfo extends AbstractDocumentInfo
{
  /**
   * The station.
   */
  private final BigDecimal m_station;

  /**
   * This documents in the database share the same beginning of the path and the same station.
   */
  private final Document[] m_dbDocuments;

  public ProfilesDocumentInfo( final Document document, final File file, final BigDecimal station, final Document[] dbDocuments )
  {
    super( document, file );

    m_station = station;
    m_dbDocuments = dbDocuments;

    initialize();
  }

  @Override
  protected void checkDocumentExists( final Document newDocument )
  {
    final CrossSection crossSection = newDocument.getCrossSection();
    if( crossSection == null )
    {
      /* We cannot import anyways, so no counterpart can exist. */
      return;
    }

    final String newFilename = newDocument.getFilenameName();

    // REMARK: This executes a query for every cross section.
    // final Set<Document> documents = crossSection.getDocuments();
    for( final Document document : m_dbDocuments )
    {
      final String existingFilename = document.getFilenameName();
      if( existingFilename != null )
      {
        if( existingFilename.equals( newFilename ) )
          addExistingDocument( document );
      }
    }
  }

  @Override
  protected IStatus validate( final Document document )
  {
    final IStatusCollector stati = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

    if( m_station == null )
    {
      stati.add( IStatus.ERROR, Messages.getString( "ImportAttachmentsDocumentsData.3" ) ); //$NON-NLS-1$
      setImportable( false );
    }
    else if( document.getCrossSection() == null )
    {
      stati.add( IStatus.ERROR, Messages.getString( "ImportAttachmentsDocumentsData.4" ) ); //$NON-NLS-1$
      setImportable( false );
    }

    final MimeType mimetype = MimeTypeUtils.createQuietly( document.getMimetype() );
    if( mimetype == null )
      stati.add( IStatus.WARNING, Messages.getString( "ImportAttachmentsDocumentsData.5" ) ); //$NON-NLS-1$

    /* Already in database? */
    if( hasExistingInDatabase() )
      stati.add( IStatus.INFO, Messages.getString( "ImportAttachmentsDocumentsData.6" ) ); //$NON-NLS-1$

    if( stati.size() == 1 )
      return stati.getAllStati()[0];
    else
      return stati.asMultiStatusOrOK( Messages.getString( "ImportAttachmentsDocumentsData.7" ) ); //$NON-NLS-1$
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }
}