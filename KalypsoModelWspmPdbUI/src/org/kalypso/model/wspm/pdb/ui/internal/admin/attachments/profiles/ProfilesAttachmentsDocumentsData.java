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
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.activation.MimeType;

import org.apache.commons.lang3.StringUtils;
import org.apache.sanselan.formats.tiff.TiffImageMetadata;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractAttachmentsDocumentsData;

/**
 * @author Gernot Belger
 */
public class ProfilesAttachmentsDocumentsData extends AbstractAttachmentsDocumentsData
{
  private final State m_state;

  private Map<BigDecimal, List<Document>> m_dbHash;

  private Map<BigDecimal, CrossSection> m_csHash;

  public ProfilesAttachmentsDocumentsData( final State state )
  {
    m_state = state;
    m_dbHash = null;
    m_csHash = null;
  }

  public Document addDocument( final BigDecimal station, final File file )
  {
    /* Create the new document */
    final Document document = new Document();
    document.setName( file.getName() );
    document.setDescription( StringUtils.EMPTY );
    document.setMeasurementDate( new Date( file.lastModified() ) );
    document.setFilename( getFilePath( findCrossSection( station ), file.getName() ) );

    /* Mime type. */
    final MimeType mimeType = getMimeType( file );
    if( mimeType != null )
      document.setMimetype( mimeType.toString() );

    /* Apply the image metadata. */
    final TiffImageMetadata exif = readImageMetadata( document, file );
    if( exif != null )
      applyImageMetadata( document, exif );

    /* Set the document container reference. */
    // REMARK: we set state + water body to null here: this is a profile document!
    // I.e. if the profile is removed, also this document will be destroyed which is ok.
    document.setState( null );
    document.setWaterBody( null );
    document.setCrossSection( findCrossSection( station ) );

    /* Create and hash the import document info. */
    final Document[] dbDocuments = findDbDocuments( station );
    addInfo( new ProfilesDocumentInfo( document, file, station, dbDocuments ) );

    return document;
  }

  private String getFilePath( final CrossSection cs, final String fileName )
  {
    if( cs == null )
      return null;

    final String stateName = m_state.getName();

    return String.format( "%s/%s", stateName, fileName ); //$NON-NLS-1$
  }

  private CrossSection findCrossSection( final BigDecimal station )
  {
    if( m_csHash == null )
      m_csHash = createCsHash();

    return m_csHash.get( station );
  }

  private Map<BigDecimal, CrossSection> createCsHash( )
  {
    final Map<BigDecimal, CrossSection> csHash = new HashMap<>();

    final Set<CrossSection> crossSections = m_state.getCrossSections();
    for( final CrossSection crossSection : crossSections )
    {
      final BigDecimal station = crossSection.getStation();
      csHash.put( station, crossSection );
    }

    return csHash;
  }

  public void setDbHash( final Map<BigDecimal, List<Document>> dbHash )
  {
    m_dbHash = dbHash;
  }

  private Document[] findDbDocuments( final BigDecimal station )
  {
    if( m_dbHash == null )
      return new Document[] {};

    final List<Document> dbDocuments = m_dbHash.get( station );
    if( dbDocuments == null )
      return new Document[] {};

    return dbDocuments.toArray( new Document[] {} );
  }
}