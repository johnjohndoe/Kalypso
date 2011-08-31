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
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.activation.MimeType;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.sanselan.ImageReadException;
import org.apache.sanselan.Sanselan;
import org.apache.sanselan.common.IImageMetadata;
import org.apache.sanselan.formats.jpeg.JpegImageMetadata;
import org.apache.sanselan.formats.tiff.TiffImageMetadata;
import org.apache.sanselan.formats.tiff.constants.TiffConstants;
import org.kalypso.commons.image.ExifUtils;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;

import com.google.common.primitives.Longs;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Gernot Belger
 */
public class ImportAttachmentsDocumentsData
{
  enum ImportMode
  {
    overwrite(Messages.getString( "ImportAttachmentsDocumentsData.0" )), //$NON-NLS-1$
    skip(Messages.getString( "ImportAttachmentsDocumentsData.1" )); //$NON-NLS-1$

    private final String m_label;

    private ImportMode( final String label )
    {
      m_label = label;
    }

    @Override
    public String toString( )
    {
      return m_label;
    }
  }

  private final GeometryFactory m_locationFactory = new GeometryFactory( new PrecisionModel(), 4326 ); // WGS84

  private final Map<Document, DocumentInfo> m_infoHash = new HashMap<Document, DocumentInfo>();

  private Map<BigDecimal, CrossSection> m_csHash = null;

  private final State m_state;

  private final MimeTypeFinder m_mimeFinder = new MimeTypeFinder();

  // Encodes database id's as base64 filenames
  private final Base64 m_idEncoder = new Base64( Integer.MAX_VALUE, null, true );


  public ImportAttachmentsDocumentsData( final State state )
  {
    m_state = state;
  }

  public Document[] getDocuments( )
  {
    return m_infoHash.keySet().toArray( new Document[m_infoHash.size()] );
  }

  public void clear( )
  {
    m_infoHash.clear();
  }

  public DocumentInfo getInfo( final Document element )
  {
    return m_infoHash.get( element );
  }

// public IStatus getStatus( final Document element )
// {
// return m_statusHash.get( element );
// }
//
// public BigDecimal getStation( final Document element )
// {
// return m_stationHash.get( element );
// }
//
// public File getFile( final Document document )
// {
// return m_fileHash.get( document );
// }

  public Document addDocument( final BigDecimal station, final File file )
  {
    final String filename = file.getName();
    final MimeType mimeType = m_mimeFinder.getMimeType( file );

    final CrossSection cs = findCrossSection( station );
    final String filePath = getFilePath( cs, filename );

    /* Create the new document */
    final Document document = new Document();

    // REMARK: we set state + water body to null here: this is a profile document!
    // I.e. if the profile is removed, also this document will be destroyed which is ok.
    document.setState( null );
    document.setWaterBody( null );
    document.setCrossSection( cs );

    document.setName( filename );
    document.setDescription( StringUtils.EMPTY );
    document.setFilename( filePath );
    if( mimeType != null )
      document.setMimetype( mimeType.toString() );

    // REMARK: create date is platform dependent, is there a helper?
    final Date lastModified = new Date( file.lastModified() );
    document.setMeasurementDate( lastModified );

    final TiffImageMetadata exif = readImageMetadata( document, file );
    if( exif != null )
      applyImageMetadata( document, exif );

    final DocumentInfo info = new DocumentInfo( document, station, file );
    m_infoHash.put( document, info );

    return document;
  }

  private String getFilePath( final CrossSection cs, final String fileName )
  {
    final String stateName = m_state.getName();
    final String stateID64 = encodeID( m_state.getId().longValue() );

    final String csName = cs.getName();
    final String csID64 = encodeID( cs.getId().longValue() );

    return String.format( "%s_%s/%s_%s/%s", stateName, stateID64, csName, csID64, fileName ); //$NON-NLS-1$
  }

  private String encodeID( final long id )
  {
    final byte[] longBytes = Longs.toByteArray( id );

    return new String( m_idEncoder.encode( longBytes ) );
  }

  private CrossSection findCrossSection( final BigDecimal station )
  {
    if( m_csHash == null )
      createCsHash();

    return m_csHash.get( station );
  }

  private void createCsHash( )
  {
    m_csHash = new HashMap<BigDecimal, CrossSection>();
    final Set<CrossSection> crossSections = m_state.getCrossSections();
    for( final CrossSection crossSection : crossSections )
    {
      final BigDecimal station = crossSection.getStation().setScale( 1, BigDecimal.ROUND_HALF_UP );
      m_csHash.put( station, crossSection );
    }
  }

  private TiffImageMetadata readImageMetadata( final Document document, final File file )
  {
    if( !file.isFile() )
      return null;

    final String mimetype = document.getMimetype();
    final MimeType type = MimeTypeUtils.createQuit( mimetype );
    if( type == null )
      return null;

    if( !type.getPrimaryType().equals( MimeTypeFinder.MIME_IMAGE ) )
      return null;

    try
    {
      final IImageMetadata metadata = Sanselan.getMetadata( file );
      if( metadata instanceof JpegImageMetadata )
      {
        final JpegImageMetadata jpegData = (JpegImageMetadata) metadata;
        return jpegData.getExif();
      }
    }
    catch( final ImageReadException e )
    {
      e.printStackTrace();
    }
    catch( final IOException e )
    {
      e.printStackTrace();
    }

    return null;
  }

  private void applyImageMetadata( final Document document, final TiffImageMetadata exif )
  {
    final Coordinate crd = ExifUtils.parseLocation( exif );
    if( crd != null )
      document.setLocation( m_locationFactory.createPoint( crd ) );

    final Double directionDeegree = ExifUtils.parseDirection( exif );
    if( directionDeegree != null )
      document.setShotdirection( BigDecimal.valueOf( directionDeegree ) );

    final Double angle = ExifUtils.parseAngleOfView( exif );
    if( angle != null )
      document.setViewangle( BigDecimal.valueOf( angle ) );

    final Date creationDate = ExifUtils.getQuietDate( exif, TiffConstants.EXIF_TAG_DATE_TIME_ORIGINAL );
    if( creationDate != null )
    {
      // REMARK: setting measurement date, as Document#creationDate means the database's creation date
      // REMAR: test for null, else we fall back to lastModified which was set before.
      document.setMeasurementDate( creationDate );
    }
  }

  public Document[] getImportableDocuments( )
  {
    final Collection<Document> importable = new ArrayList<Document>();

    for( final Entry<Document, DocumentInfo> entry : m_infoHash.entrySet() )
    {
      final Document document = entry.getKey();
      final DocumentInfo info = entry.getValue();
      if( info.isImportable() )
        importable.add( document );
    }
    return importable.toArray( new Document[importable.size()] );
  }

  public boolean isExisting( final Document document )
  {
    final DocumentInfo info = getInfo( document );
    return info.hasExistingInDatabase();
  }
}