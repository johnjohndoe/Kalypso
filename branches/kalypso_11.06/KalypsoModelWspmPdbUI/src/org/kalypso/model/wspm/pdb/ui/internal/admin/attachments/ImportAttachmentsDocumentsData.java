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
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.activation.MimeType;

import org.apache.commons.lang.StringUtils;
import org.apache.sanselan.ImageReadException;
import org.apache.sanselan.Sanselan;
import org.apache.sanselan.common.IImageMetadata;
import org.apache.sanselan.formats.jpeg.JpegImageMetadata;
import org.apache.sanselan.formats.tiff.TiffImageMetadata;
import org.apache.sanselan.formats.tiff.constants.TiffConstants;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.commons.image.ExifUtils;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.WspmPdbUiPlugin;

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
    overwrite("Overwrite"),
    skip("Skip");

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

  private final Map<Document, IStatus> m_statusHash = new HashMap<Document, IStatus>();

  private final Map<Document, BigDecimal> m_stationHash = new HashMap<Document, BigDecimal>();

  private final Map<Document, Boolean> m_importableHash = new HashMap<Document, Boolean>();

  private final Map<Document, File> m_fileHash = new HashMap<Document, File>();

  private final Collection<Document> m_selectedDocuments = new HashSet<Document>();

  private Map<BigDecimal, CrossSection> m_csHash = null;

  private final State m_state;

  private final MimeTypeFinder m_mimeFinder = new MimeTypeFinder();

  private final Map<String, Document> m_existingDocumentsByName;


  public ImportAttachmentsDocumentsData( final State state, final Map<String, Document> existingDocumentsByName )
  {
    m_state = state;
    m_existingDocumentsByName = existingDocumentsByName;
  }

  public Document[] getDocuments( )
  {
    return m_stationHash.keySet().toArray( new Document[m_stationHash.size()] );
  }

  public void clear( )
  {
    m_statusHash.clear();
    m_stationHash.clear();
    m_fileHash.clear();
    m_selectedDocuments.clear();
    m_importableHash.clear();
  }

  public IStatus getStatus( final Document element )
  {
    return m_statusHash.get( element );
  }

  public BigDecimal getStation( final Document element )
  {
    return m_stationHash.get( element );
  }

  public File getFile( final Document document )
  {
    return m_fileHash.get( document );
  }

  public Document addDocument( final BigDecimal station, final File file )
  {
    final String filename = file.getName();
    final MimeType mimeType = m_mimeFinder.getMimeType( file );

    final CrossSection cs = findCrossSection( station );
    final String filePath = getFilePath( filename );

    final Document document = new Document();

    document.setState( m_state );
    document.setCrossSection( cs );
    if( cs != null )
      document.setWaterBody( cs.getWaterBody() );

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

    validate( document, station );
    if( isImportable( document ) )
      m_selectedDocuments.add( document );

    m_stationHash.put( document, station );
    m_fileHash.put( document, file );

    return document;
  }

  private String getFilePath( final String fileName )
  {
    final String stateName = m_state.getName();
    return String.format( "%s/%s", stateName, fileName );
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

  private void validate( final Document document, final BigDecimal station )
  {
    final IStatusCollector stati = new StatusCollector( WspmPdbUiPlugin.PLUGIN_ID );

    /* Default to true */
    m_importableHash.put( document, Boolean.TRUE );

    if( station == null )
    {
      stati.add( IStatus.ERROR, "Unable to determine station" );
      m_importableHash.put( document, Boolean.FALSE );
    }
    else if( document.getCrossSection() == null )
    {
      stati.add( IStatus.ERROR, "Cross section not found" );
      m_importableHash.put( document, Boolean.FALSE );
    }

    final MimeType mimetype = MimeTypeUtils.createQuit( document.getMimetype() );
    if( mimetype == null )
      stati.add( IStatus.WARNING, "Unknown mime type" );

    /* Already in database ? */
    final String name = document.getName();
    if( m_existingDocumentsByName.containsKey( name ) )
      stati.add( IStatus.WARNING, "File already exists in the database" );

    final IStatus status;
    if( stati.size() == 1 )
      status = stati.getAllStati()[0];
    else
      status = stati.asMultiStatusOrOK( "Multiple warnings (double-click" );
    m_statusHash.put( document, status );
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

  public boolean isImportable( final Document doc )
  {
    return m_importableHash.get( doc );
  }

  public boolean isSelected( final Document doc )
  {
    return m_selectedDocuments.contains( doc );
  }

  public void selectDocument( final Document doc )
  {
    m_selectedDocuments.add( doc );
  }

  public void unselectDocument( final Document doc )
  {
    m_selectedDocuments.remove( doc );
  }

  public void clearSelection( )
  {
    m_selectedDocuments.clear();
  }

  public Document[] getSelectedDocuments( final ImportMode mode )
  {
    final Collection<Document> selected = new ArrayList<Document>();

    for( final Document document : m_selectedDocuments )
    {
      final boolean exists = m_existingDocumentsByName.containsKey( document.getName() );
      if( mode == ImportMode.overwrite || !exists )
        selected.add( document );
    }

    return selected.toArray( new Document[selected.size()] );
  }
}