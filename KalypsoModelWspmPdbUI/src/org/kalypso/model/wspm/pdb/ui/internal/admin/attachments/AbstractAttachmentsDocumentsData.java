/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
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

import javax.activation.MimeType;

import org.apache.sanselan.ImageReadException;
import org.apache.sanselan.Sanselan;
import org.apache.sanselan.common.IImageMetadata;
import org.apache.sanselan.formats.jpeg.JpegImageMetadata;
import org.apache.sanselan.formats.tiff.TiffImageMetadata;
import org.apache.sanselan.formats.tiff.constants.ExifTagConstants;
import org.kalypso.commons.image.ExifUtils;
import org.kalypso.commons.java.activation.MimeTypeFinder;
import org.kalypso.commons.java.activation.MimeTypeUtils;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Holger Albert
 */
public abstract class AbstractAttachmentsDocumentsData
{
  private final GeometryFactory m_locationFactory;

  private final Map<Document, AbstractDocumentInfo> m_infoHash;

  private final MimeTypeFinder m_mimeFinder;

  public AbstractAttachmentsDocumentsData( )
  {
    m_locationFactory = new GeometryFactory( new PrecisionModel(), 4326 ); // WGS84
    m_infoHash = new HashMap<>();
    m_mimeFinder = new MimeTypeFinder();
  }

  public AbstractDocumentInfo getInfo( final Document element )
  {
    return m_infoHash.get( element );
  }

  protected void addInfo( final AbstractDocumentInfo info )
  {
    m_infoHash.put( info.getDocument(), info );
  }

  public void clear( )
  {
    m_infoHash.clear();
  }

  public Document[] getDocuments( )
  {
    return m_infoHash.keySet().toArray( new Document[m_infoHash.size()] );
  }

  protected MimeType getMimeType( final File file )
  {
    return m_mimeFinder.getMimeType( file );
  }

  protected TiffImageMetadata readImageMetadata( final Document document, final File file )
  {
    if( !file.isFile() )
      return null;

    final String mimetype = document.getMimetype();
    final MimeType type = MimeTypeUtils.createQuietly( mimetype );
    if( type == null )
      return null;

    if( !type.getPrimaryType().equals( MimeTypeFinder.MIME_IMAGE ) )
      return null;

    try
    {
      final IImageMetadata metadata = Sanselan.getMetadata( file );
      if( metadata instanceof JpegImageMetadata )
      {
        final JpegImageMetadata jpegData = (JpegImageMetadata)metadata;
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

  protected void applyImageMetadata( final Document document, final TiffImageMetadata exif )
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

    final Date creationDate = ExifUtils.getQuietDate( exif, ExifTagConstants.EXIF_TAG_DATE_TIME_ORIGINAL );
    if( creationDate != null )
    {
      // REMARK: setting measurement date, as Document#creationDate means the database's creation date
      // REMARK: test for null, else we fall back to lastModified which was set before.
      document.setMeasurementDate( creationDate );
    }
  }

  public Document[] getImportableDocuments( )
  {
    final Collection<Document> importable = new ArrayList<>();

    for( final Entry<Document, AbstractDocumentInfo> entry : m_infoHash.entrySet() )
    {
      final Document document = entry.getKey();
      final AbstractDocumentInfo info = entry.getValue();
      if( info.isImportable() )
        importable.add( document );
    }
    return importable.toArray( new Document[importable.size()] );
  }

  public boolean isExisting( final Document document )
  {
    final AbstractDocumentInfo info = getInfo( document );
    return info.hasExistingInDatabase();
  }
}