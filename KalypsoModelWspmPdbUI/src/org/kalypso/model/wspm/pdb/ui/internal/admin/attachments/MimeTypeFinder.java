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
import java.util.HashMap;
import java.util.Map;

import javax.activation.MimeType;
import javax.activation.MimeTypeParseException;

import org.apache.commons.io.FilenameUtils;

/**
 * Helps finding the mime type of a file.<br/>
 * TODO: move into commons
 * 
 * @author Gernot Belger
 */
public class MimeTypeFinder
{
  public final static String MIME_IMAGE = "image"; //$NON-NLS-1$

  public final static String MIME_APPLICATION = "application"; //$NON-NLS-1$

  public final static String MIME_AUDIO = "audio"; //$NON-NLS-1$

  public final static String MIME_TEXT = "text"; //$NON-NLS-1$

  public final static String MIME_VIDEO = "video"; //$NON-NLS-1$

  private final Map<String, MimeType> m_knownTypes = new HashMap<String, MimeType>();

  public MimeTypeFinder( )
  {
    try
    {
      m_knownTypes.put( "dwg", new MimeType( MIME_APPLICATION, "acad" ) ); // AutoCAD-Dateien (nach NCSA) //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "dxf", new MimeType( MIME_APPLICATION, "dxf" ) ); // AutoCAD-Dateien (nach CERN) //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "gz", new MimeType( MIME_APPLICATION, "gzip" ) ); // GNU Zip-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "xls", new MimeType( MIME_APPLICATION, "msexcel" ) ); // Microsoft Excel Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "ppt", new MimeType( MIME_APPLICATION, "mspowerpoint" ) ); // Microsoft Powerpoint Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "doc", new MimeType( MIME_APPLICATION, "msword" ) ); // Microsoft Word Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "pdf", new MimeType( MIME_APPLICATION, "pdf" ) ); // Adobe PDF-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "ps", new MimeType( MIME_APPLICATION, "postscript" ) ); // Adobe PostScript-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "eps", new MimeType( MIME_APPLICATION, "postscript" ) ); // Adobe PostScript-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "rtf", new MimeType( MIME_APPLICATION, "rtf" ) ); // Microsoft RTF-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "htm", new MimeType( MIME_APPLICATION, "xhtml+xml" ) ); // XHTML-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "html", new MimeType( MIME_APPLICATION, "xhtml+xml" ) ); // XHTML-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "xhtml", new MimeType( MIME_APPLICATION, "xhtml+xml" ) ); // XHTML-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "xml", new MimeType( MIME_APPLICATION, "xml" ) ); // XML-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "gtar", new MimeType( MIME_APPLICATION, "x-gtar" ) ); // GNU tar-Archivdateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "swf", new MimeType( MIME_APPLICATION, "x-shockwave-flash" ) ); // Flash Shockwave-Dateien //$NON-NLS-1$ //$NON-NLS-2$

      m_knownTypes.put( "tar", new MimeType( MIME_APPLICATION, "x-tar" ) ); // tar-Archivdateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "zip", new MimeType( MIME_APPLICATION, "zip" ) ); // ZIP-Archivdateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "mp2", new MimeType( MIME_AUDIO, "x-mpeg" ) ); // MPEG-Dateien //$NON-NLS-1$ //$NON-NLS-2$

      m_knownTypes.put( "jpg", new MimeType( MIME_IMAGE, "jpg" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "jpeg", new MimeType( MIME_IMAGE, "jpg" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "jpe", new MimeType( MIME_IMAGE, "jpg" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "gif", new MimeType( MIME_IMAGE, "gif" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "png", new MimeType( MIME_IMAGE, "png" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "tiff", new MimeType( MIME_IMAGE, "tiff" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "tif", new MimeType( MIME_IMAGE, "tiff" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "bmp", new MimeType( MIME_IMAGE, "bmp" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "ico", new MimeType( MIME_IMAGE, "ico" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "ico", new MimeType( MIME_IMAGE, "x-icon" ) ); // Icon-Dateien (z.B. Favoriten-Icons) //$NON-NLS-1$ //$NON-NLS-2$

      m_knownTypes.put( "csv", new MimeType( MIME_TEXT, "comma-separated-values" ) ); // kommaseparierte //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "txt", new MimeType( MIME_TEXT, "plain" ) ); // *.txt reine Textdateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "tsv", new MimeType( MIME_TEXT, "tab-separated-values" ) ); // *.tsv tabulator-separierte //$NON-NLS-1$ //$NON-NLS-2$

      m_knownTypes.put( "xml", new MimeType( MIME_TEXT, "xml" ) ); // XML-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "mpeg", new MimeType( MIME_VIDEO, "mpeg" ) ); // MPEG-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "mpg", new MimeType( MIME_VIDEO, "mpeg" ) ); // MPEG-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "mpe", new MimeType( MIME_VIDEO, "mpeg" ) ); // MPEG-Dateien //$NON-NLS-1$ //$NON-NLS-2$
      m_knownTypes.put( "avi", new MimeType( MIME_VIDEO, "x-msvideo" ) ); // Microsoft AVI-Dateien //$NON-NLS-1$ //$NON-NLS-2$
    }
    catch( final MimeTypeParseException e )
    {
      e.printStackTrace();
    }
  }

  public MimeType getMimeType( final File file )
  {
    final String extension = FilenameUtils.getExtension( file.getName() );
    return m_knownTypes.get( extension.toLowerCase() );
  }
}