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
package org.kalypso.services.metadoc;

import java.util.HashMap;
import java.util.Map;

/**
 * @author schlienger
 */
public class DocumentBean
{
  private Map<Object, Object> m_metadata;

  private String m_preferredFilename;

  private String m_documentIdentifier;

  private String m_documentCategory;

  private Map<Object, Object> m_metadataExtensions;

  public DocumentBean( )
  {
    m_metadata = new HashMap<Object, Object>();
    m_preferredFilename = "";
    m_documentIdentifier = "";
    m_documentCategory = "";
    m_metadataExtensions = new HashMap<Object, Object>();
  }

  /**
   * Constructor
   * 
   * @param metadata
   *          describes the document, simple mapping between keys and values
   * @param preferredFilename
   *          the preferred file name for the underlying file. Kalypso will try to preserve it, but no guarantee is
   *          made. It is possible that extra characters are appended to it so that its uniqueness is guaranteed.
   * @param documentIdentifier
   *          identifies the document uniquely amongst the documents that can be exported. This information might be
   *          used by IMetaDocCommiters by including it in the metadata. Third party software (such as an Information
   *          Management System) which stores the document can use this identifier along with other metadata to
   *          overwrite documents which are already present for the same forecast for instance
   * @param documentCategory
   *          specifies into which category this document should be classified. This information can be used by the
   *          IMetaDocCommiters. Third party software (such as an Information Management System) which stores the
   *          document can use the category as a folder-name into which the documents can be found.
   * @param metadataExtensions
   *          [optional, can be null] additional metadata information that might be used by the document commiter. It
   *          can be wrapped by a {@link org.apache.commons.configuration.MapConfiguration}to read the properties from
   *          it.
   */
  public DocumentBean( final Map<Object, Object> metadata, final String prefFileName, final String docId, final String docCat, final Map<Object, Object> metadataEx )
  {
    m_metadata = metadata;
    m_preferredFilename = prefFileName;
    m_documentIdentifier = docId;
    m_documentCategory = docCat;
    m_metadataExtensions = metadataEx;
  }

  public String getDocumentCategory( )
  {
    return m_documentCategory;
  }

  public void setDocumentCategory( String documentCategory )
  {
    m_documentCategory = documentCategory;
  }

  public String getDocumentIdentifier( )
  {
    return m_documentIdentifier;
  }

  public void setDocumentIdentifier( String documentIdentifier )
  {
    m_documentIdentifier = documentIdentifier;
  }

  public Map<Object, Object> getMetadata( )
  {
    return m_metadata;
  }

  public void setMetadata( Map<Object, Object> metadata )
  {
    m_metadata = metadata;
  }

  public Map<Object, Object> getMetadataExtensions( )
  {
    return m_metadataExtensions;
  }

  public void setMetadataExtensions( Map<Object, Object> metadataExtensions )
  {
    m_metadataExtensions = metadataExtensions;
  }

  public String getPreferredFilename( )
  {
    return m_preferredFilename;
  }

  public void setPreferredFilename( String preferredFilename )
  {
    m_preferredFilename = preferredFilename;
  }
}
