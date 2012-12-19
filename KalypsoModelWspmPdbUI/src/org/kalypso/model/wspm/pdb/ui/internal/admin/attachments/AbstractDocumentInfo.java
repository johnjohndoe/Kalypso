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
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

/**
 * Please call {@link #initialize()} at the end of the constructor of your extending class.
 * 
 * @author Holger Albert
 */
public abstract class AbstractDocumentInfo
{
  private final Document m_document;

  private final File m_file;

  private boolean m_importable;

  /**
   * All documents that have a counterpart in the database (i.e. a document on the same document container with the same filename).<br/>
   * New document -> existing document
   */
  private final Set<Document> m_existingDocuments;

  private IStatus m_status;

  public AbstractDocumentInfo( final Document document, final File file )
  {
    m_document = document;
    m_file = file;
    m_importable = true;
    m_existingDocuments = new HashSet<>();
    m_status = null;
  }

  protected void initialize( )
  {
    /* Find if document already exists in database. */
    checkDocumentExists( m_document );

    /* Validate and update the status. */
    m_status = validate( m_document );
  }

  protected abstract void checkDocumentExists( Document document );

  protected abstract IStatus validate( Document document );

  public Document getDocument( )
  {
    return m_document;
  }

  public File getFile( )
  {
    return m_file;
  }

  public boolean isImportable( )
  {
    return m_importable;
  }

  protected void setImportable( final boolean importable )
  {
    m_importable = importable;
  }

  public boolean hasExistingInDatabase( )
  {
    return !m_existingDocuments.isEmpty();
  }

  public Document[] getExistingDocuments( )
  {
    return m_existingDocuments.toArray( new Document[m_existingDocuments.size()] );
  }

  protected void addExistingDocument( final Document document )
  {
    m_existingDocuments.add( document );
  }

  public IStatus getStatus( )
  {
    return m_status;
  }
}