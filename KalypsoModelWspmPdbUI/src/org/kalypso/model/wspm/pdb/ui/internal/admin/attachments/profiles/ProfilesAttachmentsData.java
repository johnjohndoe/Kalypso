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
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.db.mapping.Document;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractAttachmentsData;
import org.kalypso.model.wspm.pdb.ui.internal.admin.attachments.AbstractAttachmentsDocumentsData;
import org.kalypso.model.wspm.tuhh.ui.utils.GuessStationPattern;

/**
 * @author Gernot Belger
 */
public class ProfilesAttachmentsData extends AbstractAttachmentsData
{
  public static final String PROPERTY_IMPORT_PATTERN = "importPattern"; //$NON-NLS-1$

  private State m_state;

  private String m_importPattern;

  public ProfilesAttachmentsData( final IPdbConnection connection )
  {
    super( connection );

    m_state = null;
    m_importPattern = String.format( "*<%s>*", GuessStationPattern.TOKEN ); //$NON-NLS-1$
  }

  public void init( final IStructuredSelection selection, final IDialogSettings settings )
  {
    load( settings );

    m_state = findState( selection );
    Assert.isNotNull( m_state );

    /* Propose a zip file name, based on state name */
    final File zipFile2 = getZipFile();
    final File zipDir = zipFile2 != null ? zipFile2.getParentFile() : FileUtils.getUserDirectory();
    final String zipName = m_state.getName() + ".zip"; //$NON-NLS-1$
    final File zipFile = new File( zipDir, zipName );
    setZipFile( zipFile );
  }

  private State findState( final IStructuredSelection selection )
  {
    if( selection.isEmpty() )
      return null;

    final Object firstElement = selection.getFirstElement();
    if( firstElement instanceof State )
      return (State)firstElement;

    return null;
  }

  public void setDbHash( final Map<BigDecimal, List<Document>> dbHash )
  {
    final ProfilesAttachmentsDocumentsData documentData = (ProfilesAttachmentsDocumentsData)getDocumentData();
    documentData.setDbHash( dbHash );
  }

  @Override
  public void load( final IDialogSettings settings )
  {
    super.load( settings );

    if( settings == null )
      return;

    /* Pattern. */
    final String pattern = settings.get( PROPERTY_IMPORT_PATTERN );
    if( !StringUtils.isBlank( pattern ) )
      m_importPattern = pattern;
  }

  @Override
  public void store( final IDialogSettings settings )
  {
    super.store( settings );

    if( settings == null )
      return;

    /* Import pattern. */
    settings.put( PROPERTY_IMPORT_PATTERN, m_importPattern );
  }

  @Override
  protected AbstractAttachmentsDocumentsData createDocumentData( )
  {
    return new ProfilesAttachmentsDocumentsData( m_state );
  }

  public State getState( )
  {
    return m_state;
  }

  public String getImportPattern( )
  {
    return m_importPattern;
  }

  public void setImportPattern( final String importPattern )
  {
    final Object oldValue = m_importPattern;
    m_importPattern = importPattern;
    firePropertyChange( PROPERTY_IMPORT_PATTERN, oldValue, importPattern );
  }
}