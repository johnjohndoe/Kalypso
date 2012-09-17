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

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ICheckStateProvider;
import org.kalypso.model.wspm.pdb.db.mapping.Document;

/**
 * @author Gernot Belger
 */
public class DocumentsCheckstateHandler implements ICheckStateProvider, ICheckStateListener
{
  private final CheckboxTableViewer m_viewer;

  private final AbstractAttachmentsData m_data;

  public DocumentsCheckstateHandler( final CheckboxTableViewer viewer, final AbstractAttachmentsData data )
  {
    m_viewer = viewer;
    m_data = data;
  }

  @Override
  public boolean isChecked( final Object element )
  {
    final Document doc = (Document)element;
    if( cannotImport( doc ) )
      return true;

    return m_data.isSelected( doc );
  }

  @Override
  public boolean isGrayed( final Object element )
  {
    final Document doc = (Document)element;
    return cannotImport( doc );
  }

  @Override
  public void checkStateChanged( final CheckStateChangedEvent event )
  {
    final Document doc = (Document)event.getElement();
    if( cannotImport( doc ) )
    {
      // Update in order to undo check state change
      m_viewer.update( doc, null );
      return;
    }

    if( event.getChecked() )
      m_data.selectDocument( doc );
    else
      m_data.unselectDocument( doc );
  }

  protected boolean cannotImport( final Document doc )
  {
    final AbstractAttachmentsDocumentsData documentData = m_data.getDocumentData();
    final AbstractDocumentInfo info = documentData.getInfo( doc );

    final Document[] existingDcouments = info.getExistingDocuments();
    final ImportMode importMode = m_data.getImportMode();
    /* If existing docs will be skipped we ignore elements with counterparts */
    if( importMode == ImportMode.skip && !ArrayUtils.isEmpty( existingDcouments ) )
      return true;

    return !info.isImportable();
  }

  public void selectAll( )
  {
    final Document[] documents = m_data.getDocumentData().getImportableDocuments();
    for( final Document document : documents )
    {
      if( !cannotImport( document ) )
        m_data.selectDocument( document );
    }

    m_viewer.update( documents, null );
  }

  public void unselectAll( )
  {
    m_data.clearSelection();

    final Document[] documents = m_data.getDocumentData().getDocuments();
    m_viewer.update( documents, null );
  }
}