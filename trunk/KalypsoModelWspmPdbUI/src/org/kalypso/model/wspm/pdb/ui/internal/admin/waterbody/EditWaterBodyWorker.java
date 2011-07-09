/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody;

import org.apache.commons.lang.ObjectUtils;
import org.eclipse.jface.wizard.Wizard;
import org.hibernate.Session;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.connect.command.GetPdbList;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.EditWaterBodyPage.Mode;
import org.kalypso.model.wspm.pdb.ui.internal.content.ElementSelector;
import org.kalypso.model.wspm.pdb.ui.internal.content.IEditWorker;

/**
 * @author Gernot Belger
 */
public class EditWaterBodyWorker implements IEditWorker
{
  private final WaterBody m_selectedItem;

  final String windowTitle = "Edit Water Body Properties";

  private WaterBody m_clone;

  private WaterBody m_waterBodyToEdit;

  private String m_nameToSelect;

  public EditWaterBodyWorker( final WaterBody selectedItem )
  {
    m_selectedItem = selectedItem;
    m_nameToSelect = m_selectedItem.getName();
  }

  @Override
  public String getWindowTitle( )
  {
    return "Edit Water Body Properties";
  }

  @Override
  public Wizard createWizard( final Session session ) throws PdbConnectException
  {
    final WaterBody[] existingWaterbodies = GetPdbList.getArray( session, WaterBody.class );
    m_waterBodyToEdit = findWaterBody( existingWaterbodies );
    m_clone = cloneForEdit( m_waterBodyToEdit );
    return new EditWaterBodyWizard( existingWaterbodies, m_clone, Mode.EDIT );
  }

  @Override
  public void afterWizardOK( )
  {
    m_nameToSelect = m_clone.getName();
    uncloneData();
  }

  @Override
  public void addElementsToSelect( final ElementSelector selector )
  {
    selector.addWaterBodyName( m_nameToSelect );
  }

  private WaterBody findWaterBody( final WaterBody[] waterbodies )
  {
    final String name = m_selectedItem.getName();
    for( final WaterBody waterBody : waterbodies )
    {
      if( ObjectUtils.equals( waterBody.getName(), name ) )
        return waterBody;
    }
    return null;
  }

  private WaterBody cloneForEdit( final WaterBody other )
  {
    final WaterBody clone = new WaterBody( other.getId(), other.getName(), other.getLabel(), other.getDirectionOfStationing() );
    clone.setDescription( other.getDescription() );
    clone.setRiverline( other.getRiverline() );
    return clone;
  }

  /**
   * Copy the edited data back into the persistent object.
   */
  private void uncloneData( )
  {
    m_waterBodyToEdit.setName( m_clone.getName() );
    m_waterBodyToEdit.setLabel( m_clone.getLabel() );
    m_waterBodyToEdit.setDescription( m_clone.getDescription() );
    m_waterBodyToEdit.setDirectionOfStationing( m_clone.getDirectionOfStationing() );
    m_waterBodyToEdit.setRiverline( m_clone.getRiverline() );
  }
}