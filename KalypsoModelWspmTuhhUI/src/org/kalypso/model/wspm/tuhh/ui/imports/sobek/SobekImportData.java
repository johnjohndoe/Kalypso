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
package org.kalypso.model.wspm.tuhh.ui.imports.sobek;

import org.eclipse.jface.dialogs.IDialogSettings;
import org.kalypso.commons.databinding.swt.FileAndHistoryData;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.ui.profil.wizard.importProfile.ImportProfileWizard;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;

/**
 * @author Gernot Belger
 */
public class SobekImportData extends AbstractModelObject
{
  private final FileAndHistoryData m_inputDir = new FileAndHistoryData( "inputDir" ); //$NON-NLS-1$

  private CommandableWorkspace m_workspace;

  private WspmWaterBody m_water;

  void init( final IDialogSettings settings, final IFeatureSelection selection )
  {
    final IFeatureSelection featureSelection = selection;
    m_water = ImportProfileWizard.findWater( featureSelection );
    m_workspace = featureSelection.getWorkspace( m_water );

    if( settings == null )
      return;

    m_inputDir.init( settings );
  }

  void storeSettings( final IDialogSettings settings )
  {
    if( settings == null )
      return;

    m_inputDir.storeSettings( settings );
  }

  public FileAndHistoryData getInputDir( )
  {
    return m_inputDir;
  }

  public WspmWaterBody getWater( )
  {
    return m_water;
  }

  public CommandableWorkspace getWorkspace( )
  {
    return m_workspace;
  }
}