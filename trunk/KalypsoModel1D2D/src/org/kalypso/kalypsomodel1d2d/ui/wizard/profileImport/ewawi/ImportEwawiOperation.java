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
package org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ewawi;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.AbstractImportProfileOperation;
import org.kalypso.kalypsomodel1d2d.ui.wizard.profileImport.ImportProfileData;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportData;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportOperation;

/**
 * @author Holger Albert
 */
public class ImportEwawiOperation extends AbstractImportProfileOperation
{
  private final EwawiImportData m_data;

  private final ImportEwawiWorker m_worker;

  public ImportEwawiOperation( final ImportProfileData profileData, final EwawiImportData data )
  {
    super( profileData );

    m_data = data;
    m_worker = new ImportEwawiWorker( profileData.getProfNetworkColl() );
  }

  @Override
  protected IStatus execute( final ImportProfileData data, final IProgressMonitor monitor )
  {
    final EwawiImportOperation operation = new EwawiImportOperation( m_worker, m_data );
    return operation.execute( monitor );
  }

  @Override
  public IRiverProfileNetwork[] getAddedRiverNetworks( )
  {
    return m_worker.getNetworks();
  }
}