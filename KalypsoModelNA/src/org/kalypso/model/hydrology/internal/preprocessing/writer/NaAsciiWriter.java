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
package org.kalypso.model.hydrology.internal.preprocessing.writer;

import java.io.IOException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.NaAsciiDirs;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.preparation.INaPreparedData;
import org.kalypso.simulation.core.ISimulationMonitor;

/**
 * @author Gernot
 */
public class NaAsciiWriter
{
  private final INaPreparedData m_preparedData;

  private final NaAsciiDirs m_asciiDirs;

  public NaAsciiWriter( final INaPreparedData resolvedData, final NaAsciiDirs asciiDirs )
  {
    m_preparedData = resolvedData;
    m_asciiDirs = asciiDirs;
  }

  public IStatus writeBaseFiles( final ISimulationMonitor monitor ) throws IOException, NAPreprocessorException
  {
    monitor.setMessage( Messages.getString( "NAModelPreprocessor.2" ) ); //$NON-NLS-1$

    final NAControl metaControl = m_preparedData.getMetaControl();
    final NAModellControl naControl = m_preparedData.getControl();
    final Node rootNode = m_preparedData.getRootNode();
    final NaModell naModel = m_preparedData.getModel();
    final IDManager idManager = m_preparedData.getIdManager();

    final NAControlConverter naControlConverter = new NAControlConverter( metaControl, m_asciiDirs.startDir );
    naControlConverter.writeFalstart();
    naControlConverter.writeStartFile( naControl, rootNode, naModel, idManager );

    if( monitor.isCanceled() )
      throw new OperationCanceledException();

    // write net and so on....
    monitor.setMessage( Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.23" ) ); //$NON-NLS-1$

    final NAModellConverter naModellConverter = new NAModellConverter( m_preparedData, m_asciiDirs );
    return naModellConverter.writeUncalibratedFiles();
  }

  public IStatus writeCalibrationFiles( final NAOptimize optimize ) throws IOException, NAPreprocessorException
  {
    final NAModellConverter naModellConverter = new NAModellConverter( m_preparedData, m_asciiDirs );

    final CalibrationConfig config = new CalibrationConfig( optimize );
    config.applyCalibrationFactors();

    return naModellConverter.writeCalibratedFiles();
  }
}