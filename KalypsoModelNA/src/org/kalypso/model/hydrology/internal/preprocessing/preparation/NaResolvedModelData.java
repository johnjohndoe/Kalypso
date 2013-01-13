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
package org.kalypso.model.hydrology.internal.preprocessing.preparation;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.NaCatchmentData;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.osgi.framework.Version;

/**
 * Holds alls data that is necessary to write the ascii files for the Kalypso-NA calculation core.
 * 
 * @author Gernot Belger
 */
class NaResolvedModelData implements INaPreparedData
{
  private final RelevantNetElements m_relevantElements;

  private final NaCatchmentData m_catchmentData;

  private final TimeseriesFileManager m_tsFileManager;

  private final NAControl m_metaControl;

  private final NAModellControl m_control;

  private final Node m_rootNode;

  private final NaModell m_model;

  private final IDManager m_idManager;

  private final NAOptimize m_optimize;

  private final InitialValues m_initialValues;

  private final ParameterHash m_landuseHash;

  private final Parameter m_parameter;

  private final GMLWorkspace m_synthWorkspace;

  private final Version m_calcCoreVersion;

  private final IStatus m_status;

  NaResolvedModelData( final NAModellControl control, final NAControl metaControl, final NaModell model, final InitialValues initialValue, final GMLWorkspace synthWorkspace, final NAOptimize optimize, final Parameter parameter, final ParameterHash landuseHash, final Node rootNode, final RelevantNetElements relevantElements, final NaCatchmentData catchmentData, final IDManager idManager, final Version calcCoreVersion, final TimeseriesFileManager tsFileManager, final IStatus status )
  {
    m_control = control;
    m_metaControl = metaControl;
    m_model = model;
    m_initialValues = initialValue;
    m_synthWorkspace = synthWorkspace;
    m_optimize = optimize;
    m_parameter = parameter;
    m_rootNode = rootNode;
    m_landuseHash = landuseHash;
    m_idManager = idManager;
    m_calcCoreVersion = calcCoreVersion;
    m_relevantElements = relevantElements;
    m_catchmentData = catchmentData;
    m_tsFileManager = tsFileManager;
    m_status = status;
  }

  @Override
  public IStatus getNetStatus( )
  {
    return m_status;
  }

  @Override
  public NAControl getMetaControl( )
  {
    return m_metaControl;
  }

  @Override
  public NAModellControl getControl( )
  {
    return m_control;
  }

  @Override
  public Node getRootNode( )
  {
    return m_rootNode;
  }

  @Override
  public NaModell getModel( )
  {
    return m_model;
  }

  @Override
  public IDManager getIdManager( )
  {
    return m_idManager;
  }

  @Override
  public NaCatchmentData getCatchmentData( )
  {
    return m_catchmentData;
  }

  @Override
  public NAOptimize getNaOptimize( )
  {
    return m_optimize;
  }

  @Override
  public GMLWorkspace getSynthNWorkspace( )
  {
    return m_synthWorkspace;
  }

  @Override
  public RelevantNetElements getRelevantElements( )
  {
    return m_relevantElements;
  }

  @Override
  public TimeseriesFileManager getTimeseriesManager( )
  {
    return m_tsFileManager;
  }

  @Override
  public Parameter getParameter( )
  {
    return m_parameter;
  }

  @Override
  public ParameterHash getLanduseHash( )
  {
    return m_landuseHash;
  }

  @Override
  public InitialValues getInitialValues( )
  {
    return m_initialValues;
  }

  @Override
  public Version getCalcCoreVersion( )
  {
    return m_calcCoreVersion;
  }
}