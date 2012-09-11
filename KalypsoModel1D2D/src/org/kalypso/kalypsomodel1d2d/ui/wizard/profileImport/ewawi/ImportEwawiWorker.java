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

import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetwork;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.IRiverProfileNetworkCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.ewawi.data.EwawiPro;
import org.kalypso.model.wspm.ewawi.data.EwawiSta;
import org.kalypso.model.wspm.ewawi.utils.GewShape;
import org.kalypso.model.wspm.ewawi.utils.profiles.EwawiProfile;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.AbstractEwawiWorker;
import org.kalypso.model.wspm.tuhh.ui.imports.ewawi.EwawiImportData;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Holger Albert
 */
public class ImportEwawiWorker extends AbstractEwawiWorker
{
  private final IRiverProfileNetworkCollection m_profNetworkColl;

  private final List<Feature> m_terrainModelAdds;

  public ImportEwawiWorker( final IRiverProfileNetworkCollection profNetworkColl, final List<Feature> terrainModelAdds )
  {
    m_profNetworkColl = profNetworkColl;
    m_terrainModelAdds = terrainModelAdds;
  }

  @Override
  public void updateClassifications( )
  {
    /* Nothing to do. */
  }

  @Override
  public IProfileFeature createNewProfile( final EwawiImportData data, final GewShape gewShape, final EwawiSta staIndex, final EwawiProfile ewawiProfile ) throws CoreException
  {
    return null;
  }

  @Override
  public void createMarkers( final IProfileFeature profileFeature )
  {
  }

  @Override
  public void updateWaterLevelFixation( final EwawiSta staIndex, final EwawiPro proIndex )
  {
    /* Nothing to do. */
  }

  @Override
  public void fireChangeEvents( )
  {
  }

  public IRiverProfileNetworkCollection getProfNetworkColl( )
  {
    return m_profNetworkColl;
  }

  public List<Feature> getTerrainModelAdds( )
  {
    return m_terrainModelAdds;
  }

  public IRiverProfileNetwork getNetwork( )
  {
    // TODO
    return null;
  }
}