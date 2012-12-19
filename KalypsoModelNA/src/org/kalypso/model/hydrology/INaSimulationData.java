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
package org.kalypso.model.hydrology;

import java.net.URL;

import org.kalypso.model.hydrology.binding.HydrotopeCollection;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.internal.NaOptimizeData;
import org.kalypso.model.hydrology.internal.binding.cm.CatchmentModel;
import org.kalypso.model.hydrology.internal.binding.timeseriesMappings.TimeseriesMappingCollection;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.IFeatureProviderFactory;

/**
 * @author Gernot Belger
 */
public interface INaSimulationData
{
  void dispose( );

  NAModellControl getNaControl( );

  GMLWorkspace getModelWorkspace( );

  NaModell getNaModel( );

  NAControl getMetaControl( );

  void setMetaControl( NAControl metaControl );

  GMLWorkspace getParameterWorkspace( );

  GMLWorkspace getSynthNWorkspace( );

  HydrotopeCollection getHydrotopCollection( );

  InitialValues getInitialValues( );

  void setLzsimWorkspace( GMLWorkspace lzsimWorkspace );

  NaOptimizeData getOptimizeData( );

  NAOptimize getNaOptimize( );

  URL getPreprocessedASCII( );

  CatchmentModel getCatchmentModels( );

  TimeseriesMappingCollection getTimeseriesMappings( );

  IFeatureProviderFactory getFeatureProviderFactory( );
}