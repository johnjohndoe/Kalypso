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
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.NAOptimize;
import org.kalypso.model.hydrology.binding.control.NAControl;
import org.kalypso.model.hydrology.binding.control.NAModellControl;
import org.kalypso.model.hydrology.binding.initialValues.InitialValues;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.NaModell;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.model.hydrology.internal.IDManager;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.model.hydrology.internal.preprocessing.NAPreprocessorException;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ICatchmentInfos;
import org.kalypso.model.hydrology.internal.preprocessing.hydrotope.ParameterHash;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.osgi.framework.Version;

/**
 * @author Gernot Belger
 */
public final class NaPreprocessingPreparator
{
  public static INaPreparedData prepareData( final NAModellControl control, final NAControl metaControl, final NaModell model, final InitialValues initialValue, final GMLWorkspace synthWorkspace, final NAOptimize optimize, final Parameter parameter, final ParameterHash landuseHash, final Node rootNode, final ICatchmentInfos catchmentData, final IDManager idManager, final Version calcCoreVersion ) throws NAPreprocessorException
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final NetFileAnalyser nodeManager = new NetFileAnalyser( rootNode, model, idManager, log );
    final RelevantNetElements relevantElements = nodeManager.analyseNet();

    /* restrict catchment data to relevant elements */
    final Catchment[] relevantCatchments = relevantElements.getCatchmentsSorted( idManager );
    final ICatchmentInfos relevantInfos = new RelevantCatchmentInfos( relevantCatchments, catchmentData );

    final boolean usePrecipitationForm = metaControl.isUsePrecipitationForm();
    final TimeseriesFileManager tsFileManager = new TimeseriesFileManager( idManager, usePrecipitationForm );

    final IStatus status = log.asMultiStatusOrOK( Messages.getString( "NaResolvedModelData_0" ) ); //$NON-NLS-1$

    return new NaResolvedModelData( control, metaControl, model, initialValue, synthWorkspace, optimize, parameter, landuseHash, rootNode, relevantElements, relevantInfos, idManager, calcCoreVersion, tsFileManager, status );
  }
}