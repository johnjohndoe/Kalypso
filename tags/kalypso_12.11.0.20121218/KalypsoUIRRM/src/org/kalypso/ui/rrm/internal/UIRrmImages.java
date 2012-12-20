/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.rrm.internal;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider.ImageKey;

/**
 * Convenience class for storing references to image descriptors used by the readme tool.
 */
public class UIRrmImages
{
  public static enum DESCRIPTORS implements ImageKey
  {
    CALC_CASE_FOLDER("icons/resultview/calcCase.gif"), //$NON-NLS-1$

    CATCHMENT("icons/resultview/catchment.png"), //$NON-NLS-1$
    EMPTY_CATCHMENT("icons/resultview/emptyCatchment.png"), //$NON-NLS-1$
    RESULT_CATEGORY("icons/resultview/category.png"), //$NON-NLS-1$

    DELETE("icons/timeseries/delete.gif"), //$NON-NLS-1$
    EDIT_STATION("icons/timeseries/editStation.png"), //$NON-NLS-1$
    GENERATOR_EDIT("icons/cm/generatorEdit.png"), //$NON-NLS-1$
    GENERATOR_EDIT_IDW("icons/cm/generatorEditIdw.png"), //$NON-NLS-1$
    GENERATOR_EDIT_THIESSEN("icons/cm/generatorEditThiessen.png"), //$NON-NLS-1$

    GENERATOR_LINEAR_SUM("icons/cm/generatorLinearSum.png"), //$NON-NLS-1$
    GENERATOR_MULTI("icons/cm/generatorMulti.png"), //$NON-NLS-1$
    GENERATOR_NEW_LINEAR_SUM("icons/cm/generatorNewLinearSum.png"), //$NON-NLS-1$

    TIMESERIES_MAPPING_NEW_MAPPING("icons/timeseriesMapping/newMapping.png"), //$NON-NLS-1$
    TIMESERIES_MAPPING("icons/timeseriesMapping/mapping.png"), //$NON-NLS-1$

    GENERATOR_NEW_MULTI("icons/cm/generatorNewMulti.png"), //$NON-NLS-1$
    GROUP("icons/timeseries/group.gif"), //$NON-NLS-1$
    IMPORT_TIMESERIES("icons/timeseries/importTimeseries.gif"), //$NON-NLS-1$

    MISSING_PARAMETER_INPUT_TYPE_EVAPORATION("icons/timeseries/missingInputParameterType_E.png"), //$NON-NLS-1$
    MISSING_PARAMETER_INPUT_TYPE_INFLOW("icons/timeseries/missingInputParameterType_Inflow.png"), //$NON-NLS-1$
    MISSING_PARAMETER_INPUT_TYPE_TEMPERATURE("icons/timeseries/missingInputParameterType_T.png"), //$NON-NLS-1$
    MISSING_PARAMETER_INPUT_TYPE_RAINFALL("icons/timeseries/missingInputParameterType_N.png"), //$NON-NLS-1$
    MISSING_PARAMETER_INPUT_TYPE_SEA_EVAPORATION("icons/timeseries/missingInputParameterType_E.png"), //$NON-NLS-1$

    MISSING_PARAMETER_TYPE_DISCHARGE("icons/timeseries/missingParameterType_Q.png"), //$NON-NLS-1$
    MISSING_PARAMETER_TYPE_EVAPORATION("icons/timeseries/missingParameterType_E.png"), //$NON-NLS-1$
    MISSING_PARAMETER_TYPE_RAINFALL("icons/timeseries/missingParameterType_N.png"), //$NON-NLS-1$
    MISSING_PARAMETER_TYPE_SNOW_HEIGHT("icons/parameters/missingSnow.png"), //$NON-NLS-1$
    MISSING_PARAMETER_TYPE_TEMPERATURE("icons/timeseries/missingParameterType_T.png"), //$NON-NLS-1$
    MISSING_PARAMETER_TYPE_VOLUME("icons/resultview/missingStorage.png"), //$NON-NLS-1$
    MISSING_PARAMETER_TYPE_WATERLEVEL("icons/timeseries/missingParameterType_W.png"), //$NON-NLS-1$

    NA_NODE("icons/resultview/node.png"), //$NON-NLS-1$
    EMPTY_NA_NODE("icons/resultview/emptyNode.png"), //$NON-NLS-1$

    OPEN_ERROR_GML_ACTION("icons/simulations/openErrorGmlAction.gif"), //$NON-NLS-1$
    OPEN_OUTPUT_ZML_ACTION("icons/simulations/openOutputZipAction.gif"), //$NON-NLS-1$
    OPEN_TEXT_LOG_ACTION("icons/simulations/openTextLogAction.gif"), //$NON-NLS-1$
    OPEN_STATUS_LOG_ACTION("icons/simulations/openStatusLogAction.gif"), //$NON-NLS-1$

    PARAMETER_INPUT_TYPE_EVAPORATION("icons/timeseries/inputParameterType_E.png"), //$NON-NLS-1$
    PARAMETER_INPUT_TYPE_INFLOW("icons/timeseries/inputParameterType_Inflow.png"), //$NON-NLS-1$
    PARAMETER_INPUT_TYPE_TEMPERATURE("icons/timeseries/inputParameterType_T.png"), //$NON-NLS-1$
    PARAMETER_INPUT_TYPE_RAINFALL("icons/timeseries/inputParameterType_N.png"), //$NON-NLS-1$
    PARAMETER_INPUT_TYPE_SEA_EVAPORATION("icons/timeseries/inputParameterType_E.png"), //$NON-NLS-1$
    // TODO: check using discharge icons for now
    PARAMETER_INPUT_TYPE_GAUGE("icons/timeseries/parameterType_Q.png"), //$NON-NLS-1$
    MISSING_PARAMETER_INPUT_TYPE_GAUGE("icons/timeseries/missingParameterType_Q.png"), //$NON-NLS-1$

    PARAMETER_TYPE_BASE("icons/timeseries/parameterType"), //$NON-NLS-1$
    PARAMETER_TYPE_DISCHARGE("icons/timeseries/parameterType_Q.png"), //$NON-NLS-1$
    PARAMETER_TYPE_E_LAND("icons/timeseries/parameterType_E_LAND.png"), //$NON-NLS-1$
    PARAMETER_TYPE_E_WATER("icons/timeseries/parameterType_E_WATER.png"), //$NON-NLS-1$
    PARAMETER_TYPE_EVAPORATION("icons/timeseries/parameterType_E.png"), //$NON-NLS-1$
    PARAMETER_TYPE_H_SUNSHINE("icons/timeseries/parameterType_H_SUNSHINE.png"), //$NON-NLS-1$
    PARAMETER_TYPE_HUMIDITY("icons/timeseries/parameterType_U.png"), //$NON-NLS-1$
    PARAMETER_TYPE_MEAN_HUMIDITY("icons/timeseries/parameterType_MEAN_HUMIDITY.png"), //$NON-NLS-1$
    PARAMETER_TYPE_MEAN_TEMPERATURE("icons/timeseries/parameterType_MEAN_TEMPERATURE.png"), //$NON-NLS-1$
    PARAMETER_TYPE_MEAN_WIND_VELOCITY("icons/timeseries/parameterType_MEAN_WIND_VELOCITY.png"), //$NON-NLS-1$
    PARAMETER_TYPE_RAINFALL("icons/timeseries/parameterType_N.png"), //$NON-NLS-1$
    PARAMETER_TYPE_SNOW_HEIGHT("icons/parameters/snow.png"), //$NON-NLS-1$
    PARAMETER_TYPE_SUNSHINE("icons/timeseries/parameterType_H.png"), //$NON-NLS-1$
    PARAMETER_TYPE_TEMPERATURE("icons/timeseries/parameterType_T.png"), //$NON-NLS-1$
    PARAMETER_TYPE_VELOCITY("icons/timeseries/parameterType_v.png"), //$NON-NLS-1$
    PARAMETER_TYPE_VOLUME("icons/resultview/storage.png"), //$NON-NLS-1$
    PARAMETER_TYPE_WATERLEVEL("icons/timeseries/parameterType_W.png"), //$NON-NLS-1$

    INVALID_MODEL_ELEMENT("icons/timeseries/invalidItem.png"), //$NON-NLS-1$

    SIMULATION("icons/simulations/simulation.gif"), //$NON-NLS-1$
    STATION("icons/timeseries/station.gif"), //$NON-NLS-1$
    STATION_HYDROLOGICAL("icons/timeseries/station_hydrological.png"), //$NON-NLS-1$
    STATION_METEOROLOGICAL("icons/timeseries/station_meteorological.png"), //$NON-NLS-1$
    STATION_NEW_HYDROLOGICAL("icons/timeseries/stationNewHydrological.png"), //$NON-NLS-1$
    STATION_NEW_METEOROLOGICAL("icons/timeseries/stationNewMeteorological.png"), //$NON-NLS-1$

    STORAGE_CHANNEL("icons/resultview/storage.png"), //$NON-NLS-1$
    EMPTY_STORAGE_CHANNEL("icons/resultview/missingStorage.png"), //$NON-NLS-1$

    TIMESERIES("icons/timeseries/timeseries.png"), //$NON-NLS-1$
    TIMESERIES_EXTEND("icons/timeseries/extend.png"), //$NON-NLS-1$

    TIMESERIES_EXTEND_AND_OVERWRITE("icons/timeseries/extend_and_overwrite.png"), //$NON-NLS-1$
    TIMESERIES_REPLACE("icons/timeseries/overwrite.png"), //$NON-NLS-1$
    TIMESERIES_SEARCH_CONTROL_CLEAN("icons/timeseries/clear.gif"), //$NON-NLS-1$
    TIMESERIES_TREE_COLLAPSE_ALL("icons/timeseries/collapseall.gif"), //$NON-NLS-1$
    TIMESERIES_TREE_EXPAND_ALL("icons/timeseries/expandall.gif"); //$NON-NLS-1$

    private final String m_imagePath;

    private DESCRIPTORS( final String imagePath )
    {
      m_imagePath = imagePath;
    }

    @Override
    public String getImagePath( )
    {
      return m_imagePath;
    }
  }

  public static final ImageDescriptor id( final DESCRIPTORS key )
  {
    return id( key.getImagePath() );
  }

  public static final ImageDescriptor id( final String location )
  {
    return UIRrmImages.id( KalypsoUIRRMPlugin.getID(), location );
  }

  public static final ImageDescriptor id( final String pluginID, final String location )
  {
    return AbstractUIPlugin.imageDescriptorFromPlugin( pluginID, location );
  }
}