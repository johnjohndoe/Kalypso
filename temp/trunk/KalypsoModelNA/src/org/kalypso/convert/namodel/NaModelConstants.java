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
package org.kalypso.convert.namodel;

/**
 * @author doemming
 */
public interface NaModelConstants
{
  /** namespaces */
  public static final String NS_NAMETA = "org.kalypso.na.control";

  public static final String NS_NAMODELL = "http://www.tuhh.de/kalypsoNA";

  public static final String NS_NACONTROL = "org.kalypso.namodell.control";

  public static final String NS_NAHYDROTOP = "http://www.tuhh.de/hydrotop";

  public static final String NS_NAPARAMETER = "http://www.tuhh.de/parameter";

  public static final String NS_OMBROMETER = "http://org.kalypso.ombrometer";

  public static final String NS_SYNTHN = "http://www.tuhh.de/synthN";

  public static final String NS_INIVALUES = "http://www.tuhh.de/initialValues";

  /** server client bean constants */
  // input
  public static final String IN_META_ID = "MetaSteuerdaten";

  public final static String IN_ANALYSE_MODELL_XSD_ID = "AnalyseModellXSD";

  public final static String IN_MODELL_ID = "Modell";

  public final static String IN_HYDROTOP_ID = "Hydrotop";

  public final static String IN_PARAMETER_ID = "Parameter";

  public final static String IN_CONTROL_ID = "Control";

  public final static String IN_TEMPLATE_ID = "Template";

  public final static String IN_SYNTHN_ID = "synthNiederschlag";

  public static final String IN_OPTIMIZECONF_ID = "SceConf";

  public static final String IN_KLIMA_DIR_ID = "KlimaDir";

  public static final String IN_RAINFALL_ID = "NiederschlagDir";

  public static final String IN_GAUGING_STATION_ID = "PegelDir";

  public static final String IN_RESULTS_DIR_ID = "ErgebnisDir";

  public final static String LZSIM_IN_ID = "LZSIM_IN";

  // output
  public static final String OUT_ZML = "OUT_ZML";

  public static final String OUT_OPTIMIZEFILE = "OUT_OPTIMIZEFILE";

  public static final String OUTPUT_DIR_NAME = "results";

  public final static String LZSIM_OUT_ID = "LZSIM_OUT";

  public final static String LOG_EXE_STDOUT_ID = "LOG_EXE_STDOUT";

  public static final String LOG_EXE_ERROUT_ID = "LOG_EXE_ERROUT";

  public static final String LOG_OUTRES_ID = "LOG_OUTRES";

  public static final String LOG_OUTERR_ID = "LOG_OUTERR";

  public static final String LOG_INFO_ID = "LOG_INFO";

  /** GML property constants */
  public static final String GML_FEATURE_NAME_PROP = "name";

  public static final String GML_FEATURE_DESCRIPTION_PROP = "description";

  /** hydrotop.xsd */
  public static final String HYDRO_MEMBER = "hydrotopMember";

  public static final String HYDRO_FT = "HydrotopFeatureType";

  public static final String HYDRO_PROP_SEAL_CORR_FACTOR = "corrSealing";

  public static final String HYDRO_PROP_GEOM = "position";

  public static final String HYDRO_PROP_AREA = "area";

  public static final String HYDRO_PROP_LANDUSE_NAME = "landuse";

  public static final String HYDRO_PROP_HYDTYPE = "hydType";

  public static final String HYDRO_PROP_DAINAGETYPE = "drainageType";

  public static final String HYDRO_ENUM_HYDTYPE_SWALETRENCH = "MuldenRigole";

  public static final String HYDRO_ENUM_HYDTYPE_GREENROOF = "Dachbegruenung";

  public static final String HYDRO_ENUM_HYDTYPE_REGULAR = "Bodenspeicher";

  /** namodell.xsd */
  // channels
  public static final String CHANNEL_COLLECTION_MEMBER_PROP = "ChannelCollectionMember";

  public static final String CHANNEL_MEMBER_PROP = "channelMember";

  public static final String CHANNEL_ABSTRACT_FT = "_Channel";

  public static final String CHANNEL_GEOM_PROP = "Ort";

  public static final String STORAGE_CHANNEL_ZMLINLINE_PROP = "hvvsqd";

  public static final String STORAGE_CHANNEL_VMAX_PROP = "vmax";

  public static final String STORAGE_CHANNEL_VMIN_PROP = "vmin";

  public static final String STORAGE_CHANNEL_SV_PROP = "sv";

  public static final String STORAGE_CHANNEL_ELEMENT_FT = "StorageChannel";

  public static final String KM_CHANNEL_ELEMENT_FT = "KMChannel";

  public static final String V_CHANNEL_ELEMENT_FT = "VirtualChannel";

  // nodes
  public static final String NODE_GEOM_PROP = "Ort";

  public static final String NODE_ELEMENT_FT = "Node";

  public static final String NODE_MEMBER_PROP = "nodeMember";

  public static final String NODE_COLLECTION_FT = "NodeCollection";

  public static final String NODE_COLLECTION_MEMBER_PROP = "NodeCollectionMember";

  public static final String NODE_RESULT_TIMESERIESLINK_PROP = "qberechnetZR";

  // catchments
  public static final String CATCHMENT_ELEMENT_FT = "Catchment";

  public static final String CATCHMENT_GEOM_PROP = "Ort";

  public static final String CATCHMENT_COLLECTION_MEMBER_PROP = "CatchmentCollectionMember";

  public static final String CATCHMENT_MEMBER_PROP = "catchmentMember";

  // swale and trench

  public static final String MRS_FT = "SwaleAndTrench";

  public static final String MRS_COLLECTION_FT = "SwaleAndTrenchCollection";

  public static final String MRS_COLLECTION_MEMBER_PROP = "SwaleAndTrenchCollectionMember";

  public static final String MRS_MEMBER_PROP = "swaleTrenchMember";

  public static final String MRS_GEOM_PROP = "position";

  public static final String MRS_LENGTH_PROP = "length";

  public static final String MRS_WIDTH_PROP = "widthTrench";

  public static final String MRS_SLOPE_PROP = "drainPipeSlope";

  public static final String MRS_KF_PIPE_PROP = "kfPipe";

  public static final String MRS_DIAMETER_PIPE_PROP = "diameterPipe";

  public static final String MRS_ROUGHNESS_PIPE_PROP = "roughnessPipe";

  public static final String MRS_LANDUSE_TYPE_PROP = "nutzung";

  public static final String MRS_SOIL_PROFIL_TYPE_PROP = "boden";

  public static final String MRS_MAX_PERK_PROP = "maxPerk";

  public static final String MRS_INFLOW_GW_PROP = "InflowGW";

  // link Properties
  public static final String LINK_CATCHMENT_CHANNEL = "entwaesserungsStrangMember";

  public static final String LINK_CHANNEL_DOWNSTREAMNODE = "downStreamNodeMember";

  public static final String LINK_NODE_DOWNSTREAMCHANNEL = "downStreamChannelMember";

  public static final String LINK_MRS_DISCHARGE_NODE_PROP = "dischargeNode";

  // default values
  public static final String DEFAULT_MRS_SOIL_PROFIL_PROP = "mrsp";

  // misc
  public static final String GENERATE_RESULT_PROP = "generateResult";

  /** parameter.xsd */
  public static final String PARA_LANDUSE_FT = "LanduseType";

  public static final String PARA_LANDUSE_NAME = "Landuse";

  public static final String PARA_PROP_SEALING_MEMBER = "sealingMember";

  public static final String PARA_PROP_LANDUSE_MEMBER = "landuseMember";

  public static final String PARA_LANDUSE_PROP_SEALING = "m_vers";

  public static final String PARA_LANDUSE_PROP_SEALING_LINK = "sealingLink";

  public static final String PARA_LANDUSE_PROP_LANDUSE_LINK = "idealLandUsePeriodLink";

  // default values
  public static final String DEFAULT_MRS_LANDUSE_PROP = "6";

  /** initalValues.xsd */
  public static final String INI_CATCHMENT_MEMBER_PROP = "catchmentMember";

  public static final String INI_CHANNEL_MEMBER_PROP = "channelMember";

  public static final String INI_HYD_MEMBER_PROP = "IniHyd";

  public static final String INI_HYD_FEATUREID_PROP = "featureId";

  public static final String INI_CATCHMENT_LINK_HYD_PROP = "hyd";

}