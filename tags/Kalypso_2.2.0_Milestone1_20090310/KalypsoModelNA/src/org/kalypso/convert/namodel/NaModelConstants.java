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

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;

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

  public static final String NS_NAFORTRANLOG = "http://www.tuhh.de/NAFortranLog";

  public static final String NS_NALANDUSE = "http://sourceforge.kalypso.org/schemata/hydrology/landuse";

  public static final String NS_NAPEDOLOGIE = "http://sourceforge.kalypso.org/schemata/hydrology/pedologie";

  public static final String NS_NAGEOLOGIE = "http://sourceforge.kalypso.org/schemata/hydrology/geologie";

  public static final String NS_NASUDS = "http://sourceforge.kalypso.org/schemata/hydrology/suds";

  /** server client bean constants */
  // input
  public static final String IN_META_ID = "MetaSteuerdaten";

  public final static String IN_ANALYSE_MODELL_XSD_ID = "AnalyseModellXSD";

  public final static String IN_MODELL_ID = "Modell";

  public final static String IN_HYDROTOP_ID = "Hydrotop";

  public final static String IN_PARAMETER_ID = "Parameter";

  public final static String IN_CONTROL_ID = "Control";

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
  public static final QName GML_FEATURE_NAME_PROP = new QName( NS.GML2, "name" );

  public static final QName GML_FEATURE_DESCRIPTION_PROP = new QName( NS.GML2, "description" );

  /** hydrotop.xsd */
  public static final QName HYDRO_MEMBER = new QName( NS_NAHYDROTOP, "hydrotopMember" );

  public static final QName HYDRO_ELEMENT_FT = new QName( NS_NAHYDROTOP, "Hydrotop" );

  public static final QName HYDRO_FT = new QName( NS_NAHYDROTOP, "HydrotopFeatureType" );

  public static final QName HYDRO_PROP_SEAL_CORR_FACTOR = new QName( NS_NAHYDROTOP, "corrSealing" );

  public static final QName HYDRO_PROP_GEOM = new QName( NS_NAHYDROTOP, "position" );

  public static final QName HYDRO_PROP_AREA = new QName( NS_NAHYDROTOP, "area" );

  public static final QName HYDRO_PROP_LANDUSE_NAME = new QName( NS_NAHYDROTOP, "landuse" );

  public static final QName HYDRO_PROP_HYDTYPE = new QName( NS_NAHYDROTOP, "hydType" );

  public static final QName HYDRO_PROP_DAINAGETYPE = new QName( NS_NAHYDROTOP, "drainageType" );

  public static final QName HYDRO_ENUM_HYDTYPE_SWALETRENCH = new QName( NS_NAHYDROTOP, "MuldenRigole" );

  public static final QName HYDRO_ENUM_HYDTYPE_GREENROOF = new QName( NS_NAHYDROTOP, "Dachbegruenung" );

  public static final QName HYDRO_ENUM_HYDTYPE_REGULAR = new QName( NS_NAHYDROTOP, "Bodenspeicher" );

  /** namodell.xsd */
  // channels
  public static final QName CHANNEL_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "ChannelCollectionMember" );

  public static final QName CHANNEL_MEMBER_PROP = new QName( NS_NAMODELL, "channelMember" );

  public static final QName CHANNEL_ABSTRACT_FT = new QName( NS_NAMODELL, "_Channel" );

  public static final QName CHANNEL_GEOM_PROP = new QName( NS_NAMODELL, "Ort" );

  public static final QName STORAGE_CHANNEL_ZMLINLINE_PROP = new QName( NS_NAMODELL, "hvvsqd" );

  public static final QName STORAGE_CHANNEL_VMAX_PROP = new QName( NS_NAMODELL, "vmax" );

  public static final QName STORAGE_CHANNEL_VMIN_PROP = new QName( NS_NAMODELL, "vmin" );

  public static final QName STORAGE_CHANNEL_SV_PROP = new QName( NS_NAMODELL, "sv" );

  public static final QName STORAGE_CHANNEL_HVVSQD_PROP = new QName( NS_NAMODELL, "hvvsqd" );

  public static final QName STORAGE_CHANNEL_ELEMENT_FT = new QName( NS_NAMODELL, "StorageChannel" );

  public static final QName KM_CHANNEL_ELEMENT_FT = new QName( NS_NAMODELL, "KMChannel" );

  public static final QName V_CHANNEL_ELEMENT_FT = new QName( NS_NAMODELL, "VirtualChannel" );

  public static final QName KM_CHANNEL_PARAMETER_FT = new QName( NS_NAMODELL, "KMParameter" );

  public static final QName KM_CHANNEL_PARAMETER_MEMBER = new QName( NS_NAMODELL, "KMParameterMember" );

  public static final QName KM_CHANNEL_FAKTOR_RKF_PROP = new QName( NS_NAMODELL, "faktorRkf" );

  public static QName KM_CHANNEL_QRK_PROP = new QName( NS_NAMODELL, "qrk" );

  public static final QName KM_CHANNEL_RKF_PROP = new QName( NS_NAMODELL, "rkf" );

  public static final QName KM_CHANNEL_RKV_PROP = new QName( NS_NAMODELL, "rkv" );

  public static final QName KM_CHANNEL_C_PROP = new QName( NS_NAMODELL, "c" );

  public static final QName KM_CHANNEL_FAKTOR_RNF_PROP = new QName( NS_NAMODELL, "faktorRnf" );

  public static final QName KM_CHANNEL_RNF_PROP = new QName( NS_NAMODELL, "rnf" );

  public static final QName KM_CHANNEL_RNV_PROP = new QName( NS_NAMODELL, "rnv" );

  public static QName KM_CHANNEL_KMSTART = new QName( NS_NAMODELL, "startkm" );

  public static QName KM_CHANNEL_KMEND = new QName( NS_NAMODELL, "endkm" );

  public static final QName IKNOT_MEMBER_PROP = new QName( NS_NAMODELL, "iknotNodeMember" );

  public static final QName DOWNSTREAM_NODE_MEMBER_PROP = new QName( NS_NAMODELL, "downStreamNodeMember" );

  // nodes
  public static final QName NODE_GEOM_PROP = new QName( NS_NAMODELL, "Ort" );

  public static final QName NODE_ELEMENT_FT = new QName( NS_NAMODELL, "Node" );

  public static final QName NODE_MEMBER_PROP = new QName( NS_NAMODELL, "nodeMember" );

  public static final QName NODE_BRANCHING_MEMBER_PROP = new QName( NS_NAMODELL, "branchingMember" );

  public static final QName NODE_BRANCHING_NODE_MEMBER_PROP = new QName( NS_NAMODELL, "branchingNodeMember" );

  public static final QName NODE_VERZW_MEMBER_PROP = new QName( NS_NAMODELL, "verzweigungNodeMember" );

  public static final QName NODE_VERZW_ENTNAHME = new QName( NS_NAMODELL, "KontEntnahme" );

  public static final QName NODE_VERZW_ZUFLUSS = new QName( NS_NAMODELL, "KontZufluss" );

  public static final QName NODE_VERZW_UEBERLAUF = new QName( NS_NAMODELL, "Ueberlauf" );

  public static final QName NODE_VERZW_VERZWEIGUNG = new QName( NS_NAMODELL, "Verzweigung" );

  public static final QName NODE_VERZW_QZUG_PROP = new QName( NS_NAMODELL, "qzug" );

  public static final QName NODE_VERZW_ZPROZ_PROP = new QName( NS_NAMODELL, "zproz" );

  public static final QName NODE_VERZW_QABG_PROP = new QName( NS_NAMODELL, "qabg" );

  public static final QName NODE_VERZW_QUEB_PROP = new QName( NS_NAMODELL, "queb" );

  public static final QName NODE_COLLECTION_FT = new QName( NS_NAMODELL, "NodeCollection" );

  public static final QName NODE_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "NodeCollectionMember" );

  public static final QName NODE_RESULT_TIMESERIESLINK_PROP = new QName( NS_NAMODELL, "qberechnetZR" );

  public static final QName NODE_ZUFLUSS_ZR_REPOSITORY_PROP = new QName( NS_NAMODELL, "zuflussZRRepository" );

  public static final QName NODE_ZUFLUSS_ZR_PROP = new QName( NS_NAMODELL, "zuflussZR" );

  public static final QName NODE_SYNTHETIC_ZUFLUSS_ZR_PROP = new QName( NS_NAMODELL, "syntheticZuflussZR" );

  public static final QName NODE_PEGEL_ZR_PROP = new QName( NS_NAMODELL, "pegelZR" );

  public static final QName NODE_RIVER_CODE_PROP = new QName( NS_NAMODELL, "riverCode" );

  public static final QName NODE_RIVER_KILOMETER_PROP = new QName( NS_NAMODELL, "riverKilometer" );

  // catchments
  public static final QName CATCHMENT_ELEMENT_FT = new QName( NS_NAMODELL, "Catchment" );

  public static final QName CATCHMENT_GEOM_PROP = new QName( NS_NAMODELL, "Ort" );

  public static final QName CATCHMENT_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "CatchmentCollectionMember" );

  public static final QName CATCHMENT_MEMBER_PROP = new QName( NS_NAMODELL, "catchmentMember" );

  public static final QName BODENKORREKTUR_MEMBER = new QName( NS_NAMODELL, "bodenkorrekturmember" );

  public static final QName GRUNDWASSERABFLUSS_MEMBER = new QName( NS_NAMODELL, "grundwasserabflussMember" );

  public static final QName STATNPARA_MEMBER = new QName( NS_SYNTHN, "statNParameterMember" );

  public static final QName STATN_PROP_XJAH = new QName( NS_SYNTHN, "xjah" );

  public static final QName CATCHMENT_PROP_NGWZU = new QName( NS_NAMODELL, "ngwzu" );

  public static final QName CATCHMENT_PROP_GWWI = new QName( NS_NAMODELL, "gwwi" );

  public static final QName CATCHMENT_PROP_ZFT = new QName( NS_NAMODELL, "zft" );

  public static final QName CATCHMENT_PROP_STATN_DIAG = new QName( NS_SYNTHN, "statNDiag" );

  public static final QName CATCHMENT_PROP_RETBAS = new QName( NS_NAMODELL, "retbas" );

  public static final QName CATCHMENT_PROP_RETINT = new QName( NS_NAMODELL, "retint" );

  public static final QName CATCHMENT_PROP_RETOB = new QName( NS_NAMODELL, "retob" );

  public static final QName CATCHMENT_PROP_RETVS = new QName( NS_NAMODELL, "retvs" );

  public static final QName CATCHMENT_PROP_RETGW = new QName( NS_NAMODELL, "retgw" );

  public static final QName CATCHMENT_PROP_RETKLU = new QName( NS_NAMODELL, "retklu" );

  public static final QName CATCHMENT_PROP_XJAH = new QName( NS_NAMODELL, "xjah" );

  public static final QName CATCHMENT_PROP_FAKTOR_RETBAS = new QName( NS_NAMODELL, "faktorRetbas" );

  public static final QName CATCHMENT_PROP_FAKTOR_RETINT = new QName( NS_NAMODELL, "faktorRetint" );

  public static final QName CATCHMENT_PROP_FAKTOR_RETOB = new QName( NS_NAMODELL, "faktorRetob" );

  public static final QName CATCHMENT_PROP_FAKTOR_RETVS = new QName( NS_NAMODELL, "faktorRetvs" );

  public static final QName CATCHMENT_PROP_FAKTOR_RETGW = new QName( NS_NAMODELL, "faktorRetgw" );

  public static final QName CATCHMENT_PROP_FAKTOR_RETKLU = new QName( NS_NAMODELL, "faktorRetklu" );

  public static final QName CATCHMENT_PROP_IZKN_VERS = new QName( NS_NAMODELL, "izkn_vers" );

  public static final QName CATCHMENT_PROP_IZKN = new QName( NS_NAMODELL, "izkn" );

  public static final QName CATCHMENT_PROP_ZR_NIEDERSCHLAG = new QName( NS_NAMODELL, "niederschlagZR" );

  public static final QName CATCHMENT_PROP_ZR_TEMPERATUR = new QName( NS_NAMODELL, "temperaturZR" );

  public static final QName CATCHMENT_PROP_ZR_VERDUNSTUNG = new QName( NS_NAMODELL, "verdunstungZR" );

  public static final QName CATCHMENT_PROP_ZR_SYNTH = new QName( NS_NAMODELL, "synthZR" );

  // swale and trench

  public static final QName MRS_FT = new QName( NS_NAMODELL, "SwaleAndTrench" );

  public static final QName MRS_COLLECTION_FT = new QName( NS_NAMODELL, "SwaleAndTrenchCollection" );

  public static final QName MRS_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "SwaleAndTrenchCollectionMember" );

  public static final QName MRS_MEMBER_PROP = new QName( NS_NAMODELL, "swaleTrenchMember" );

  public static final QName MRS_GEOM_PROP = new QName( NS_NAMODELL, "position" );

  public static final QName MRS_LENGTH_PROP = new QName( NS_NAMODELL, "length" );

  public static final QName MRS_WIDTH_PROP = new QName( NS_NAMODELL, "widthTrench" );

  public static final QName MRS_SLOPE_PROP = new QName( NS_NAMODELL, "drainPipeSlope" );

  public static final QName MRS_KF_PIPE_PROP = new QName( NS_NAMODELL, "kfPipe" );

  public static final QName MRS_DIAMETER_PIPE_PROP = new QName( NS_NAMODELL, "diameterPipe" );

  public static final QName MRS_ROUGHNESS_PIPE_PROP = new QName( NS_NAMODELL, "roughnessPipe" );

  public static final QName MRS_LANDUSE_TYPE_PROP = new QName( NS_NAMODELL, "nutzung" );

  public static final QName MRS_SOIL_PROFIL_TYPE_PROP = new QName( NS_NAMODELL, "boden" );

  public static final QName MRS_MAX_PERK_PROP = new QName( NS_NAMODELL, "maxPerk" );

  public static final QName MRS_INFLOW_GW_PROP = new QName( NS_NAMODELL, "InflowGW" );

  // link Properties
  public static final QName LINK_CATCHMENT_CHANNEL = new QName( NS_NAMODELL, "entwaesserungsStrangMember" );

  public static final QName LINK_CHANNEL_DOWNSTREAMNODE = new QName( NS_NAMODELL, "downStreamNodeMember" );

  public static final QName LINK_NODE_DOWNSTREAMCHANNEL = new QName( NS_NAMODELL, "downStreamChannelMember" );

  public static final QName LINK_MRS_DISCHARGE_NODE_PROP = new QName( NS_NAMODELL, "dischargeNode" );

  // default values
  public static final QName DEFAULT_MRS_SOIL_PROFIL_PROP = new QName( NS_NAMODELL, "mrsp" );

  // misc
  public static final QName GENERATE_RESULT_PROP = new QName( NS_NAMODELL, "generateResult" );

  public static final QName NA_MODEL_FLAECH_PROP = new QName( NS_NAMODELL, "flaech" );

  public static final QName NA_MODEL_ROOT_FT = new QName( NS_NAMODELL, "NaModell" );

  public static final QName NA_CATCHMENT_COLLECTION_FT = new QName( NS_NAMODELL, "CatchmentCollection" );

  public static final QName NA_CHANNEL_COLLECTION_FT = new QName( NS_NAMODELL, "ChannelCollection" );

  /** parameter.xsd */
  public static final QName PARA_ROOT_FT = new QName( NS_NAPARAMETER, "Parameter" );

  public static final QName PARA_LANDUSE_FT = new QName( NS_NAPARAMETER, "LanduseType" );

  public static final QName PARA_LANDUSE = new QName( NS_NAPARAMETER, "Landuse" );

  public static final QName PARA_SEALING = new QName( NS_NAPARAMETER, "Sealing" );

  public static final QName PARA_Soiltype_FT = new QName( NS_NAPARAMETER, "Soiltype" );

  public static final QName PARA_SoilLayerParameter_FT = new QName( NS_NAPARAMETER, "SoilLayerParameter" );

  public static final QName PARA_SOIL_LAYER_PARAMETER_MEMBER = new QName( NS_NAPARAMETER, "soilLayerParameterMember" );

  public static final QName PARA_LANDUSE_NAME = new QName( NS_NAPARAMETER, "Landuse" );

  public static final QName PARA_SNOW_NAME = new QName( NS_NAPARAMETER, "Snow" );

  public static final QName PARA_SoilLayer_FT = new QName( NS_NAPARAMETER, "SoilLayer" );

  public static final QName PARA_PROP_SEALING_MEMBER = new QName( NS_NAPARAMETER, "sealingMember" );

  public static final QName PARA_PROP_LANDUSE_MEMBER = new QName( NS_NAPARAMETER, "landuseMember" );

  public static final QName PARA_PROP_SNOW_MEMBER = new QName( NS_NAPARAMETER, "snowMember" );

  public static final QName PARA_IDEAL_LANDUSE_MEMBER = new QName( NS_NAPARAMETER, "idealLandUseMember" );

  public static final QName PARA_IDEAL_LANDUSE = new QName( NS_NAPARAMETER, "IdealLandUse" );

  public static final QName PARA_IDEAL_LANDUSE_ZML = new QName( NS_NAPARAMETER, "idealLandUseZML" );

  public static final QName PARA_SOILTYPE_MEMBER = new QName( NS_NAPARAMETER, "soiltypeMember" );

  public static final QName PARA_LANDUSE_PROP_SEALING = new QName( NS_NAPARAMETER, "m_vers" );

  public static final QName PARA_LANDUSE_PROP_SEALING_LINK = new QName( NS_NAPARAMETER, "sealingLink" );

  public static final QName PARA_LANDUSE_PROP_LANDUSE_LINK = new QName( NS_NAPARAMETER, "idealLandUsePeriodLink" );

  public static final QName PARA_SOIL_LAYER_MEMBER = new QName( NS_NAPARAMETER, "soilLayerMember" );

  public static final QName PARA_SOIL_LAYER_LINK = new QName( NS_NAPARAMETER, "soilLayerLink" );

  public static final QName PARA_PROP_XRET = new QName( NS_NAPARAMETER, "xret" );

  // default values
  public static final QName DEFAULT_MRS_LANDUSE_PROP = new QName( NS_NAPARAMETER, "15" ); // Nutzung Grünland

  /** initalValues.xsd */
  public static final QName INI_CATCHMENT_MEMBER_PROP = new QName( NS_INIVALUES, "catchmentMember" );

  public static final QName INI_CHANNEL_MEMBER_PROP = new QName( NS_INIVALUES, "channelMember" );

  public static final QName INI_HYD_MEMBER_PROP = new QName( NS_INIVALUES, "IniHyd" );

  public static final QName INI_HYD_FEATUREID_PROP = new QName( NS_INIVALUES, "featureId" );

  public static final QName INI_CATCHMENT_LINK_HYD_PROP = new QName( NS_INIVALUES, "hyd" );

  /** control.xsd */
  public static final QName CONTROL_STARTSIMULATION = new QName( NS_NAMETA, "startsimulation" );

  public static final QName CONTROL_FORECAST = new QName( NS_NAMETA, "startforecast" );

  public static final QName CONTROL_SCENARIO_ID_PROP = new QName( NS_NAMETA, "scenarioId" );

  public static final QName CONTROL_HOURS_FORECAST_PROP = new QName( NS_NAMETA, "hoursforecast" );

  public static final QName CONTROL_MINUTES_TIMESTEP_PROP = new QName( NS_NAMETA, "minutesTimestep" );

  public static final QName CONTROL_VERSION_KALYPSONA_PROP = new QName( NS_NAMETA, "versionKalypsoNA" );

  public static final QName CONTROL_PNS_PROP = new QName( NS_NAMETA, "pns" );

  public static final QName CONTROL_XJAH_PROP = new QName( NS_NAMETA, "xjah" );

  public static final QName CONTROL_XWAHL2_PROP = new QName( NS_NAMETA, "xwahl2" );

  public static final QName CONTROL_IPVER_PROP = new QName( NS_NAMETA, "ipver" );

  /** synthN.xsd */
  public static final QName SYNTHN_STATN_FT = new QName( NS_SYNTHN, "StatN" );

  /** nacontrol.xsd */
  public static final QName NACONTROL_INITIALVALUEDATE_PROP = new QName( NS_NACONTROL, "InitialValueDate" );

  public static final QName NACONTROL_WRITE_PROP = new QName( NS_NACONTROL, "write" );

  public static final QName NACONTROL_INITIALDATE_PROP = new QName( NS_NACONTROL, "initialDate" );

  public static final QName NACONTROL_TMP_PROP = new QName( NS_NACONTROL, "tmp" );

  public static final QName NACONTROL_PRE_PROP = new QName( NS_NACONTROL, "pre" );

  public static final QName NACONTROL_SCH_PROP = new QName( NS_NACONTROL, "sch" );

  public static final QName NACONTROL_BOF_PROP = new QName( NS_NACONTROL, "bof" );

  public static final QName NACONTROL_BSP_PROP = new QName( NS_NACONTROL, "bsp" );

  public static final QName NACONTROL_GWS_PROP = new QName( NS_NACONTROL, "gws" );

  public static final QName NACONTROL_QGS_PROP = new QName( NS_NACONTROL, "qgs" );

  public static final QName NACONTROL_QGG_PROP = new QName( NS_NACONTROL, "qgg" );

  public static final QName NACONTROL_QNA_PROP = new QName( NS_NACONTROL, "qna" );

  public static final QName NACONTROL_QIF_PROP = new QName( NS_NACONTROL, "qif" );

  public static final QName NACONTROL_QVS_PROP = new QName( NS_NACONTROL, "qvs" );

  public static final QName NACONTROL_QBS_PROP = new QName( NS_NACONTROL, "qbs" );

  public static final QName NACONTROL_QT1_PROP = new QName( NS_NACONTROL, "qt1" );

  public static final QName NACONTROL_QTG_PROP = new QName( NS_NACONTROL, "qtg" );

  public static final QName NACONTROL_QGW_PROP = new QName( NS_NACONTROL, "qgw" );

  public static final QName NACONTROL_VET_PROP = new QName( NS_NACONTROL, "vet" );

  public static final QName NACONTROL_QMR_PROP = new QName( NS_NACONTROL, "qmr" );

  public static final QName NACONTROL_HYD_PROP = new QName( NS_NACONTROL, "hyd" );

  public static final QName NACONTROL_BIL_PROP = new QName( NS_NACONTROL, "bil" );

  public static final QName NACONTROL_NMQ_PROP = new QName( NS_NACONTROL, "nmq" );

  public static final QName NACONTROL_SPI_PROP = new QName( NS_NACONTROL, "spi" );

  public static final QName NACONTROL_SUP_PROP = new QName( NS_NACONTROL, "sup" );

  public static final QName NACONTROL_ROOTNODE_PROP = new QName( NS_NACONTROL, "rootNode" );

  public static final QName NACONTROL_AUTOCALI_PROP = new QName( NS_NACONTROL, "automaticCallibration" );

  public static final QName NACONTROL_ACCPRED_PROP = new QName( NS_NACONTROL, "accuracyPrediction" );

  public static final QName NACONTROL_USEOFFSTARTPRED_PROP = new QName( NS_NACONTROL, "useOffsetStartPrediction" );

  public static final QName NACONTROL_USEOFFENDPRED_PROP = new QName( NS_NACONTROL, "useOffsetEndPrediction" );

  public static final QName NACONTROL_USE_RESULTS_PROP = new QName( NS_NACONTROL, "useResults" );


}