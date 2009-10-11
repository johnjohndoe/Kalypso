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
  public static final String NS_NAMETA = "org.kalypso.na.control"; //$NON-NLS-1$

  public static final String NS_NAMODELL = "http://www.tuhh.de/kalypsoNA"; //$NON-NLS-1$

  public static final String NS_NACONTROL = "org.kalypso.namodell.control"; //$NON-NLS-1$

  public static final String NS_NAHYDROTOP = "http://www.tuhh.de/hydrotop"; //$NON-NLS-1$

  public static final String NS_NAPARAMETER = "http://www.tuhh.de/parameter"; //$NON-NLS-1$

  public static final String NS_OMBROMETER = "http://org.kalypso.ombrometer"; //$NON-NLS-1$

  public static final String NS_SYNTHN = "http://www.tuhh.de/synthN"; //$NON-NLS-1$

  public static final String NS_INIVALUES = "http://www.tuhh.de/initialValues"; //$NON-NLS-1$

  public static final String NS_NAFORTRANLOG = "http://www.tuhh.de/NAFortranLog"; //$NON-NLS-1$

  public static final String NS_NALANDUSE = "http://sourceforge.kalypso.org/schemata/hydrology/landuse"; //$NON-NLS-1$

  public static final String NS_NAPEDOLOGIE = "http://sourceforge.kalypso.org/schemata/hydrology/pedologie"; //$NON-NLS-1$

  public static final String NS_NAGEOLOGIE = "http://sourceforge.kalypso.org/schemata/hydrology/geologie"; //$NON-NLS-1$

  public static final String NS_NASIMPLESHAPECATCHMENT = "http://sourceforge.kalypso.org/schemata/hydrology/simpleShapeCatchment"; //$NON-NLS-1$

  public static final String NS_NASUDS = "http://sourceforge.kalypso.org/schemata/hydrology/suds"; //$NON-NLS-1$

  /** server client bean constants */
  // input
  public static final String IN_META_ID = "MetaSteuerdaten"; //$NON-NLS-1$

  public static final String IN_ANALYSE_MODELL_XSD_ID = "AnalyseModellXSD"; //$NON-NLS-1$

  public static final String IN_MODELL_ID = "Modell"; //$NON-NLS-1$

  public static final String IN_HYDROTOP_ID = "Hydrotop"; //$NON-NLS-1$

  public static final String IN_PARAMETER_ID = "Parameter"; //$NON-NLS-1$

  public static final String IN_LANDUSE_ID = "Landuse"; //$NON-NLS-1$

  public static final String IN_SUDS_ID = "Suds"; //$NON-NLS-1$

  public static final String IN_CONTROL_ID = "Control"; //$NON-NLS-1$

  public static final String IN_SYNTHN_ID = "synthNiederschlag"; //$NON-NLS-1$

  public static final String IN_OPTIMIZECONF_ID = "SceConf"; //$NON-NLS-1$

  public static final String IN_KLIMA_DIR_ID = "KlimaDir"; //$NON-NLS-1$

  public static final String IN_RAINFALL_ID = "NiederschlagDir"; //$NON-NLS-1$

  public static final String IN_GAUGING_STATION_ID = "PegelDir"; //$NON-NLS-1$

  public static final String IN_RESULTS_DIR_ID = "ErgebnisDir"; //$NON-NLS-1$

  public static final String LZSIM_IN_ID = "LZSIM_IN"; //$NON-NLS-1$

  // output
  public static final String OUT_ZML = "OUT_ZML"; //$NON-NLS-1$

  public static final String OUT_OPTIMIZEFILE = "OUT_OPTIMIZEFILE"; //$NON-NLS-1$

  public static final String OUTPUT_DIR_NAME = "results"; //$NON-NLS-1$

  public static final String LZSIM_OUT_ID = "LZSIM_OUT"; //$NON-NLS-1$

  public static final String LOG_EXE_STDOUT_ID = "LOG_EXE_STDOUT"; //$NON-NLS-1$

  public static final String LOG_EXE_ERROUT_ID = "LOG_EXE_ERROUT"; //$NON-NLS-1$

  public static final String LOG_OUTRES_ID = "LOG_OUTRES"; //$NON-NLS-1$

  public static final String LOG_OUTERR_ID = "LOG_OUTERR"; //$NON-NLS-1$

  public static final String LOG_INFO_ID = "LOG_INFO"; //$NON-NLS-1$

  /** GML property constants */
  public static final QName GML_FEATURE_NAME_PROP = new QName( NS.GML2, "name" ); //$NON-NLS-1$

  public static final QName GML_FEATURE_DESCRIPTION_PROP = new QName( NS.GML2, "description" ); //$NON-NLS-1$

  /** hydrotop.xsd */
  public static final QName HYDRO_MEMBER = new QName( NS_NAHYDROTOP, "hydrotopMember" ); //$NON-NLS-1$

  public static final QName HYDRO_ELEMENT_FT = new QName( NS_NAHYDROTOP, "Hydrotop" ); //$NON-NLS-1$

  public static final QName HYDRO_FT = new QName( NS_NAHYDROTOP, "HydrotopFeatureType" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_SEAL_CORR_FACTOR = new QName( NS_NAHYDROTOP, "corrSealing" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_GEOM = new QName( NS_NAHYDROTOP, "position" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_AREA = new QName( NS_NAHYDROTOP, "area" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_LANDUSE_NAME = new QName( NS_NAHYDROTOP, "landuse" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_SOILTYPE = new QName( NS_NAHYDROTOP, "soiltype" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_HYDTYPE = new QName( NS_NAHYDROTOP, "hydType" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_DAINAGETYPE = new QName( NS_NAHYDROTOP, "drainageType" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_MAXPERCOLATIONSRATE = new QName( NS_NAHYDROTOP, "m_perkm" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_INFLOWRATEGW = new QName( NS_NAHYDROTOP, "m_f1gws" ); //$NON-NLS-1$

  public static final QName HYDRO_PROP_SUDS = new QName( NS_NASUDS, "sudLinkMember" ); //$NON-NLS-1$

  public static final QName HYDRO_ENUM_HYDTYPE_SWALETRENCH = new QName( NS_NAHYDROTOP, "MuldenRigole" ); //$NON-NLS-1$

  public static final QName HYDRO_ENUM_HYDTYPE_GREENROOF = new QName( NS_NAHYDROTOP, "Dachbegruenung" ); //$NON-NLS-1$

  public static final QName HYDRO_ENUM_HYDTYPE_REGULAR = new QName( NS_NAHYDROTOP, "Bodenspeicher" ); //$NON-NLS-1$

  /** namodell.xsd */
  // channels
  public static final QName CHANNEL_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "ChannelCollectionMember" ); //$NON-NLS-1$

  public static final QName CHANNEL_MEMBER_PROP = new QName( NS_NAMODELL, "channelMember" ); //$NON-NLS-1$

  public static final QName CHANNEL_ABSTRACT_FT = new QName( NS_NAMODELL, "_Channel" ); //$NON-NLS-1$

  public static final QName CHANNEL_GEOM_PROP = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  public static final QName STORAGE_CHANNEL_ZMLINLINE_PROP = new QName( NS_NAMODELL, "hvvsqd" ); //$NON-NLS-1$

  public static final QName STORAGE_CHANNEL_VMAX_PROP = new QName( NS_NAMODELL, "vmax" ); //$NON-NLS-1$

  public static final QName STORAGE_CHANNEL_VMIN_PROP = new QName( NS_NAMODELL, "vmin" ); //$NON-NLS-1$

  public static final QName STORAGE_CHANNEL_SV_PROP = new QName( NS_NAMODELL, "sv" ); //$NON-NLS-1$

  public static final QName STORAGE_CHANNEL_HVVSQD_PROP = new QName( NS_NAMODELL, "hvvsqd" ); //$NON-NLS-1$

  public static final QName STORAGE_CHANNEL_ELEMENT_FT = new QName( NS_NAMODELL, "StorageChannel" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_ELEMENT_FT = new QName( NS_NAMODELL, "KMChannel" ); //$NON-NLS-1$

  public static final QName V_CHANNEL_ELEMENT_FT = new QName( NS_NAMODELL, "VirtualChannel" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_PARAMETER_FT = new QName( NS_NAMODELL, "KMParameter" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_PARAMETER_MEMBER = new QName( NS_NAMODELL, "KMParameterMember" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_FAKTOR_RKF_PROP = new QName( NS_NAMODELL, "faktorRkf" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_QRK_PROP = new QName( NS_NAMODELL, "qrk" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_RKF_PROP = new QName( NS_NAMODELL, "rkf" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_RKV_PROP = new QName( NS_NAMODELL, "rkv" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_C_PROP = new QName( NS_NAMODELL, "c" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_FAKTOR_RNF_PROP = new QName( NS_NAMODELL, "faktorRnf" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_RNF_PROP = new QName( NS_NAMODELL, "rnf" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_RNV_PROP = new QName( NS_NAMODELL, "rnv" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_KMSTART = new QName( NS_NAMODELL, "startkm" ); //$NON-NLS-1$

  public static final QName KM_CHANNEL_KMEND = new QName( NS_NAMODELL, "endkm" ); //$NON-NLS-1$

  public static final QName IKNOT_MEMBER_PROP = new QName( NS_NAMODELL, "iknotNodeMember" ); //$NON-NLS-1$

  public static final QName DOWNSTREAM_NODE_MEMBER_PROP = new QName( NS_NAMODELL, "downStreamNodeMember" ); //$NON-NLS-1$

  // nodes
  public static final QName NODE_GEOM_PROP = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  public static final QName NODE_ELEMENT_FT = new QName( NS_NAMODELL, "Node" ); //$NON-NLS-1$

  public static final QName NODE_MEMBER_PROP = new QName( NS_NAMODELL, "nodeMember" ); //$NON-NLS-1$

  public static final QName NODE_BRANCHING_MEMBER_PROP = new QName( NS_NAMODELL, "branchingMember" ); //$NON-NLS-1$

  public static final QName NODE_BRANCHING_NODE_MEMBER_PROP = new QName( NS_NAMODELL, "branchingNodeMember" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_MEMBER_PROP = new QName( NS_NAMODELL, "verzweigungNodeMember" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_ENTNAHME = new QName( NS_NAMODELL, "KontEntnahme" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_ZUFLUSS = new QName( NS_NAMODELL, "KontZufluss" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_UEBERLAUF = new QName( NS_NAMODELL, "Ueberlauf" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_VERZWEIGUNG = new QName( NS_NAMODELL, "Verzweigung" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_QZUG_PROP = new QName( NS_NAMODELL, "qzug" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_ZPROZ_PROP = new QName( NS_NAMODELL, "zproz" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_QABG_PROP = new QName( NS_NAMODELL, "qabg" ); //$NON-NLS-1$

  public static final QName NODE_VERZW_QUEB_PROP = new QName( NS_NAMODELL, "queb" ); //$NON-NLS-1$

  public static final QName NODE_COLLECTION_FT = new QName( NS_NAMODELL, "NodeCollection" ); //$NON-NLS-1$

  public static final QName NODE_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "NodeCollectionMember" ); //$NON-NLS-1$

  public static final QName NODE_RESULT_TIMESERIESLINK_PROP = new QName( NS_NAMODELL, "qberechnetZR" ); //$NON-NLS-1$

  public static final QName NODE_ZUFLUSS_ZR_REPOSITORY_PROP = new QName( NS_NAMODELL, "zuflussZRRepository" ); //$NON-NLS-1$

  public static final QName NODE_ZUFLUSS_ZR_PROP = new QName( NS_NAMODELL, "zuflussZR" ); //$NON-NLS-1$

  public static final QName NODE_SYNTHETIC_ZUFLUSS_ZR_PROP = new QName( NS_NAMODELL, "syntheticZuflussZR" ); //$NON-NLS-1$

  public static final QName NODE_PEGEL_ZR_PROP = new QName( NS_NAMODELL, "pegelZR" ); //$NON-NLS-1$

  public static final QName NODE_RIVER_CODE_PROP = new QName( NS_NAMODELL, "riverCode" ); //$NON-NLS-1$

  public static final QName NODE_RIVER_KILOMETER_PROP = new QName( NS_NAMODELL, "riverKilometer" ); //$NON-NLS-1$

  // catchments
  public static final QName CATCHMENT_ELEMENT_FT = new QName( NS_NAMODELL, "Catchment" ); //$NON-NLS-1$

  public static final QName CATCHMENT_GEOM_PROP = new QName( NS_NAMODELL, "Ort" ); //$NON-NLS-1$

  public static final QName CATCHMENT_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "CatchmentCollectionMember" ); //$NON-NLS-1$

  public static final QName CATCHMENT_MEMBER_PROP = new QName( NS_NAMODELL, "catchmentMember" ); //$NON-NLS-1$

  public static final QName BODENKORREKTUR_MEMBER = new QName( NS_NAMODELL, "bodenkorrekturmember" ); //$NON-NLS-1$

  public static final QName GRUNDWASSERABFLUSS_MEMBER = new QName( NS_NAMODELL, "grundwasserabflussMember" ); //$NON-NLS-1$

  public static final QName STATNPARA_MEMBER = new QName( NS_SYNTHN, "statNParameterMember" ); //$NON-NLS-1$

  public static final QName STATN_PROP_XJAH = new QName( NS_SYNTHN, "xjah" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_NGWZU = new QName( NS_NAMODELL, "ngwzu" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_GWWI = new QName( NS_NAMODELL, "gwwi" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_ZFT = new QName( NS_NAMODELL, "zft" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_STATN_DIAG = new QName( NS_SYNTHN, "statNDiag" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_RETBAS = new QName( NS_NAMODELL, "retbas" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_RETINT = new QName( NS_NAMODELL, "retint" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_RETOB = new QName( NS_NAMODELL, "retob" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_RETVS = new QName( NS_NAMODELL, "retvs" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_RETGW = new QName( NS_NAMODELL, "retgw" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_RETKLU = new QName( NS_NAMODELL, "retklu" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_XJAH = new QName( NS_NAMODELL, "xjah" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_FAKTOR_RETBAS = new QName( NS_NAMODELL, "faktorRetbas" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_FAKTOR_RETINT = new QName( NS_NAMODELL, "faktorRetint" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_FAKTOR_RETOB = new QName( NS_NAMODELL, "faktorRetob" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_FAKTOR_RETVS = new QName( NS_NAMODELL, "faktorRetvs" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_FAKTOR_RETGW = new QName( NS_NAMODELL, "faktorRetgw" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_FAKTOR_RETKLU = new QName( NS_NAMODELL, "faktorRetklu" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_IZKN_VERS = new QName( NS_NAMODELL, "izkn_vers" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_IZKN = new QName( NS_NAMODELL, "izkn" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_ZR_NIEDERSCHLAG = new QName( NS_NAMODELL, "niederschlagZR" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_ZR_TEMPERATUR = new QName( NS_NAMODELL, "temperaturZR" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_ZR_VERDUNSTUNG = new QName( NS_NAMODELL, "verdunstungZR" ); //$NON-NLS-1$

  public static final QName CATCHMENT_PROP_ZR_SYNTH = new QName( NS_NAMODELL, "synthZR" ); //$NON-NLS-1$

  // swale and trench

  public static final QName MRS_FT = new QName( NS_NAMODELL, "SwaleAndTrench" ); //$NON-NLS-1$

  public static final QName MRS_COLLECTION_FT = new QName( NS_NAMODELL, "SwaleAndTrenchCollection" ); //$NON-NLS-1$

  public static final QName MRS_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "SwaleAndTrenchCollectionMember" ); //$NON-NLS-1$

  public static final QName MRS_MEMBER_PROP = new QName( NS_NAMODELL, "swaleTrenchMember" ); //$NON-NLS-1$

  public static final QName MRS_GEOM_PROP = new QName( NS_NAMODELL, "position" ); //$NON-NLS-1$

  public static final QName MRS_LENGTH_PROP = new QName( NS_NAMODELL, "length" ); //$NON-NLS-1$

  public static final QName MRS_WIDTH_PROP = new QName( NS_NAMODELL, "widthTrench" ); //$NON-NLS-1$

  public static final QName MRS_SLOPE_PROP = new QName( NS_NAMODELL, "drainPipeSlope" ); //$NON-NLS-1$

  public static final QName MRS_KF_PIPE_PROP = new QName( NS_NAMODELL, "kfPipe" ); //$NON-NLS-1$

  public static final QName MRS_DIAMETER_PIPE_PROP = new QName( NS_NAMODELL, "diameterPipe" ); //$NON-NLS-1$

  public static final QName MRS_ROUGHNESS_PIPE_PROP = new QName( NS_NAMODELL, "roughnessPipe" ); //$NON-NLS-1$

  public static final QName MRS_LANDUSE_TYPE_PROP = new QName( NS_NAMODELL, "nutzung" ); //$NON-NLS-1$

  public static final QName MRS_SOIL_PROFIL_TYPE_PROP = new QName( NS_NAMODELL, "boden" ); //$NON-NLS-1$

  public static final QName MRS_MAX_PERK_PROP = new QName( NS_NAMODELL, "maxPerk" ); //$NON-NLS-1$

  public static final QName MRS_INFLOW_GW_PROP = new QName( NS_NAMODELL, "InflowGW" ); //$NON-NLS-1$

  // link Properties
  public static final QName LINK_CATCHMENT_CHANNEL = new QName( NS_NAMODELL, "entwaesserungsStrangMember" ); //$NON-NLS-1$

  public static final QName LINK_CHANNEL_DOWNSTREAMNODE = new QName( NS_NAMODELL, "downStreamNodeMember" ); //$NON-NLS-1$

  public static final QName LINK_NODE_DOWNSTREAMCHANNEL = new QName( NS_NAMODELL, "downStreamChannelMember" ); //$NON-NLS-1$

  public static final QName LINK_MRS_DISCHARGE_NODE_PROP = new QName( NS_NAMODELL, "dischargeNode" ); //$NON-NLS-1$

  // default values
  public static final QName DEFAULT_MRS_SOIL_PROFIL_PROP = new QName( NS_NAMODELL, "mrsp" ); //$NON-NLS-1$

  // misc
  public static final QName GENERATE_RESULT_PROP = new QName( NS_NAMODELL, "generateResult" ); //$NON-NLS-1$

  public static final QName NA_MODEL_FLAECH_PROP = new QName( NS_NAMODELL, "flaech" ); //$NON-NLS-1$

  public static final QName NA_MODEL_ROOT_FT = new QName( NS_NAMODELL, "NaModell" ); //$NON-NLS-1$

  public static final QName NA_CATCHMENT_COLLECTION_FT = new QName( NS_NAMODELL, "CatchmentCollection" ); //$NON-NLS-1$

  public static final QName NA_CHANNEL_COLLECTION_FT = new QName( NS_NAMODELL, "ChannelCollection" ); //$NON-NLS-1$

  /** parameter.xsd */
  public static final QName PARA_ROOT_FT = new QName( NS_NAPARAMETER, "Parameter" ); //$NON-NLS-1$

  public static final QName PARA_LANDUSE_FT = new QName( NS_NAPARAMETER, "LanduseType" ); //$NON-NLS-1$

  public static final QName PARA_LANDUSE = new QName( NS_NAPARAMETER, "Landuse" ); //$NON-NLS-1$

  public static final QName PARA_SEALING = new QName( NS_NAPARAMETER, "Sealing" ); //$NON-NLS-1$

  public static final QName PARA_Soiltype_FT = new QName( NS_NAPARAMETER, "Soiltype" ); //$NON-NLS-1$

  public static final QName PARA_SoilLayerParameter_FT = new QName( NS_NAPARAMETER, "SoilLayerParameter" ); //$NON-NLS-1$

  public static final QName PARA_SOIL_LAYER_PARAMETER_MEMBER = new QName( NS_NAPARAMETER, "soilLayerParameterMember" ); //$NON-NLS-1$

  public static final QName PARA_LANDUSE_NAME = new QName( NS_NAPARAMETER, "Landuse" ); //$NON-NLS-1$

  public static final QName PARA_SNOW_NAME = new QName( NS_NAPARAMETER, "Snow" ); //$NON-NLS-1$

  public static final QName PARA_SoilLayer_FT = new QName( NS_NAPARAMETER, "SoilLayer" ); //$NON-NLS-1$

  public static final QName PARA_PROP_SEALING_MEMBER = new QName( NS_NAPARAMETER, "sealingMember" ); //$NON-NLS-1$

  public static final QName PARA_PROP_LANDUSE_MEMBER = new QName( NS_NAPARAMETER, "landuseMember" ); //$NON-NLS-1$

  public static final QName PARA_PROP_SNOW_MEMBER = new QName( NS_NAPARAMETER, "snowMember" ); //$NON-NLS-1$

  public static final QName PARA_IDEAL_LANDUSE_MEMBER = new QName( NS_NAPARAMETER, "idealLandUseMember" ); //$NON-NLS-1$

  public static final QName PARA_IDEAL_LANDUSE = new QName( NS_NAPARAMETER, "IdealLandUse" ); //$NON-NLS-1$

  public static final QName PARA_IDEAL_LANDUSE_ZML = new QName( NS_NAPARAMETER, "idealLandUseZML" ); //$NON-NLS-1$

  public static final QName PARA_SOILTYPE_MEMBER = new QName( NS_NAPARAMETER, "soiltypeMember" ); //$NON-NLS-1$

  public static final QName PARA_LANDUSE_PROP_SEALING = new QName( NS_NAPARAMETER, "m_vers" ); //$NON-NLS-1$

  public static final QName PARA_LANDUSE_PROP_SEALING_LINK = new QName( NS_NAPARAMETER, "sealingLink" ); //$NON-NLS-1$

  public static final QName PARA_LANDUSE_PROP_LANDUSE_LINK = new QName( NS_NAPARAMETER, "idealLandUsePeriodLink" ); //$NON-NLS-1$

  public static final QName PARA_SOIL_LAYER_MEMBER = new QName( NS_NAPARAMETER, "soilLayerMember" ); //$NON-NLS-1$

  public static final QName PARA_SOIL_LAYER_LINK = new QName( NS_NAPARAMETER, "soilLayerLink" ); //$NON-NLS-1$

  public static final QName PARA_PROP_XTIEF = new QName( NS_NAPARAMETER, "xtief" ); //$NON-NLS-1$

  public static final QName PARA_PROP_XRET = new QName( NS_NAPARAMETER, "xret" ); //$NON-NLS-1$

  // default values
  public static final QName DEFAULT_MRS_LANDUSE_PROP = new QName( NS_NAPARAMETER, "15" ); // Nutzung Grünland //$NON-NLS-1$

  /** initalValues.xsd */
  public static final QName INI_CATCHMENT_MEMBER_PROP = new QName( NS_INIVALUES, "catchmentMember" ); //$NON-NLS-1$

  public static final QName INI_CHANNEL_MEMBER_PROP = new QName( NS_INIVALUES, "channelMember" ); //$NON-NLS-1$

  public static final QName INI_HYD_MEMBER_PROP = new QName( NS_INIVALUES, "IniHyd" ); //$NON-NLS-1$

  public static final QName INI_HYD_FEATUREID_PROP = new QName( NS_INIVALUES, "featureId" ); //$NON-NLS-1$

  public static final QName INI_CATCHMENT_LINK_HYD_PROP = new QName( NS_INIVALUES, "hyd" ); //$NON-NLS-1$

  /** control.xsd */
  public static final QName CONTROL_STARTSIMULATION = new QName( NS_NAMETA, "startsimulation" ); //$NON-NLS-1$

  public static final QName CONTROL_FORECAST = new QName( NS_NAMETA, "startforecast" ); //$NON-NLS-1$

  public static final QName CONTROL_SCENARIO_ID_PROP = new QName( NS_NAMETA, "scenarioId" ); //$NON-NLS-1$

  public static final QName CONTROL_HOURS_FORECAST_PROP = new QName( NS_NAMETA, "hoursforecast" ); //$NON-NLS-1$

  public static final QName CONTROL_MINUTES_TIMESTEP_PROP = new QName( NS_NAMETA, "minutesTimestep" ); //$NON-NLS-1$

  public static final QName CONTROL_VERSION_KALYPSONA_PROP = new QName( NS_NAMETA, "versionKalypsoNA" ); //$NON-NLS-1$

  public static final QName CONTROL_PNS_PROP = new QName( NS_NAMETA, "pns" ); //$NON-NLS-1$

  public static final QName CONTROL_XJAH_PROP = new QName( NS_NAMETA, "xjah" ); //$NON-NLS-1$

  public static final QName CONTROL_XWAHL2_PROP = new QName( NS_NAMETA, "xwahl2" ); //$NON-NLS-1$

  public static final QName CONTROL_IPVER_PROP = new QName( NS_NAMETA, "ipver" ); //$NON-NLS-1$

  public static final QName CONTROL_RETURN_PERIOD_PROP = new QName( NS_NAMETA, "returnPeriod" ); //$NON-NLS-1$

  /** synthN.xsd */
  public static final QName SYNTHN_STATN_FT = new QName( NS_SYNTHN, "StatN" ); //$NON-NLS-1$

  /** nacontrol.xsd */
  public static final QName NACONTROL_INITIALVALUEDATE_PROP = new QName( NS_NACONTROL, "InitialValueDate" ); //$NON-NLS-1$

  public static final QName NACONTROL_WRITE_PROP = new QName( NS_NACONTROL, "write" ); //$NON-NLS-1$

  public static final QName NACONTROL_INITIALDATE_PROP = new QName( NS_NACONTROL, "initialDate" ); //$NON-NLS-1$

  public static final QName NACONTROL_TMP_PROP = new QName( NS_NACONTROL, "tmp" ); //$NON-NLS-1$

  public static final QName NACONTROL_PRE_PROP = new QName( NS_NACONTROL, "pre" ); //$NON-NLS-1$

  public static final QName NACONTROL_SCH_PROP = new QName( NS_NACONTROL, "sch" ); //$NON-NLS-1$

  public static final QName NACONTROL_BOF_PROP = new QName( NS_NACONTROL, "bof" ); //$NON-NLS-1$

  public static final QName NACONTROL_BSP_PROP = new QName( NS_NACONTROL, "bsp" ); //$NON-NLS-1$

  public static final QName NACONTROL_GWS_PROP = new QName( NS_NACONTROL, "gws" ); //$NON-NLS-1$

  public static final QName NACONTROL_QGS_PROP = new QName( NS_NACONTROL, "qgs" ); //$NON-NLS-1$

  public static final QName NACONTROL_QGG_PROP = new QName( NS_NACONTROL, "qgg" ); //$NON-NLS-1$

  public static final QName NACONTROL_QNA_PROP = new QName( NS_NACONTROL, "qna" ); //$NON-NLS-1$

  public static final QName NACONTROL_QIF_PROP = new QName( NS_NACONTROL, "qif" ); //$NON-NLS-1$

  public static final QName NACONTROL_QVS_PROP = new QName( NS_NACONTROL, "qvs" ); //$NON-NLS-1$

  public static final QName NACONTROL_QBS_PROP = new QName( NS_NACONTROL, "qbs" ); //$NON-NLS-1$

  public static final QName NACONTROL_QT1_PROP = new QName( NS_NACONTROL, "qt1" ); //$NON-NLS-1$

  public static final QName NACONTROL_QTG_PROP = new QName( NS_NACONTROL, "qtg" ); //$NON-NLS-1$

  public static final QName NACONTROL_QGW_PROP = new QName( NS_NACONTROL, "qgw" ); //$NON-NLS-1$

  public static final QName NACONTROL_VET_PROP = new QName( NS_NACONTROL, "vet" ); //$NON-NLS-1$

  public static final QName NACONTROL_QMR_PROP = new QName( NS_NACONTROL, "qmr" ); //$NON-NLS-1$

  public static final QName NACONTROL_HYD_PROP = new QName( NS_NACONTROL, "hyd" ); //$NON-NLS-1$

  public static final QName NACONTROL_BIL_PROP = new QName( NS_NACONTROL, "bil" ); //$NON-NLS-1$

  public static final QName NACONTROL_NMQ_PROP = new QName( NS_NACONTROL, "nmq" ); //$NON-NLS-1$

  public static final QName NACONTROL_SPI_PROP = new QName( NS_NACONTROL, "spi" ); //$NON-NLS-1$

  public static final QName NACONTROL_SUP_PROP = new QName( NS_NACONTROL, "sup" ); //$NON-NLS-1$

  public static final QName NACONTROL_RESULT_TIMESERIESLINK_PROP = new QName( NS_NACONTROL, "qberechnetZR" ); //$NON-NLS-1$

  public static final QName NACONTROL_ROOTNODE_PROP = new QName( NS_NACONTROL, "rootNode" ); //$NON-NLS-1$

  public static final QName NACONTROL_AUTOCALI_PROP = new QName( NS_NACONTROL, "automaticCallibration" ); //$NON-NLS-1$

  public static final QName NACONTROL_ACCPRED_PROP = new QName( NS_NACONTROL, "accuracyPrediction" ); //$NON-NLS-1$

  public static final QName NACONTROL_USEOFFSTARTPRED_PROP = new QName( NS_NACONTROL, "useOffsetStartPrediction" ); //$NON-NLS-1$

  public static final QName NACONTROL_USEOFFENDPRED_PROP = new QName( NS_NACONTROL, "useOffsetEndPrediction" ); //$NON-NLS-1$

  public static final QName NACONTROL_USE_RESULTS_PROP = new QName( NS_NACONTROL, "useResults" ); //$NON-NLS-1$

}