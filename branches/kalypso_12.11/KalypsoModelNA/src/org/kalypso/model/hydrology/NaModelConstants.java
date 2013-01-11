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
package org.kalypso.model.hydrology;

import javax.xml.namespace.QName;

import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author doemming
 */
public interface NaModelConstants
{
  String EXE_PATTERN = "Kalypso-NA_(.*).exe"; //$NON-NLS-1$

  /** namespaces */

  String NS_NAMETA = "org.kalypso.na.control_v2"; //$NON-NLS-1$

  String NS_NAMODELL = "http://www.tuhh.de/kalypsoNA"; //$NON-NLS-1$

  String NS_NACONTROL = "org.kalypso.namodell.control_v2"; //$NON-NLS-1$

  /**
   * @deprecated Do not use, only for conversion of existing projects.
   */
  @Deprecated
  String NS_NAHYDROTOP_11_6 = "http://www.tuhh.de/hydrotop"; //$NON-NLS-1$

  String NS_NAHYDROTOPETYPES = "http://sourceforge.kalypso.org/schemata/hydrology/hydrotopeTypes"; //$NON-NLS-1$

  String NS_NAHYDROTOP = "http://sourceforge.kalypso.org/schemata/hydrology/hydrotope"; //$NON-NLS-1$

  String NS_NAPARAMETER = "http://www.tuhh.de/parameter"; //$NON-NLS-1$

  @Deprecated
  String NS_OMBROMETER = "http://org.kalypso.ombrometer"; //$NON-NLS-1$

  String NS_SYNTHN = "http://www.tuhh.de/synthN"; //$NON-NLS-1$

  String NS_INIVALUES = "http://www.tuhh.de/initialValues"; //$NON-NLS-1$

  String NS_NAFORTRANLOG = "http://www.tuhh.de/NAFortranLog"; //$NON-NLS-1$

  String NS_NALANDUSE = "http://sourceforge.kalypso.org/schemata/hydrology/landuse_v2"; //$NON-NLS-1$

  String NS_NASUDS = "http://sourceforge.kalypso.org/schemata/hydrology/suds"; //$NON-NLS-1$

  /**
   * @deprecated Do not use, only for conversion of existing projects.
   */
  @Deprecated
  String NS_NALANDUSE_11_6 = "http://sourceforge.kalypso.org/schemata/hydrology/landuse"; //$NON-NLS-1$

  String NS_NAPEDOLOGIE = "http://sourceforge.kalypso.org/schemata/hydrology/pedologie"; //$NON-NLS-1$

  /**
   * @deprecated Do not use, only for conversion of existing projects.
   */
  @Deprecated
  String NS_NAGEOLOGIE_11_6 = "http://sourceforge.kalypso.org/schemata/hydrology/geologie"; //$NON-NLS-1$

  String NS_NAGEOLOGY = "http://sourceforge.kalypso.org/schemata/hydrology/geology"; //$NON-NLS-1$

  String NS_NAOVERLAY = "http://sourceforge.kalypso.org/schemata/hydrology/hydrotopeOverlay"; //$NON-NLS-1$

  String NS_NASIMPLESHAPECATCHMENT = "http://sourceforge.kalypso.org/schemata/hydrology/simpleShapeCatchment"; //$NON-NLS-1$

  String NS_NAOPTIMIZE = "http://kalypso.sorgeforge.net/schemata/hydrology/optimize"; //$NON-NLS-1$

  String NS_TIMESERIES_MANAGEMENT = "http://kalypso.sorgeforge.net/schemata/hydrology/timeseriesManagement"; //$NON-NLS-1$

  String NS_CATCHMENT_MODEL = "http://kalypso.sorgeforge.net/schemata/hydrology/catchmentModel"; //$NON-NLS-1$

  String NS_TIMESERIES_MAPPING = "http://kalypso.sorgeforge.net/schemata/hydrology/timeseriesMapping"; //$NON-NLS-1$

  /** server client bean constants */
  // input
  String IN_META_ID = "MetaSteuerdaten"; //$NON-NLS-1$

  String IN_MODELL_ID = "Modell"; //$NON-NLS-1$

  String IN_HYDROTOP_ID = "Hydrotop"; //$NON-NLS-1$

  String IN_PARAMETER_ID = "Parameter"; //$NON-NLS-1$

  String IN_LANDUSE_ID = "Landuse"; //$NON-NLS-1$

  String IN_SUDS_ID = "Suds"; //$NON-NLS-1$

  String IN_CONTROL_ID = "Control"; //$NON-NLS-1$

  String IN_OPTIMIZE_ID = "Optimize"; //$NON-NLS-1$

  String IN_OPTIMIZE_FEATURE_PATH_ID = "OptimizeFeaturePath"; //$NON-NLS-1$

  String IN_SYNTHN_ID = "synthNiederschlag"; //$NON-NLS-1$

  String IN_OPTIMIZECONF_ID = "SceConf"; //$NON-NLS-1$

  String IN_KLIMA_DIR_ID = "KlimaDir"; //$NON-NLS-1$

  String IN_RAINFALL_ID = "NiederschlagDir"; //$NON-NLS-1$

  String IN_PEGEL_DIR = "PegelDir"; //$NON-NLS-1$

  String IN_RESULTS_DIR_ID = "ErgebnisDir"; //$NON-NLS-1$

  String IN_LZSIM_IN_ID = "LZSIM_IN"; //$NON-NLS-1$

  String IN_PREPROCESSED_ASCII = "PreprocessedAscii"; //$NON-NLS-1$

  // output
  String OUT_ZML = "OUT_ZML"; //$NON-NLS-1$

  String OUT_OPTIMIZEFILE = "OUT_OPTIMIZEFILE"; //$NON-NLS-1$

  String OUTPUT_DIR_NAME = "results"; //$NON-NLS-1$

  /** namodell.xsd */

  QName BODENKORREKTUR_MEMBER = new QName( NS_NAMODELL, "bodenkorrekturmember" ); //$NON-NLS-1$

  QName STATNPARA_MEMBER = new QName( NS_SYNTHN, "statNParameterMember" ); //$NON-NLS-1$

  QName STATN_PROP_XJAH = new QName( NS_SYNTHN, "xjah" ); //$NON-NLS-1$

  QName STATN_PROP_STATN_DIAG = new QName( NS_SYNTHN, "statNDiag" ); //$NON-NLS-1$

  // swale and trench

  QName MRS_FT = new QName( NS_NAMODELL, "SwaleAndTrench" ); //$NON-NLS-1$

  QName MRS_COLLECTION_FT = new QName( NS_NAMODELL, "SwaleAndTrenchCollection" ); //$NON-NLS-1$

  QName MRS_COLLECTION_MEMBER_PROP = new QName( NS_NAMODELL, "SwaleAndTrenchCollectionMember" ); //$NON-NLS-1$

  QName MRS_MEMBER_PROP = new QName( NS_NAMODELL, "swaleTrenchMember" ); //$NON-NLS-1$

  QName MRS_GEOM_PROP = new QName( NS_NAMODELL, "position" ); //$NON-NLS-1$

  QName MRS_LENGTH_PROP = new QName( NS_NAMODELL, "length" ); //$NON-NLS-1$

  QName MRS_WIDTH_PROP = new QName( NS_NAMODELL, "widthTrench" ); //$NON-NLS-1$

  QName MRS_SLOPE_PROP = new QName( NS_NAMODELL, "drainPipeSlope" ); //$NON-NLS-1$

  QName MRS_KF_PIPE_PROP = new QName( NS_NAMODELL, "kfPipe" ); //$NON-NLS-1$

  QName MRS_DIAMETER_PIPE_PROP = new QName( NS_NAMODELL, "diameterPipe" ); //$NON-NLS-1$

  QName MRS_ROUGHNESS_PIPE_PROP = new QName( NS_NAMODELL, "roughnessPipe" ); //$NON-NLS-1$

  QName MRS_LANDUSE_TYPE_PROP = new QName( NS_NAMODELL, "nutzung" ); //$NON-NLS-1$

  QName MRS_SOIL_PROFIL_TYPE_PROP = new QName( NS_NAMODELL, "boden" ); //$NON-NLS-1$

  QName MRS_MAX_PERK_PROP = new QName( NS_NAMODELL, "maxPerk" ); //$NON-NLS-1$

  QName MRS_INFLOW_GW_PROP = new QName( NS_NAMODELL, "InflowGW" ); //$NON-NLS-1$

  // link Properties

  QName LINK_MRS_DISCHARGE_NODE_PROP = new QName( NS_NAMODELL, "dischargeNode" ); //$NON-NLS-1$

  // default values
  QName DEFAULT_MRS_SOIL_PROFIL_PROP = new QName( NS_NAMODELL, "mrsp" ); //$NON-NLS-1$

  QName NA_MODEL_FLAECH_PROP = new QName( NS_NAMODELL, "flaech" ); //$NON-NLS-1$

  QName NA_MODEL_ROOT_FT = new QName( NS_NAMODELL, "NaModell" ); //$NON-NLS-1$

  /** parameter.xsd */
  QName PARA_LANDUSE_FT = new QName( NS_NAPARAMETER, "LanduseType" ); //$NON-NLS-1$

  QName PARA_LANDUSE = new QName( NS_NAPARAMETER, "Landuse" ); //$NON-NLS-1$

  QName PARA_SEALING = new QName( NS_NAPARAMETER, "Sealing" ); //$NON-NLS-1$

  QName PARA_SOIL_LAYER_PARAMETER_FT = new QName( NS_NAPARAMETER, "SoilLayerParameter" ); //$NON-NLS-1$

  QName PARA_LANDUSE_NAME = new QName( NS_NAPARAMETER, "Landuse" ); //$NON-NLS-1$

  QName PARA_PROP_SEALING_MEMBER = new QName( NS_NAPARAMETER, "sealingMember" ); //$NON-NLS-1$

  QName PARA_PROP_LANDUSE_MEMBER = new QName( NS_NAPARAMETER, "landuseMember" ); //$NON-NLS-1$

  QName PARA_IDEAL_LANDUSE_MEMBER = new QName( NS_NAPARAMETER, "idealLandUseMember" ); //$NON-NLS-1$

  QName PARA_IDEAL_LANDUSE = new QName( NS_NAPARAMETER, "IdealLandUse" ); //$NON-NLS-1$

  QName PARA_IDEAL_LANDUSE_ZML = new QName( NS_NAPARAMETER, "idealLandUseZML" ); //$NON-NLS-1$

  QName PARA_LANDUSE_PROP_SEALING = new QName( NS_NAPARAMETER, "m_vers" ); //$NON-NLS-1$

  QName PARA_LANDUSE_PROP_SEALING_LINK = new QName( NS_NAPARAMETER, "sealingLink" ); //$NON-NLS-1$

  QName PARA_LANDUSE_PROP_LANDUSE_LINK = new QName( NS_NAPARAMETER, "idealLandUsePeriodLink" ); //$NON-NLS-1$

  // default values
  QName DEFAULT_MRS_LANDUSE_PROP = new QName( NS_NAPARAMETER, "15" ); // Nutzung Grünland //$NON-NLS-1$

  /** initalValues.xsd */
  QName INI_HYD_MEMBER_PROP = new QName( NS_INIVALUES, "IniHyd" ); //$NON-NLS-1$

  QName INI_CATCHMENT_LINK_HYD_PROP = new QName( NS_INIVALUES, "hyd" ); //$NON-NLS-1$

  /** synthN.xsd */
  QName SYNTHN_STATN_FT = new QName( NS_SYNTHN, "StatN" ); //$NON-NLS-1$

  String[] TA_AXES = new String[] { ITimeseriesConstants.TYPE_HOURS, ITimeseriesConstants.TYPE_NORM };

  String[] WTKCLAI_AXES = new String[] { ITimeseriesConstants.TYPE_DATE, ITimeseriesConstants.TYPE_LAI, ITimeseriesConstants.TYPE_WT, ITimeseriesConstants.TYPE_KC };

  String[] TN_AXES = new String[] { ITimeseriesConstants.TYPE_MIN, ITimeseriesConstants.TYPE_RAINFALL };
}