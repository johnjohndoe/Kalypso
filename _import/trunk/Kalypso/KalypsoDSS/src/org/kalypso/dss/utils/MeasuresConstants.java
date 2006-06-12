/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.dss.utils;

/**
 * Constance to work with measure schemas
 * 
 * @author kuepfer
 */
public class MeasuresConstants
{
  /** Namespaces */
  public static final String NS_MEASURES_SEALING = "http://schema.kalypso.wb.tu-harburg.de/measure/sealing";

  public static final String NS_MEASURES_RHB = "http://schema.kalypso.wb.tu-harburg.de/measure/rhb";

  public static final String NS_MEASURES_MRS = "http://schema.kalypso.wb.tu-harburg.de/measure/mrs";

  public static final String NS_DESIGNAREA = "http://schema.kalypso.wb.tu-harburg.de/plangebiet.xsd";

  public static final String NS_XPLANUNG = "http://www.xplanung.de/bplangml";

  /** Bean id's */
  public static final String IN_MEASURE_RHB_ID = "measureRHB";

  public static final String IN_MEASURE_MRS_ID = "measureMRS";

  public static final String IN_MEASURE_SEALING_ID = "measureSealing";

  public static final String IN_DESIGN_AREA_ID = "designArea";

  public static final String IN_MEASURE_PLANNING_ID = "xplanung";

  /** properties and FeatureTypes for all measures */
  // retension basin measure
  public static final String RHB_MEASURE_FT = "RhbMeasureFeature";

  public static final String RHB_MEASURE_GEOMETRY_PROP = "position";

  public static final String RHB_MEASURE_PROP_QMAX = "qmax";

  public static final String RHB_MEASURE_PROP_DEPTH = "depth";

  public static final String RHB_MEASURE_PROP_SLOPE = "slopeRhb";

  public static final String RHB_MEASURE_INFLOWTYP_PROP = "inflowtype";

  public static final String RHB_MEASURE_INFLOWTYP_RIVER = "Hauptschluss-Gewaesser";

  public static final String RHB_MEASURE_INFLOWTYP_CATCHMENT = "Hauptschluss-Teilgebiet";

  public static final String RHB_MEASURE_PROP_DIAMETER = "diameterDischarge";

  // swale and trench
  public static final String MRS_MEASURE_FT = "MrsMeasureFeature";

  public static final String MRS_MEASURE_PROP_DIAMETER = "diameter";

  public static final String MRS_MEASURE_PROP_WIDTH = "width";

  public static final String MRS_MEASURE_PROP_PERCENTAGE = "percentage";

  public static final String MRS_MEASURE_GEOMETRY_PROP = "position";

  // sealing measure
  public static final String SEALING_MEASURE_FT = "SealingMeasureFeature";

  public static final String SEALING_MEASURE_GEOMETRY_PROP = "position";

  public static final String SEALING_MEASURE_SEALINGFACTOR_PROP = "sealingfactor";

  public static final String DEFAULT_ENCONDING = "UTF-8";

  /** default values */
  public static final double MRS_DEFAULT_SLOPE_PROP = 0.003d;

  public static final double MRS_DEFAULT_INFLOW_GW_PROP = 1.0d;

  public static final double MRS_DEFAULT_MAX_PERK_PROP = 2e-8d;

  public static final double MRS_DEFAULT_ROUGHNESS_PIPE_PROP = 10.0d;

  /** desing area */
  public static final String DESIGNAREA_MEMBER_PROP = "featureMember";

  public static final String DESINGAREA_GEOM_PROP = "gebiet";

  /** XPlanung (BPlan) */
  
  public static final String XPLANUNG_GEOM_PROP = "position";

  public static final String XPLANUNG_GRZ_PROP = "GRZ";
  
  public static final String XPLANUNG_ART_BAULICHNUTZ_PROP = "artDerBaulichenNutzung";

  public static final String XPLANUNG_GEMEINBED_FT = "GemeinbedarfsFlaeche";
  
  public static final String XPLANUNG_VERKEHRSFMITBESZWECK_FT = "VerkehrsflaecheBesondererZweckbestimmung";

  public static final String XPLANUNG_VERKEHRSFL_FT = "VerkehrsFlaeche";
  
  public static final String XPLANUNG_GRUENFL_FT = "GruenFlaeche";

  public static final String XPLANUNG_BAUGEBIET_FT = "BaugebietsFlaechenTeil";
  
  
}
