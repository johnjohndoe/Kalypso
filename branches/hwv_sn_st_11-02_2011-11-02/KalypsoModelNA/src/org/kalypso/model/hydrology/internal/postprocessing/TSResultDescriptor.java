/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.hydrology.internal.postprocessing;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.model.hydrology.binding.model.StorageChannel;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author Gernot Belger
 */
public enum TSResultDescriptor
{
  qgs(".qgs", Node.FEATURE_NODE, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  pre("j Niederschlag .pre", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RAINFALL, 1.0d),
  tmp("j Temperatur .tmp", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_TEMPERATURE, 1.0d),
  qif("n Interflow .qif", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  qgw("n Grundwasser .qgw", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  qgg("n Gesamtabfluss TG .qgg", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  gws("n Grundwasserstand .gws - Umrechnung von m auf cm", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_WATERLEVEL, 100.0d),
  qbs("n Basisabfluss .qbs", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  qna("n Oberflaechenabfluss .qna", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  qvs("n Abfluss vers. Flaechen .qvs", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  sch("// n Schnee .sch [mm]", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_WATERLEVEL, 0.1d),
  qt1("n Kluftgrundw1 .qt1", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  qtg("n Kluftgrundw .qtg", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  vet("n Evapotranspiration .vet [mm]", Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_EVAPORATION, 0.1d),

  // Straenge
  sph("n Wasserstand Speicher .sph [muNN]", StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_NORMNULL, 1.0d),
  sub("n Speicherueberlauf .sub [m³/s]", StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_RUNOFF, 1.0d),
  spi("n Speicherinhalt .spi [hm³] - Umrechnung auf m³", StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_VOLUME, 1000000.0d);

  public static final String SUFFIX_QGS = "qgs";

  private final String m_description;

  private final QName m_featureType;

  private final String m_axisType;

  private final double m_resultFaktor;

  TSResultDescriptor( final String description, final QName featureType, final String axisType, final double resultFaktor )
  {
    m_description = description;
    m_featureType = featureType;
    m_axisType = axisType;
    m_resultFaktor = resultFaktor;
  }

  public String getDescription( )
  {
    return m_description;
  }

  public QName getFeatureType( )
  {
    return m_featureType;
  }

  public double getResultFactor( )
  {
    return m_resultFaktor;
  }

  public String getAxisType( )
  {
    return m_axisType;
  }

  // FIXME: change to switch or member of descriptor
  public static String getAxisTitleForSuffix( final String suffix )
  {
    // j Temperatur .tmp
    if( suffix.equalsIgnoreCase( "tmp" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.5" ); //$NON-NLS-1$
    // j Niederschlag .pre
    if( suffix.equalsIgnoreCase( "pre" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.6" ); //$NON-NLS-1$
    // n Schnee .sch
    if( suffix.equalsIgnoreCase( "sch" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.7" ); //$NON-NLS-1$
    // j Bodenfeuchte .bof
    if( suffix.equalsIgnoreCase( "bof" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.8" ); //$NON-NLS-1$
    // n Bodenspeicher .bsp
    if( suffix.equalsIgnoreCase( "bsp" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.9" ); //$NON-NLS-1$
    // n Grundwasserstand .gws
    if( suffix.equalsIgnoreCase( "gws" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.10" ); //$NON-NLS-1$
    // Gesamtabfluss Knoten .qgs, Gesamtabfluss TG .qgg, Oberflaechenabfluss .qna, Interflow .qif, Abfluss vers.
    // Flaechen .qvs, Basisabfluss .qbs, Kluftgrundw1 .qt1, Kluftgrundw .qtg, Grundwasser .qgw
    if( suffix.equalsIgnoreCase( "qgs" ) | suffix.equalsIgnoreCase( "qgg" ) | suffix.equalsIgnoreCase( "qna" ) | suffix.equalsIgnoreCase( "qif" ) | suffix.equalsIgnoreCase( "qvs" ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
        | suffix.equalsIgnoreCase( "qbs" ) | suffix.equalsIgnoreCase( "qt1" ) | suffix.equalsIgnoreCase( "qtg" ) | suffix.equalsIgnoreCase( "qgw" ) ) //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ); //$NON-NLS-1$
    // n Evapotranspiration .vet
    if( suffix.equalsIgnoreCase( "vet" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.12" ); //$NON-NLS-1$
    // n Ausgabe hydrotope .hyd
    if( suffix.equalsIgnoreCase( "hyd" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.16" ); //$NON-NLS-1$
    // n Abflussbilanz .bil
    if( suffix.equalsIgnoreCase( "bil" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.17" ); //$NON-NLS-1$
    // n Statistische Abflusswerte .nmq
    if( suffix.equalsIgnoreCase( "nmq" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.18" ); //$NON-NLS-1$
    // n Speicherinhalt .spi
    if( suffix.equalsIgnoreCase( "spi" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.19" ); //$NON-NLS-1$
    // n Wasserspiegelhöhe .sph
    if( suffix.equalsIgnoreCase( "sph" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.20" ); //$NON-NLS-1$
    // n Verdunstung aus Talsperre .spv
    if( suffix.equalsIgnoreCase( "spv" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.21" ); //$NON-NLS-1$
    // n Niederschlag in Talsperre .spn
    if( suffix.equalsIgnoreCase( "spn" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.22" ); //$NON-NLS-1$
    // n Zehrung .spb
    if( suffix.equalsIgnoreCase( "spb" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.24" ); //$NON-NLS-1$
    // n Speicherueberlauf .sub
    if( suffix.equalsIgnoreCase( "sub" ) ) //$NON-NLS-1$
      return Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.25" ); //$NON-NLS-1$
    return suffix;
  }

}
