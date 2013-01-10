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
package org.kalypso.model.hydrology.timeseries;

import javax.xml.namespace.QName;

import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;

/**
 * @author Gernot Belger
 */
public enum TSResultDescriptor
{
  qgs( Node.FEATURE_NODE, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Gesamtabfluss", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  pre( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RAINFALL, 1.0d, "Niederschlag", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.6" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  tmp( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_TEMPERATURE, 1.0d, "Temperatur", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.5" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qif( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Interflow", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qgw( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Grundwasserabfluss", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qgg( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Gesamtabfluss", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  gws( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_WATERLEVEL, 100.0d, "Grundwasserstand", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.10" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qbs( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Basisabfluss", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qna( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Oberflaechenabfluss(natuerlich)", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qvs( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Oberflaechenabfluss(versiegelt)", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  sch( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_WATERLEVEL, 0.1d, "Schneehoehe", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.7" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  // REMARK: beide nicht in Oberfläche, lt. Niloufar fraglich ob Ausgabe sinnvoll
  qt1( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Kluftgrundw1abfluss", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  qtg( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "KluftGWAbfluss", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.11" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  // TODO: evapotranspiration != evaporation -> use other datatype
  vet( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_EVAPORATION, 0.1d, "Evapotranspiration", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.12" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  // TODO: nicht in Oberfläche: Pasche wollte es nicht?!
  // bof(Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_SOIL_MOISTURE, 0.1d, "Bodenfeuchte" , Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.8" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  // REMARK: bodenspeicherbilanz ist keine Zeitreihe
  // bsp(Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_XXX, 0.1d, "Bodenspeicherbilanz" , Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.9" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  // hyd(Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_XXX, 0.1d, "Hydrotope" , Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.16" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  spv( Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_EVAPORATION_WATER_BASED, 0.1d, "Talsperrenverdunstung", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.21" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  // REMARK: werden schon rausgeschrieben, macht aber lt. Niloufar keinen Sinn
  // spn(Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_XXX, 0.1d, "Niederschlag" , Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.22" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  // spb(Catchment.FEATURE_CATCHMENT, ITimeseriesConstants.TYPE_XXX, 0.1d, "Zehrung" , Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.24" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  // Speicher
  // REMARK: nicht in Oberfläche, wird lt. Niloufar nicht gebraucht
  sph( StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_NORMNULL, 1.0d, "Wasserspiegelhoehe", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.20" ) ), //$NON-NLS-1$ //$NON-NLS-2$

  sub( StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_RUNOFF, 1.0d, "Speicherueberlauf", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.25" ) ), //$NON-NLS-1$ //$NON-NLS-2$
  spi( StorageChannel.FEATURE_STORAGE_CHANNEL, ITimeseriesConstants.TYPE_VOLUME, 1000000.0d, "Fuellvolumen", Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.19" ) ); //$NON-NLS-1$ //$NON-NLS-2$

  /*
   * In altem switch über Ausgabedateinamen, sind aber vermutlich keine Zeitreihen: // n Abflussbilanz .bil case "bil":
   * //$NON-NLS-1$ return "Abflussbilanz"; //$NON-NLS-1$ // n Statistische Abflusswerte .nmq case "nmq": //$NON-NLS-1$
   * return "Abflusswerte(statistisch)"; //$NON-NLS-1$ if( suffix.equalsIgnoreCase( "nmq" ) ) //$NON-NLS-1$ return
   * Messages.getString( "org.kalypso.convert.namodel.NaModelInnerCalcJob.18" ); //$NON-NLS-1$
   */

  private final QName m_featureType;

  private final String m_axisType;

  private final double m_resultFaktor;

  private final String m_resultFilename;

  private final String m_axisTitle;

  TSResultDescriptor( final QName featureType, final String axisType, final double resultFaktor, final String resultFilename, final String axisTitle )
  {
    m_featureType = featureType;
    m_axisType = axisType;
    m_resultFaktor = resultFaktor;
    m_resultFilename = resultFilename;
    m_axisTitle = axisTitle;
  }

  public String getAxisTitle( )
  {
    return m_axisTitle;
  }

  public String getResultFilename( )
  {
    return m_resultFilename;
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
}