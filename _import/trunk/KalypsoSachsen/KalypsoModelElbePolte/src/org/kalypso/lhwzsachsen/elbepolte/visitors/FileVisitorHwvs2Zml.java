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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.lhwzsachsen.elbepolte.visitors;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Properties;

import org.kalypso.commons.lhwz.LhwzHelper;
import org.kalypso.contribs.java.io.FileVisitor;
import org.kalypso.lhwzsachsen.elbepolte.ElbePolteConst;
import org.kalypso.lhwzsachsen.elbepolte.ElbePolteConverter;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.status.KalypsoStati;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.timeseries.envelope.TranProLinFilterUtilities;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;

/**
 * 
 * converts native (HWVS) files to ZML files
 * 
 * @author thuel2
 */
public class FileVisitorHwvs2Zml implements FileVisitor
{

  private final File m_outputDir;

  private final Properties m_props;
  private final GMLWorkspace m_wkspce;
  private final FeatureList m_fList;
  private final boolean m_writeUmhuellende;
  private Collection m_exceptions = new ArrayList();

  private final String m_propZmlLink;
  private final Map m_metaMap;

  /**
   * @param outputDir
   * @param props
   * @param propFeatList
   * @param propZmlLink
   * @param writeUmhuellende
   * @param metaMap
   *  
   */

  public FileVisitorHwvs2Zml( File outputDir, Properties props, String propFeatList, String propZmlLink,
      boolean writeUmhuellende, Map metaMap )
  {
    m_outputDir = outputDir;
    m_props = props;

    m_wkspce = (GMLWorkspace)m_props.get( ElbePolteConst.DATA_GML );
    m_fList = (FeatureList)m_wkspce.getFeatureFromPath( propFeatList );
    m_writeUmhuellende = writeUmhuellende;
    m_propZmlLink = propZmlLink;
    m_metaMap = metaMap;

  }

  /**
   * @see org.kalypso.contribs.java.io.FileVisitor#visit(java.io.File)
   */
  public boolean visit( File fleHwvs )
  {
    // kontrollieren, ob file (per ID) in featureList auftaucht
    if( fleHwvs.isFile() )
    {
      final String fleExt = getExtension( fleHwvs );

      for( int ii = 0; ii < m_fList.size(); ii++ )
      {
        final Feature f = (Feature)m_fList.get( ii );
        final String id = (String)f.getProperty( "nr" );
        if( fleExt.equals( id ) )
        {

          final Object objTSLink = f.getProperty( m_propZmlLink );
          if( objTSLink instanceof TimeseriesLinkType )
          {
            final TimeseriesLinkType tlt = (TimeseriesLinkType)objTSLink;
            final String sFlePath = tlt.getHref();

            final File fleZml = new File( m_outputDir, sFlePath );
            fleZml.getParentFile().mkdirs();

            final String sZmlFileBaseName = fleZml.getName().replaceAll( ".zml", "" );

            ElbePolteConverter.hwvs2zml( fleHwvs, fleZml, m_metaMap );
            final Object objAccuracy = f.getProperty( "accuracyPrediction" );
            double accuracy = LhwzHelper.getDefaultUmhuellendeAccuracy();
            if( objAccuracy instanceof Double )
              accuracy = ( (Double)objAccuracy ).doubleValue();

            // und Umh¸llende "_unten", "_oben"
            //final InputSource is = new InputSource( fleZml.getAbsolutePath() );
            try
            {
              final URL is = fleZml.toURL();
              final IObservation obsZml = ZmlFactory.parseXML( is, "" );

              if( m_writeUmhuellende )
              {

                // get first and last date of observation
                final IAxis dateAxis = ObservationUtilities.findAxisByType( obsZml.getAxisList(),
                    TimeserieConstants.TYPE_DATE );
                final IAxis valueAxis = ObservationUtilities.findAxisByType( obsZml.getAxisList(),
                    TimeserieConstants.TYPE_RUNOFF );
                final ITuppleModel values = obsZml.getValues( null );
                final int valueCount = values.getCount();
                if( valueCount > 1 )
                {

                  final org.kalypso.ogc.sensor.DateRange forecastRange = TimeserieUtils.isForecast( obsZml );
                  if( forecastRange != null )
                  {
                    final Date startPrediction = forecastRange.getFrom();

                    // final Date endPrediction = forecastRange.getTo();
                    // sicher ist sicher...
                    final Date endPrediction = (Date)values.getElement( valueCount - 1, dateAxis );
                    final Double endValue = (Double)values.getElement( valueCount - 1, valueAxis );

                    final Calendar calBegin = Calendar.getInstance();
                    calBegin.setTime( startPrediction );

                    final Calendar calEnd = Calendar.getInstance();
                    calEnd.setTime( endPrediction );

                    final long millisOf60hours = 1000 * 60 * 60 * 60;

                    final double endAccuracy = accuracy
                        * ( ( (double)( endPrediction.getTime() - startPrediction.getTime() ) ) / ( (double)millisOf60hours ) );

                    final double endOffset = Math.abs( endValue.doubleValue() * endAccuracy / 100 );

                    TranProLinFilterUtilities.transformAndWrite( obsZml, calBegin, calEnd, 0, endOffset, "-",
                        TimeserieConstants.TYPE_RUNOFF, KalypsoStati.BIT_DERIVATED, new File( fleZml.getParentFile(),
                            sZmlFileBaseName + "_unten.zml" ), "- Spur Unten" );
                    TranProLinFilterUtilities.transformAndWrite( obsZml, calBegin, calEnd, 0, endOffset, "+",
                        TimeserieConstants.TYPE_RUNOFF, KalypsoStati.BIT_DERIVATED, new File( fleZml.getParentFile(),
                            sZmlFileBaseName + "_oben.zml" ), "- Spur Oben" );

                  }
                }
              }
            }
            catch( Exception e )
            {
              e.printStackTrace();
              m_exceptions.add( e );
            }
          }
        }
      }
    }
    return true;
  }

  /**
   * Gibt Datei-Extension zur¸ck (z.B. png)
   *  
   */
  public static String getExtension( final File file )
  {
    String ext;
    String s;
    int pos;

    ext = null;
    s = file.getName();
    pos = s.lastIndexOf( '.' );

    if( pos > 0 && pos < s.length() - 1 )
    {
      ext = s.substring( pos + 1 );
    }
    return ext;
  }

  public boolean hasException()
  {
    return !m_exceptions.isEmpty();
  }

  public Exception[] getExceptions()
  {
    return (Exception[])m_exceptions.toArray( new Exception[m_exceptions.size()] );
  }

}
