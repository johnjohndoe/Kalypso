/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
/*
 * 
 * @author huebsch
 */
package org.kalypso.convert.namodel.manager;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.schema.binding.suds.IGreenRoof;
import org.kalypso.convert.namodel.schema.binding.suds.ISwale;
import org.kalypso.convert.namodel.schema.binding.suds.ISwaleInfiltrationDitch;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.model.hydrology.NaModelConstants;
import org.kalypso.model.hydrology.internal.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITupleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.metadata.ITimeserieConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;

public class NutzungManager extends AbstractManager
{
  private final NAConfiguration m_conf;

  private final IFeatureType m_LanduseFT;

  private final IFeatureType m_IdleLanduseFT;

  final Hashtable<String, Integer> m_LTable = new Hashtable<String, Integer>();

  private int m_idCounter = 0;

  public NutzungManager( final GMLSchema parameterSchema, final NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    m_conf = conf;
    m_LanduseFT = parameterSchema.getFeatureType( NaModelConstants.PARA_LANDUSE );
    m_IdleLanduseFT = parameterSchema.getFeatureType( NaModelConstants.PARA_IDEAL_LANDUSE );
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  @Override
  public String mapID( final int id, final IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  @Override
  public Feature[] parseFile( final URL url ) throws Exception
  {
    final String nutzDatei = url.getPath().replaceAll( ".+/", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final String nutzID = nutzDatei.replaceAll( "\\.nuz", "" ); //$NON-NLS-1$ //$NON-NLS-2$
    final List<Feature> result = new ArrayList<Feature>();
    final LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    while( (fe = readNextFeature( reader, nutzID )) != null )
      result.add( fe );
    return result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( final LineNumberReader reader, final String nutzID ) throws Exception
  {
    final HashMap<String, String> landusePropCollector = new HashMap<String, String>();
    final Map<IPropertyType, Object> fePropMap = new LinkedHashMap<IPropertyType, Object>();
    String line;
    // 9
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line ); //$NON-NLS-1$

    if( !m_LTable.containsKey( line ) )
    {
      m_idCounter = m_idCounter + 1;
      final Integer idleLanduseID = new Integer( m_idCounter );
      m_LTable.put( line, idleLanduseID );
    }
    final Object idleLanduseStringID = m_LTable.get( line );

    // Kommentarzeilen
    line = reader.readLine();
    line = reader.readLine();

    // FeatureProperty landuseNameProp = FeatureFactory.createFeatureProperty( "name", nutzID );
    landusePropCollector.put( "name", nutzID ); //$NON-NLS-1$

    // generate id:
    // FeatureProperty prop = (FeatureProperty)landusePropCollector.get( "name" );
    // String asciiStringId = nutzID;
    final Feature feature = getFeature( nutzID, m_LanduseFT );
    for( int i = 0; i < 12; i++ )
    {
      line = reader.readLine();
      System.out.println( Messages.getString( "org.kalypso.convert.namodel.manager.NutzungManager.6", i, line ) ); //$NON-NLS-1$ //$NON-NLS-2$
    }

    final Feature idleLanduseFE = getFeature( idleLanduseStringID.toString(), m_IdleLanduseFT );
    final IPropertyType pt = feature.getFeatureType().getProperty( NaModelConstants.PARA_LANDUSE_PROP_LANDUSE_LINK );
    fePropMap.put( pt, idleLanduseFE.getId() );
    line = reader.readLine();

    // continue reading
    // Collection collection = landusePropCollector.values();
    setParsedProperties( feature, landusePropCollector, fePropMap );
    return feature;
  }

  public void writeFile( final GMLWorkspace paraWorkspace ) throws Exception
  {
    final Feature rootFeature = paraWorkspace.getRootFeature();
    final List<Feature> list = (List<Feature>) rootFeature.getProperty( NaModelConstants.PARA_PROP_LANDUSE_MEMBER );
    final Iterator<Feature> iter = list.iterator();

    while( iter.hasNext() )
    {
      final Feature nutzungFE = iter.next();
      final IRelationType rt = (IRelationType) nutzungFE.getFeatureType().getProperty( NaModelConstants.PARA_LANDUSE_PROP_LANDUSE_LINK );
      final Feature linkedIdealLanduseFE = paraWorkspace.resolveLink( nutzungFE, rt );
      writeFeature( nutzungFE, linkedIdealLanduseFE );
    }
    writeConstantSudsIdealLanduse();
  }

  private void writeFeature( final Feature feature, final Feature linkedIdealLanduseFE ) throws Exception
  {
    final String nutzName = m_conf.getLanduseFeatureShortedName( feature.getName() );
    final File outputFile = new File( m_conf.getNutzungDir(), nutzName + ".nuz" ); //$NON-NLS-1$
    final FileWriter writer = new FileWriter( outputFile );
    writer.write( linkedIdealLanduseFE.getName() );
    writer.write( "\nidealisierter jahresgang" );// "ideali" ist Kennung! //$NON-NLS-1$
    writer.write( "\nxxdatum     F EVA    We    BIMAX\n" ); //$NON-NLS-1$
    final Object idealLanduseProp = linkedIdealLanduseFE.getProperty( NaModelConstants.PARA_IDEAL_LANDUSE_ZML );
    writeIdealLanduse( (IObservation) idealLanduseProp, writer );
    writer.write( "993456789012345678901234567890" ); //$NON-NLS-1$
    IOUtils.closeQuietly( writer );
  }

  private void writeConstantSudsIdealLanduse( ) throws Exception
  {
    final List<String> resources = new ArrayList<String>();
    resources.add( IGreenRoof.IDEAL_LANDUSE_EXTENSIVE );
    resources.add( IGreenRoof.IDEAL_LANDUSE_INTENSIVE );
    resources.add( ISwale.IDEAL_LANDUSE );
    resources.add( ISwaleInfiltrationDitch.IDEAL_LANDUSE );

    for( final String resource : resources )
    {
      final URL source = getClass().getResource( String.format( "resources/idealLanduseSuds/%s.nuz", resource ) ); //$NON-NLS-1$
      final File destination = new File( m_conf.getNutzungDir(), resource + ".nuz" ); //$NON-NLS-1$
      FileUtils.copyURLToFile( source, destination );
    }
  }

  private void writeIdealLanduse( final IObservation observation, final Writer zmlWriter ) throws SensorException, IOException
  {
    final IAxis[] axisList = observation.getAxisList();
    final IAxis idleDateAxis = ObservationUtilities.findAxisByType( axisList, ITimeserieConstants.TYPE_DATE );
    final IAxis kcAxis = ObservationUtilities.findAxisByType( axisList, ITimeserieConstants.TYPE_KC );
    final IAxis wtAxis = ObservationUtilities.findAxisByType( axisList, ITimeserieConstants.TYPE_WT );
    final IAxis laiAxis = ObservationUtilities.findAxisByType( axisList, ITimeserieConstants.TYPE_LAI );
    final ITupleModel values = observation.getValues( null );
    final int count = values.getCount();
    final int yearOffset = 2001;
    for( int row = 0; row < count; row++ )
    {// TODO: hier evtl. noch Zeitzone ber�cksichtigen - au�erdem mit Fortran abgleichen!!!
      final Date date = (Date) values.getElement( row, idleDateAxis );
      final Calendar calendar = NATimeSettings.getInstance().getCalendar( date );

      final int year = calendar.get( Calendar.YEAR ) - yearOffset;
      final int month = calendar.get( Calendar.MONTH ) + 1;
      final int day = calendar.get( Calendar.DATE );
      zmlWriter.write( FortranFormatHelper.printf( day, "i2" ).replaceAll( " ", "0" ) ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
      zmlWriter.write( "." ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( month, "i2" ).replaceAll( " ", "0" ) ); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
      zmlWriter.write( "." ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( year, "i2" ).replaceAll( " ", "0" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

      final Double kc = (Double) values.getElement( row, kcAxis );
      final Double wt = (Double) values.getElement( row, wtAxis );
      final Double lai = (Double) values.getElement( row, laiAxis );

      zmlWriter.write( FortranFormatHelper.printf( kc, "f8.2" ) ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( wt, "f8.2" ) ); //$NON-NLS-1$
      zmlWriter.write( FortranFormatHelper.printf( lai, "f8.2" ) ); //$NON-NLS-1$
      zmlWriter.write( "\n" ); //$NON-NLS-1$
    }
  }
}
