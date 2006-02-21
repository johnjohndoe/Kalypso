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
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.IOUtils;
import org.kalypso.contribs.java.util.FortranFormatHelper;
import org.kalypso.convert.namodel.NAConfiguration;
import org.kalypso.convert.namodel.timeseries.NATimeSettings;
import org.kalypso.gmlschema.GMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureProperty;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

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
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
/*
 * 
 * @author huebsch
 */
public class NutzungManager extends AbstractManager
{

  private final NAConfiguration m_conf;

  private final IFeatureType m_LanduseFT;

  private final IFeatureType m_IdleLanduseFT;

  final Hashtable m_LTable = new Hashtable();

  private int m_idCounter = 0;

  public NutzungManager( GMLSchema parameterSchema, NAConfiguration conf ) throws IOException
  {
    super( conf.getParameterFormatURL() );
    // m_crs = crs;
    m_conf = conf;
    m_LanduseFT = parameterSchema.getFeatureType( "Landuse" );
    m_IdleLanduseFT = parameterSchema.getFeatureType( "IdealLandUse" );

  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#mapID(int, org.kalypsodeegree.model.feature.FeatureType)
   */
  public String mapID( int id, IFeatureType ft )
  {
    return null;
  }

  /**
   * @see org.kalypso.convert.namodel.manager.AbstractManager#parseFile(java.net.URL)
   */
  public Feature[] parseFile( URL url ) throws Exception
  {
    String nutzDatei = url.getPath().replaceAll( ".+/", "" );
    String nutzID = nutzDatei.replaceAll( "\\.nuz", "" );
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new InputStreamReader( url.openConnection().getInputStream() ) );// new
    Feature fe = null;
    while( (fe = readNextFeature( reader, nutzID )) != null )
      result.add( fe );
    return (Feature[]) result.toArray( new Feature[result.size()] );
  }

  private Feature readNextFeature( LineNumberReader reader, String nutzID ) throws Exception
  {
    final HashMap<String, String> landusePropCollector = new HashMap<String, String>();
    final Collection<FeatureProperty> fePropCol = new ArrayList<FeatureProperty>();
    String line;
    // 9
    line = reader.readLine();
    if( line == null )
      return null;
    System.out.println( reader.getLineNumber() + ": " + line );

    if( !m_LTable.containsKey( line ) )
    {
      m_idCounter = m_idCounter + 1;
      Integer idleLanduseID = new Integer( m_idCounter );
      m_LTable.put( line, idleLanduseID );
    }
    Object idleLanduseStringID = m_LTable.get( line );

    // Kommentarzeilen
    line = reader.readLine();
    line = reader.readLine();

    // FeatureProperty landuseNameProp = FeatureFactory.createFeatureProperty( "name", nutzID );
    landusePropCollector.put( "name", nutzID );

    // generate id:
    // FeatureProperty prop = (FeatureProperty)landusePropCollector.get( "name" );
    // String asciiStringId = nutzID;
    final Feature feature = getFeature( nutzID, m_LanduseFT );
    for( int i = 0; i < 12; i++ )
    {
      line = reader.readLine();
      System.out.println( "NutzParameter(" + i + "): " + line );
    }

    final Feature idleLanduseFE = getFeature( idleLanduseStringID.toString(), m_IdleLanduseFT );
    final IPropertyType pt = feature.getFeatureType().getProperty( "idealLandUsePeriodLink" );
    FeatureProperty linkedIdleLanduseProp = FeatureFactory.createFeatureProperty( pt, idleLanduseFE.getId() );
    fePropCol.add( linkedIdleLanduseProp );
    line = reader.readLine();

    // continue reading
    // Collection collection = landusePropCollector.values();
    setParsedProperties( feature, landusePropCollector, fePropCol );
    return feature;
  }

  public void writeFile( GMLWorkspace paraWorkspace ) throws Exception
  {
    Feature rootFeature = paraWorkspace.getRootFeature();
    List list = (List) rootFeature.getProperty( "landuseMember" );
    Iterator iter = list.iterator();

    while( iter.hasNext() )
    {

      final Feature nutzungFE = (Feature) iter.next();
      final IRelationType rt = (IRelationType) nutzungFE.getFeatureType().getProperty( "idealLandUsePeriodLink" );
      final Feature linkedIdealLanduseFE = paraWorkspace.resolveLink( nutzungFE, rt );
      writeFeature( nutzungFE, linkedIdealLanduseFE );
    }

  }

  private void writeFeature( Feature feature, Feature linkedIdealLanduseFE ) throws Exception
  {

    String nutzName = FeatureHelper.getAsString( feature, "name" );
    File nutzungDir = new File( m_conf.getNutzungDir() + "\\" + nutzName + ".nuz" );
    FileWriter writer = new FileWriter( nutzungDir );
    String name;
    try
    {
      name = linkedIdealLanduseFE.getProperty( "name" ).toString();
    }
    catch( Exception e )
    {
      name = "pflanzenabhaengige verdunstung";
    }
    writer.write( name );
    writer.write( "\nidealisierter jahresgang\n" );// "ideali" ist Kennung!
    writer.write( "xxdatum     F EVA    We    BIMAX\n" );
    Object idealLanduseProp = linkedIdealLanduseFE.getProperty( "idealLandUseZML" );
    writeIdealLanduse( (IObservation) idealLanduseProp, writer );
    writer.write( "993456789012345678901234567890" );
    IOUtils.closeQuietly( writer );

  }

  private void writeIdealLanduse( IObservation observation, Writer zmlWriter ) throws SensorException, IOException
  {
    IAxis[] axisList = observation.getAxisList();
    IAxis idleDateAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_DATE );
    IAxis kcAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_KC );
    IAxis wtAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_WT );
    IAxis laiAxis = ObservationUtilities.findAxisByType( axisList, TimeserieConstants.TYPE_LAI );
    ITuppleModel values = observation.getValues( null );
    int count = values.getCount();
    int yearOffset = 2001;
    for( int row = 0; row < count; row++ )
    {// TODO: hier evtl. noch Zeitzone ber¸cksichtigen - auﬂerdem mit Fortran abgleichen!!!
      Date date = (Date) values.getElement( row, idleDateAxis );
      final Calendar calendar = NATimeSettings.getInstance().getCalendar( date );

      final int year = calendar.get( Calendar.YEAR ) - yearOffset;
      final int month = calendar.get( Calendar.MONTH ) + 1;
      final int day = calendar.get( Calendar.DATE );
      zmlWriter.write( FortranFormatHelper.printf( day, "i2" ).replaceAll( " ", "0" ) );
      zmlWriter.write( "." );
      zmlWriter.write( FortranFormatHelper.printf( month, "i2" ).replaceAll( " ", "0" ) );
      zmlWriter.write( "." );
      zmlWriter.write( FortranFormatHelper.printf( year, "i2" ).replaceAll( " ", "0" ) );

      Double kc = (Double) values.getElement( row, kcAxis );
      Double wt = (Double) values.getElement( row, wtAxis );
      Double lai = (Double) values.getElement( row, laiAxis );

      zmlWriter.write( FortranFormatHelper.printf( kc, "f8.2" ) );
      zmlWriter.write( FortranFormatHelper.printf( wt, "f8.2" ) );
      zmlWriter.write( FortranFormatHelper.printf( lai, "f8.2" ) );
      zmlWriter.write( "\n" );
    }
  }
}
