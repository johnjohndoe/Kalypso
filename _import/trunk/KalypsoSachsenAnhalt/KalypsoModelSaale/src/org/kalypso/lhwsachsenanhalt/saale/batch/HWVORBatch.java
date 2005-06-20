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
package org.kalypso.lhwsachsenanhalt.saale.batch;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.OutputStreamWriter;
import java.util.logging.Logger;

import javax.xml.bind.Marshaller;

import org.kalypso.lhwsachsenanhalt.saale.HWVOR00Converter;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.zml.ObjectFactory;
import org.kalypso.zml.ObservationType;

public class HWVORBatch
{
  private Logger m_logger = Logger.getLogger( HWVORBatch.class.getName() );

  /** Syntax: HWVORBatch <inputdir><outputdir> */
  public void convert( String[] args ) throws Exception
  {
    if( args.length != 2 )
    {
      System.out.println( "Usage: HWVORBatch <inputdir> <outputdir>" );
      return;
    }

    final File outputDir = new File( args[1] );

    File currDir = new File( args[0] );
    File wasserDir = new File( outputDir, "Wasserstand" );
    File durchDir = new File( outputDir, "Durchfluss" );
    File speicherDir = new File( outputDir, "Speicherinhalt" );
    File temperaturDir = new File( outputDir, "Temperatur" );
    //    File wqDir = new File( outputDir, "WQ" );
    File niederDir = new File( outputDir, "Niederschlag" );
    File schneeDir = new File( outputDir, "Schnee" );

    File currFile;
    String files[] = currDir.list();
    String sValue;

    final ObjectFactory zmlFac = new ObjectFactory();
    final Marshaller marshaller = zmlFac.createMarshaller();
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );

    IObservation[] obs;
    ObservationType type;
    MetadataList metadata;

    for( int i = 0; i < files.length; i++ )
    {
      final String file = files[i].toUpperCase();
      if( file.endsWith( ".VOR" ) )
      {

        if( file.startsWith( "W_" ) )
        {
          currDir = wasserDir;
          sValue = TimeserieConstants.TYPE_WATERLEVEL;
        }
        else if( file.startsWith( "Q_" ) )
        {
          currDir = durchDir;
          sValue = TimeserieConstants.TYPE_RUNOFF;
        }
        else if( file.startsWith( "TS" ) )
        {
          currDir = speicherDir;
          sValue = TimeserieConstants.TYPE_VOLUME;
        }
        //        else if( files[i].startsWith( "WQ" ) )
        //        {
        //          currDir = wqDir;
        //          sValue = TimeserieConstants.MD_WQ;
        //        }
        else if( file.startsWith( "P_" ) )
        {
          currDir = niederDir;
          sValue = TimeserieConstants.TYPE_RAINFALL;
        }
        else if( file.startsWith( "SN" ) )
        {
          currDir = schneeDir;
          sValue = TimeserieConstants.TYPE_RAINFALL;
        }
        else if( file.startsWith( "TL" ) )
        {
          currDir = temperaturDir;
          sValue = TimeserieConstants.TYPE_TEMPERATURE;
        }
        else
        {
          currDir = outputDir;
          sValue = TimeserieConstants.TYPE_TEMPERATURE;
        }

        metadata = new MetadataList();
        metadata.put( "FILE_NAME", args[0] + "\\" + files[i] );

        m_logger.info( "Lese Zeitreihen: " + files[i] );
        obs = HWVOR00Converter.toZML( sValue, new FileReader( args[0] + "/" + files[i] ), metadata );

        currDir.mkdirs();
        for( int l = 0; l < obs.length; l++ )
        {
          type = ZmlFactory.createXML( obs[l], null );

          // use IResource

          currFile = new File( currDir, "ID" + obs[l].getName() + ".zml" );

          final FileOutputStream stream = new FileOutputStream( currFile );
          final OutputStreamWriter writer = new OutputStreamWriter( stream, "UTF-8" );
          marshaller.marshal( type, writer );
          writer.close();
        }
      }
    }
  }
}
