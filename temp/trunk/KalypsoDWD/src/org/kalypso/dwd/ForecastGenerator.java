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
package org.kalypso.dwd;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.FileUtils;
import org.kalypso.dwd.dwdzml.DwdzmlConf;
import org.kalypso.dwd.dwdzml.ObjectFactory;
import org.kalypso.dwd.dwdzml.DwdzmlConfType.CatchmentType;

/**
 * 
 * ForecastGenerator generates zml-forecast timeseries from dwd raster formated
 * files. parameters must be provided via configuration file.
 * 
 * @author doemming
 */
public class ForecastGenerator
{
  private DwdzmlConf m_conf = null;

  private static final SimpleDateFormat m_zmlDF = new SimpleDateFormat( "yyyyMMddHHmm" );

  private final File m_confFile;

  private static DecimalFormat m_decimalFormat = new DecimalFormat();

  private final RasterStorage m_storage;
  static
  {
    DecimalFormatSymbols dfs = new DecimalFormatSymbols();
    dfs.setDecimalSeparator( '.' );
    m_decimalFormat.setDecimalFormatSymbols( dfs );
    m_decimalFormat.setMaximumFractionDigits( 4 );
    m_decimalFormat.setMinimumIntegerDigits( 1 );
  }

  /**
   * @param args:
   *          configurationfile
   */
  public static void main( String[] args )
  {
    File confFile = new File( args[0] );
    if( confFile.exists() )
    {
      ForecastGenerator generator = new ForecastGenerator( confFile );
      try
      {
        generator.init();
        generator.generateForecast();
      }

      catch( IOException e )
      {
        e.printStackTrace();
      }
      catch( ParseException e )
      {
        e.printStackTrace();
      }
      catch( JAXBException e )
      {
        e.printStackTrace();
      }

    }
  }

  public ForecastGenerator( File confFile )
  {
    m_confFile = confFile;
    m_storage = new RasterStorage();
  }

  public void init() throws JAXBException
  {
    final ObjectFactory o = new ObjectFactory();
    final Unmarshaller unmarshaller = o.createUnmarshaller();
    m_conf = (DwdzmlConf)unmarshaller.unmarshal( m_confFile );
  }

  public void generateForecast() throws IOException, ParseException
  {
    final File rasterFile = DWDRasterHelper.getNewestFileAndRemoveOthers( new File( m_conf
        .getInputFolder() ) );
    m_storage.loadRaster( rasterFile );
    final List catchmentList = m_conf.getCatchment();
    for( Iterator iter = catchmentList.iterator(); iter.hasNext(); )
    {
      final CatchmentType catchment = (CatchmentType)iter.next();
      final String fid = catchment.getFid();
      final List posList = catchment.getRasterPos();
      createTimserie( fid, posList );
    }

    final String processedRasterDir = m_conf.getProcessedRasterDir();
    System.out.println( "processed file " + rasterFile.getCanonicalPath() );
    if( processedRasterDir != null && processedRasterDir.length() > 0 )
    {
      File processedRasterStorage = new File( processedRasterDir );
      if( !processedRasterStorage.exists() )
        processedRasterStorage.mkdirs();
      FileUtils.copyFile( rasterFile, new File( processedRasterStorage, rasterFile.getName() ) );
      System.out.println( "copied processed file to " + processedRasterStorage.getCanonicalPath() );
      rasterFile.delete();
      System.out.println( "removed processed file " + rasterFile.getCanonicalPath() );
    }
    else
    {
      System.out.println( "processed file " + rasterFile.getCanonicalPath() );
      System.out.println( "processed file will NOT be delete (see configuration)" );
    }
  }

  private boolean createTimserie( String id, List posList ) throws IOException
  {
    System.out.println( "Feature: " + id + " has " + posList.size() + " rasterpoints" );
    final String statusValue = m_conf.getDefaultStatusValue() == null ? "" : m_conf
        .getDefaultStatusValue();
    final SortedMap zr = new TreeMap();
    createTimserie( zr, posList, DWDRaster.KEY_RAIN );
    createTimserie( zr, posList, DWDRaster.KEY_SNOW );
    StringBuffer csvBuffer = new StringBuffer();
    Set set = zr.keySet();
    for( Iterator iter = set.iterator(); iter.hasNext(); )
    {
      final Date date = (Date)iter.next();
      final Double value = (Double)zr.get( date );
      final double niederschlag = value.doubleValue() / 100d;
      csvBuffer.append( m_zmlDF.format( date ) + "," + m_decimalFormat.format( niederschlag ) + ","
          + statusValue + "\n" );
    }
    final File ascciZmlFile = new File( m_conf.getOutputFolder(), id + ".csv" );
    if( ascciZmlFile.exists() )
      ascciZmlFile.delete();
    // write data:
    final FileWriter writer = new FileWriter( ascciZmlFile );
    writer.write( csvBuffer.toString() );
    writer.close();
    // write repositoryconf:
    if( m_conf.isCreateZMLFile() )
      writeZMLLink( id, ascciZmlFile );
    return true;
  }

  private void createTimserie( SortedMap timeserie, List posList, int rasterKey )
  {

    final List rasters = (List)m_storage.get( rasterKey );
    for( Iterator iter = rasters.iterator(); iter.hasNext(); )
    {
      final DWDRaster raster = (DWDRaster)iter.next();
      double value = 0d;
      for( Iterator i2 = posList.iterator(); i2.hasNext(); )
      {
        int pos = ( (Integer)i2.next() ).intValue();
        final String rValue = (String)raster.getElementAt( pos );
        final double d = Double.parseDouble( rValue );
        value += d;
      }
      final Date date = raster.getDate();
      final double niederschlag = value / posList.size();
      if( timeserie.containsKey( date ) )
      {
        double newValue = niederschlag + ( (Double)timeserie.get( date ) ).doubleValue();
        timeserie.put( date, new Double( newValue ) );
      }
      else
        timeserie.put( raster.getDate(), new Double( niederschlag ) );
    }
  }

  private void writeZMLLink( String id, File ascciZmlFile )
  {
    String base;
    try
    {
      base = getBasisZML();

      String content = base.replaceAll( ":ascciFile:", ascciZmlFile.getName() );
      content = content.replaceAll( ":title:", "Vorhersage Niederschlag " + id );
      File linkFile = new File( ascciZmlFile.getParent(), id + ".zml" );
      FileWriter writer = new FileWriter( linkFile );
      writer.write( content );
      writer.close();
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  private static String basisZML = null;

  private String getBasisZML() throws IOException
  {
    if( basisZML == null )
    {

      StringBuffer result = new StringBuffer();
      InputStream resourceAsStream = getClass().getResourceAsStream(
          "resource/baseLinkVorhersage.zml" );
      InputStreamReader reader = new InputStreamReader( resourceAsStream );
      char[] buffer = new char[100];
      int i;
      while( ( i = reader.read( buffer ) ) >= 0 )
      {
        result.append( buffer, 0, i );
      }
      reader.close();
      basisZML = result.toString();
    }
    return basisZML;
  }

}