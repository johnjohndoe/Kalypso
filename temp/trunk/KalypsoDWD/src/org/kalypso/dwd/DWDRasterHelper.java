package org.kalypso.dwd;

import java.io.File;
import java.io.FileFilter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.io.filefilter.PrefixFileFilter;
/**
 * Helper class for dwd raster based methodes
 * 
 * @author doemming
 */
public class DWDRasterHelper
{
  private static SimpleDateFormat m_lmDateFormat = new SimpleDateFormat( "'lm_'yyyy'_'MM'_'dd'_'hh" );

  
/**
 * example filename for dwd raster format: "lm_2004_11_10_00"
 * @param file rasterfile 
 * @return date from raster 
 */
  public static Date getDateFromRaster( File file )
  {
    String fileName = file.getName();
    try
    {
      return m_lmDateFormat.parse( fileName );
    }
    catch( ParseException e )
    {
      System.out.println( " file " + fileName + " is must be in format \"lm_yyyy_MM_dd_hh\"" );
      return null;
    }
  }

  public static File getNewestFileAndRemoveOthers( File srcDir )
  {
    final FileFilter filter = new PrefixFileFilter( "lm_" );
    final File[] files = srcDir.listFiles( filter );
    File result = null;
    Date date = null;
    // search newest...
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      if( file.isDirectory() )
        continue;
      Date testdate = DWDRasterHelper.getDateFromRaster( file );
      if( testdate == null )
        continue;
      if( result == null )
      {
        result = file;
        date = testdate;
      }
      else if( testdate.after( date ) )
      {
        result = file;
        date = testdate;
      }
    }
    if( result == null )
      return null;
    // got it
    // remove others
    for( int i = 0; i < files.length; i++ )
    {
      final File file = files[i];
      if( !file.isDirectory() && ( file != result ) )
      {
        System.out.println( "remove " + file.getName() );
        file.delete();
      }
    }
    return result;
  }
}