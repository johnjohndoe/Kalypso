package org.kalypso.java.util.zip;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.apache.commons.io.CopyUtils;

/**
 * @author belger
 */
public class ZipUtilities
{
  private ZipUtilities()
  {
    // wird nicht instantiiert
  }

  public static void unzip( final File zip, final File targetdir ) throws ZipException, IOException
  {
    final ZipFile file = new ZipFile( zip );
    for( final Enumeration enum = file.entries(); enum.hasMoreElements(); )
    {
      final ZipEntry entry = (ZipEntry)enum.nextElement();
      
      final File newfile = new File( targetdir, entry.getName() );
      if( entry.isDirectory() )
        newfile.mkdirs();
      else
        CopyUtils.copy( file.getInputStream( entry ), new FileOutputStream( newfile ) );
    }
  }
}
