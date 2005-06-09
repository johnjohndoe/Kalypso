package com.bce.eind.core.strang;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Scanner;

import org.apache.commons.io.IOUtils;

/**
 * @author gernot
 */
public class StrangSerializer
{
  private StrangSerializer( )
  {
  }

  /**
   * Liest eine .str Datei in einen StrangInfo.
   * 
   * @param strpath
   *          Pfad auf die .str Datei
   * @param prfname
   *          Falls ungleich null, wird der Anfangsindex auf diese Profildatei gesetzt, sonst auf
   *          den ersten.
   * @throws IOException
   * @throws IOException
   */
  public static StrangInfo load( final String strpath, final String prfname ) throws IOException
  {
    LineNumberReader lnr = null;
    try
    {
      final File strFile = new File( strpath );
      lnr = new LineNumberReader( new FileReader( strFile ) );

      final File profDir = strFile.getParentFile();
      final File prfSollFile = new File( profDir, prfname );

      final Collection<ProfilInfo> infos = new LinkedList<ProfilInfo>();

      final Scanner s = new Scanner( lnr );
      // use decimal points
      s.useLocale( Locale.US );

      final int profilcount = s.nextInt();
      /* final int strangcount = */s.nextInt();

      /* final String gewname = */s.next();
      /* final String strangname = */s.next();
      int index = 0; // index to set the StrangInfo to
      for( int i = 0; i < profilcount; i++ )
      {
        final IOException exception = s.ioException();
        if( exception != null )
          throw exception;

        // Gewässername
        s.next();

        final String station = s.next();

        // VZK
        s.next();

        // MFB
        s.next();

        // Zustandsname
        s.next();

        final String filename = s.next();

        final File prfFile = new File( profDir, filename );
        if( prfFile.equals( prfSollFile ) )
          index = i;

        infos.add( new ProfilInfo( station, prfFile.getAbsolutePath() ) );
      }

      final ProfilInfo[] pinfos = infos.toArray( new ProfilInfo[infos.size()] );
      return new StrangInfo( pinfos, index );
    }
    finally
    {
      IOUtils.closeQuietly( lnr );
    }
  }
}
