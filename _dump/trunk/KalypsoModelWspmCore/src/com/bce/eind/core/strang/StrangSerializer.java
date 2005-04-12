package com.bce.eind.core.strang;

import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Locale;
import java.util.Scanner;

/**
 * @author gernot
 *
 */
public class StrangSerializer
{
  private StrangSerializer( )
  {
  }

  public static StrangInfo load( final LineNumberReader lnr, final String basepath ) throws IOException
  {
    final Collection<ProfilInfo> infos = new LinkedList<ProfilInfo>();
    
    final Scanner s = new Scanner( lnr );
    // use decimal points
    s.useLocale( Locale.US );
    
    final int profilcount = s.nextInt();
    /* final int strangcount = */ s.nextInt();
    
    /* final String gewname = */ s.next();
    /* final String strangname = */ s.next();
    
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
      
      infos.add( new ProfilInfo( station, basepath + filename ) );
    }
    
    final ProfilInfo[] pinfos = infos.toArray( new ProfilInfo[infos.size()] );
    return new StrangInfo( pinfos, 0 );
  }
}
