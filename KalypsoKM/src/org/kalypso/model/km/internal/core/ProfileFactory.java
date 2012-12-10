package org.kalypso.model.km.internal.core;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
 * Liest die KM-Dateien und erzeugt hieraus die Profildaten
 */
public class ProfileFactory
{
  // double
  private final static String _D = "(\\d*\\.\\d*)"; //$NON-NLS-1$

  // integer
  private final static String _I = "(\\d+)"; //$NON-NLS-1$

  // space
  private final static String _S = ".+?"; //$NON-NLS-1$

  final static Pattern PATTERN_HEAD = Pattern.compile( _S + _D + _S + "Station \\[km\\].*" ); //$NON-NLS-1$

  final static Pattern PATTERN_TABLE = Pattern.compile( _S + _I + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + ".*?" ); //$NON-NLS-1$

  final static FileFilter KM_FILEFILTER = new FileFilter()
  {
    @Override
    public boolean accept( final File file )
    {
      return file.getName().endsWith( "km" ); //$NON-NLS-1$
    }
  };

  public static ProfileDataSet createProfileSet( final File[] profileFiles, final double minKM, final double maxKM )
  {
    return new ProfileDataSet( profileFiles, 1000d * minKM, 1000d * maxKM );
  }

  public static ProfileDataSet createProfileSet( final File profileDir, final double minKM, final double maxKM )
  {
    final File[] profileFiles = profileDir.listFiles( KM_FILEFILTER );
    return createProfileSet( profileFiles, minKM, maxKM );
  }

  public static ProfileDataSet createProfileSet( final File profileDir )
  {
    final File[] profileFiles = profileDir.listFiles( KM_FILEFILTER );
    return new ProfileDataSet( profileFiles );
  }

  public static ProfileData createQWProfile( final File file, final double min, final double max ) throws IOException
  {
    final FileReader fileReader = new FileReader( file );
    final LineNumberReader lineReader = new LineNumberReader( fileReader );
    ProfileData wqProfile = null;
    final List<Row> rows = new ArrayList<Row>();
    while( lineReader.ready() )
    {
      final String line = lineReader.readLine();
      if( line == null )
        break;

      final Matcher kmMatcher = PATTERN_HEAD.matcher( line );
      if( kmMatcher.matches() )
      {
        final double meter = 1000d * Double.parseDouble( kmMatcher.group( 1 ) );
        wqProfile = new ProfileData( file, min, max, meter );
        continue;
      }
      final Matcher tableMatcher = PATTERN_TABLE.matcher( line );
      if( tableMatcher.matches() )
      {
        // number
        // final int index = Integer.parseInt(tableMatcher.group(1));
        // waterstage [mNN]
        final double hNN = Double.parseDouble( tableMatcher.group( 2 ) );
        // discharge river bed [m3/s]
        final double q = Double.parseDouble( tableMatcher.group( 3 ) );
        // discharge foreland [m3/s]
        final double qf = Double.parseDouble( tableMatcher.group( 4 ) );
        // area river bed [m2]
        final double a = Double.parseDouble( tableMatcher.group( 5 ) );
        // area foreland [m2]
        final double af = Double.parseDouble( tableMatcher.group( 6 ) );
        // width river bed [m]
        final double w = Double.parseDouble( tableMatcher.group( 7 ) );
        // width foreland [m]
        final double wf = Double.parseDouble( tableMatcher.group( 8 ) );
        // slope [m/m]
        final double i = Double.parseDouble( tableMatcher.group( 9 ) );
        rows.add( new Row( hNN, q, qf, a, af, w, wf, i ) );
      }
    }
    final Row[] row = rows.toArray( new Row[rows.size()] );
    wqProfile.set( row );
    lineReader.close();
    return wqProfile;
  }

}
