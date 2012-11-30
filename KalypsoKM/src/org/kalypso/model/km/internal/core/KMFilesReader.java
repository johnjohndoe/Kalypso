package org.kalypso.model.km.internal.core;

import java.io.File;
import java.io.FileFilter;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.io.IOCase;
import org.apache.commons.io.filefilter.SuffixFileFilter;
import org.kalypso.contribs.java.lang.NumberUtils;

public class KMFilesReader implements IKMReader
{
  // double
  private final static String _D = "(\\d*\\.\\d*)"; //$NON-NLS-1$

  // integer
  private final static String _I = "(\\d+)"; //$NON-NLS-1$

  // space
  private final static String _S = ".+?"; //$NON-NLS-1$

  private final static Pattern PATTERN_HEAD = Pattern.compile( _S + _D + _S + "Station \\[km\\].*" ); //$NON-NLS-1$

  private final static Pattern PATTERN_TABLE = Pattern.compile( _S + _I + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + _S + _D + ".*?" ); //$NON-NLS-1$

  private final static FileFilter KM_FILEFILTER = new SuffixFileFilter( ".km", IOCase.INSENSITIVE ); //$NON-NLS-1$

  /**
   * The profile files.
   */
  private final File[] m_profileFiles;

  public KMFilesReader( final File profileDir )
  {
    this( profileDir.listFiles( KM_FILEFILTER ) );
  }

  /**
   * @param profileFiles
   *          The profile files.
   */
  public KMFilesReader( final File[] profileFiles )
  {
    m_profileFiles = profileFiles;
  }

  @Override
  public ProfileDataSet getDataSet( ) throws IOException
  {
    final ProfileDataSetFactory factory = new ProfileDataSetFactory();

    for( final File file : m_profileFiles )
    {
      final ProfileData qwProfile = createQWProfile( file );
      factory.addProfileData( qwProfile );
    }

    return factory.createProfileDataSet();
  }

  private ProfileData createQWProfile( final File file ) throws IOException
  {
    final FileReader fileReader = new FileReader( file );
    final LineNumberReader lineReader = new LineNumberReader( fileReader );
    ProfileData wqProfile = null;
    final List<Row> rows = new ArrayList<>();
    while( lineReader.ready() )
    {
      final String line = lineReader.readLine();
      if( line == null )
        break;

      final Matcher kmMatcher = PATTERN_HEAD.matcher( line );
      if( kmMatcher.matches() )
      {
        final BigDecimal station = NumberUtils.parseBigDecimal( kmMatcher.group( 1 ) );
        wqProfile = new ProfileData( file.getAbsolutePath(), station );
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