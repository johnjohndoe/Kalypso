package org.kalypso.java.io.filter;

import java.io.File;

import java.util.regex.Pattern;

/**
 * Ein FileFilter was mit Regex definiert werden kann.
 * 
 * @author belger;schlienger
 */
public class MultipleRegexFileFilter implements java.io.FileFilter
{
  /** Die Regex Pattern, eines davon muss die gesuchte Datei erfüllen */
  private Pattern[] m_patterns;

  /** Pattern auch auf Verzeichnisse anwenden */
  private final boolean m_bFilterDirs;

  /**
   * falls false, wird für accept( File f ) immer false zurückgegeben, falls f
   * ein Verzeichnis ist
   */
  private final boolean m_bShowDir;

  /**
   * Constructor
   * 
   * @param patterns
   *          string regex pattern unqualified filename must match to be
   *          included.
   */
  public MultipleRegexFileFilter( final String[] patterns, boolean bFilterDirs, boolean bShowDirs,
      boolean bIgnoreCase )
  {
    m_bFilterDirs = bFilterDirs;
    m_bShowDir = bShowDirs;

    setPatterns( patterns, bIgnoreCase );
  }

  public void setPatterns( String[] patterns, boolean bIgnoreCase )
  {
    if( patterns == null )
    {
      m_patterns = null;

      return;
    }

    m_patterns = new Pattern[patterns.length];

    int flags = bIgnoreCase ? ( Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE ) : 0;

    for( int patternID = 0; patternID < patterns.length; patternID++ )
      m_patterns[patternID] = Pattern.compile( patterns[patternID], flags );
  }

  /**
   * Select only files that match the pattern.
   * 
   * @param file
   *          the name of the file
   * 
   * @return true if and only if the name should be included in the file list;
   *         false otherwise.
   */
  public boolean accept( File file )
  {
    if( m_patterns == null )
      return false;

    // falls Verzeichnisse nicht gezeigt werden sollen, gleich zurück
    if( file.isDirectory() && !m_bShowDir )
      return false;

    // falls Pattern nicht auf Verzeichnis engewendet wereden soll, gleich
    // zurück
    if( file.isDirectory() && !m_bFilterDirs )
      return true;

    String name = file.getName();

    for( int patternID = 0; patternID < m_patterns.length; patternID++ )
    {
      if( m_patterns[patternID].matcher( name ).matches() == true )
        return true;
    }

    return false;
  }
}