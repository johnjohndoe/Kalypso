package org.kalypso.java.io;

/**
 * Ein FileFilter was mit Wildcards beschrieben werden kann.
 * 
 * @author belger;schlienger
 */
public class MultipleWildCardFileFilter extends MultipleRegexFileFilter
{
  /**
   * Constructor
   * @param wildCards string wildCards Die zu prüfenden WildCards
   */
  public MultipleWildCardFileFilter( String[] wildCards, boolean bFilterDirs, boolean bShowDirs,
                                     boolean bIgnoreCase )
  {
    super( null, bFilterDirs, bShowDirs, bIgnoreCase );

    // die wildCards in Regex Patterns übersetzen
    String[] patterns = new String[wildCards.length];

    for( int wildID = 0; wildID < patterns.length; wildID++ )
      patterns[wildID] = translateWildCardToRegex( wildCards[wildID] );

    setPatterns( patterns, false );
  }

  public static String translateWildCardToRegex( String wildCard )
  {
    StringBuffer sb = new StringBuffer( wildCard.length(  ) ); // mindestens so lang wie die WildCard

    for( int wildID = 0; wildID < wildCard.length(  ); wildID++ )
    {
      char wildChar = wildCard.charAt( wildID );

      switch( wildChar )
      {
        case '*':
          sb.append( ".*" ); // eine Menge beliebiger Zeichen

          break;

        case '?':
          sb.append( "." ); // ein beliebiges Zeichen

          break;

        case '.':
          sb.append( "\\." );

          break;

        default:

          // alles andere einfach dranhängen
          if( Character.isLetter( wildChar ) )
            sb.append( "[" + Character.toLowerCase( wildChar ) + Character.toUpperCase( wildChar ) +
                       "]" ); // Buchstaben immer in beiden Formen zulassen
          else
            sb.append( wildChar ); // sonst einfach anhängen td: das geht sicher nicht mit allen Zeichen gut

          break;
      }
    }

    return sb.toString(  );
  }
}
