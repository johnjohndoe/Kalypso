package org.kalypso.convert;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.model.feature.FeatureFactory;

/**
 * @author doemming
 */
public abstract class AbstractManager
{
  private final HashMap m_map = new HashMap();

  // (stringID, intID)
  // und
  // (intDI,stringID)

  public final Feature m_rootFeature;

  private static final HashMap m_allFeatures = new HashMap(); // (stringID,feature)

  private String[] m_asciiFormat;

  public AbstractManager( File parseDefinition, Feature rootFeature ) throws IOException
  {
    readParseDefinition( parseDefinition );
    m_rootFeature = rootFeature;
  }
  
  public Feature getFeature( int id, FeatureType ft )
  {
    if( !exists( id ) )
      createFeature( id, ft );
    return (Feature)m_allFeatures.get( getMappedID( id ) );
  }

  //  public ExtFeature getFeature( String id, FeatureType ft )
  //  {
  //    if( !exists( id ) )
  //      createFeature( id, ft );
  //    return (ExtFeature)m_managedFeatures.get( id );
  //  }

  private boolean exists( int id )
  {
    Integer key = new Integer( id );
    return m_map.containsKey( key );
  }

  //  private boolean exists( String id )
  //  {
  //    return m_map.containsKey( id );
  //  }
  //
  //  private void createFeature( String id, FeatureType ft )
  //  {
  //    createMapping( id );
  //    ExtFeature feature = ExtFeatureFactory.createFeature( ft, id );
  //    m_allFeatures.put( id, feature );
  //  }

  private void createFeature( int id, FeatureType ft )
  {
    createMapping( id );
    String stringID = getMappedID( id );
    Feature feature = null;//= FeatureFactory.createFeature( stringID,
    // m_featureType,
    //    new Object[m_featureType.getProperties().length] );
    try
    {
      feature = FeatureFactory.createFeature(stringID,ft); 
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }

    m_allFeatures.put( stringID, feature );
  }

  //  private void createMapping( String id )
  //  {
  //    Integer intID = new Integer( getID( id ) );
  //    m_map.put( id, intID );
  //    m_map.put( intID, id );
  //  }

  private void createMapping( int id )
  {
    Integer intID = new Integer( id );
    String stringID = intID.toString();
    m_map.put( stringID, intID );
    m_map.put( intID, stringID );
  }

//  public final int getID( String id )
//  {
//    return Integer.parseInt(id);
//  }


  //  private int getMappedID( String id )
  //  {
  //    if( m_map.containsKey( id ) )
  //    {
  //      Integer value = (Integer)m_map.get( id );
  //      return value.intValue();
  //    }
  //    return -1;
  //  }

  private final String getMappedID( int id )
  {
    Integer key = new Integer( id );
    if( m_map.containsKey( key ) )
      return (String)m_map.get( key );
    return null;
  }

  private void readParseDefinition( File file ) throws IOException
  {
    List result = new ArrayList();
    LineNumberReader reader = new LineNumberReader( new FileReader( file ) );

    String line;
    while( ( line = reader.readLine() ) != null )
      result.add( line );
    m_asciiFormat = (String[])result.toArray( new String[result.size()] );
  }

  public abstract Feature[] parseFile( File file ) throws IOException;

  public abstract void writeFile( Writer writer ) throws IOException;

  private final static Pattern patternBrackets = Pattern.compile( "[\\(|\\)]" );

  public void createProperties( HashMap propCollector, String line, int formatLine )
  {
    createProperties( propCollector, line, m_asciiFormat[formatLine] );
  }

  public void createProperties( HashMap propCollector, String line, String formatLine )
  {
    line = line + "                                                ";
    List nameCollector = new ArrayList();
    StringBuffer pattern = new StringBuffer( "^" );
    String[] formats = patternBrackets.split( formatLine );
    for( int i = 0; i < formats.length; i++ )
    {
      String format = formats[i];
      String regExp = getRegExp( format, nameCollector );
      pattern.append( regExp );
    }
    pattern.append( "\\s*$" );
    Pattern linePattern = Pattern.compile( pattern.toString() );
    Matcher m = linePattern.matcher( line );
    if( !m.matches() )
    {
      System.out.println( "format:" + formatLine );
      System.out.println( "line:" + line );
      System.out.println( "regExp:" + pattern.toString() );
      System.out.println( "\n\n                :-( does not match !!!!!!!! \n\n\n" );
    }
    else
    {
      for( int i = 0; i < m.groupCount(); i++ )
      {
        String name = (String)nameCollector.get( i );
        String value = m.group( i + 1 ).trim();
        //System.out.println( 1 + ". " + name + "=" + value );
        propCollector.put( name, FeatureFactory.createFeatureProperty( name, value ) );
      }
    }
  }

  private final static Pattern pFortranFormat = Pattern
      .compile( "(a|A|i|I|f|F)([0-9]*)\\.?([0-9]*)" );

  private final static Pattern pSpaceFormat = Pattern.compile( "_*" );

  private final static Pattern pPairFormat = Pattern.compile( "^.+,.+$" );

  private final static String textRegExp = "[a-zA-Z0-9_\\. ]";

  private final static String freeFormat = "\\s*[a-zA-Z0-9_\\.-]+\\s*";

  private final static String decimalPoint = "[ \\.]";

  private final static String decimalValue = "[0-9 ]";

  private String getRegExp( String string, List nameCollector )
  {
    Matcher m = pPairFormat.matcher( string );
    if( m.matches() )
      return "(" + getPairRegExp( string, nameCollector ) + ")";
    m = pSpaceFormat.matcher( string );
    if( m.matches() )
    {
      return string.replace( '_', ' ' );
    }
    throw new UnsupportedOperationException();
  }

  private String getPairRegExp( String fPair, List nameCollector )
  {
    if( "".equals( fPair ) )
      return "";
    final String[] s = fPair.split( "," );
    nameCollector.add( s[0] ); // PropertyName

    final String format = s[1];

    if( "*".equals( format ) || "a".equals( format ) || "A".equals( format ) )
      return freeFormat;
    Matcher m = pFortranFormat.matcher( format );
    if( m.matches() )
    {
      String type = m.group( 1 );
      String regExpChar = "";
      if( "aA".indexOf( type ) >= 0 )
        regExpChar = textRegExp;
      if( "iIfF".indexOf( type ) >= 0 )
        regExpChar = decimalValue;

      String charMax = m.group( 2 ); // gesamt anzahl stellen
      String decimalPlace = m.group( 3 ); // Nachkommastellen

      if( "".equals( decimalPlace ) )
      {
        return regExpChar + "{1," + charMax + "}";
      }
      int max = Integer.parseInt( charMax );
      int decimal = Integer.parseInt( decimalPlace );
      return regExpChar + "{1," + ( max - decimal - 1 ) + "}" + decimalPoint + regExpChar + "{"
          + ( decimal ) + "}";
    }
    throw new UnsupportedOperationException();
  }

  public String toAscci( Feature feature, int formatLine )
  {
    StringBuffer result = new StringBuffer( "" );

    String[] formats = patternBrackets.split( m_asciiFormat[formatLine] );
    for( int i = 0; i < formats.length; i++ )
    {
      String format = formats[i];
      Matcher m = pPairFormat.matcher( format );
      if( m.matches() )
        result.append( toAsciiValue( feature, format ) );
      m = pSpaceFormat.matcher( format );
      if( m.matches() )
        result.append( format.replace( '_', ' ' ) );
    }

    return result.toString();
  }

  private String toAsciiValue( Feature feature, String pairFormat )
  {
    if( "".equals( pairFormat ) )
      return "";
    final String[] s = pairFormat.split( "," );
    if( "todo".equals( s[0] ) )
      return "(TODO:" + s[0] + ")";
    if( "IGNORE".equals( s[0] ) )
      return "";
    Object property = feature.getProperty( s[0] );
    if( property == null )
      return "(" + s[0] + "==NULL ?)";
    String value = property.toString(); // PropertyName

    final String format = s[1];

    if( "*".equals( format ) || "a".equals( format ) || "A".equals( format ) )
      return value;
    Matcher m = pFortranFormat.matcher( format );
    if( m.matches() )
    {
      String type = m.group( 1 );

      String charMax = m.group( 2 ); // gesamt anzahl stellen
      int max = Integer.parseInt( charMax );
      String decimalPlace = m.group( 3 ); // Nachkommastellen
      int decimal;
      StringBuffer result = new StringBuffer( "" );
      for( int i = 0; i < max; i++ )
        result.append( " " );
      if( value == null || "".equals( value ) )
        return result.toString();
      if( !"".equals( decimalPlace ) )
        decimal = Integer.parseInt( decimalPlace );
      else
      {
        if( "aA".indexOf( type ) >= 0 ) //TEXT
        {
          return ( result.replace( 0, value.length(), value ) ).toString();
        }
        decimal = 0;
      }

      boolean found = false;
      while( !found )
      {
        int pointPos = value.indexOf( '.' );
        if( pointPos < 0 && decimal > 0 ) // da fehlt ein Komma
        {
          value = value + ".0";
          continue;
        }
        if( pointPos < 0 && decimal == 0 )
        {
          found = true;
          continue;
        }
        int points = value.length() - pointPos - 1;
        if( points == decimal )
        {
          found = true;
          continue;
        }

        if( points > decimal ) // zuviele Nachkommastellen
        {
          value = value.substring( 0, value.length() - 1 );
          continue;
        }
        if( points < decimal ) // zuwenig Nachkommastellen
        {
          value = value + "0";
          continue;
        }

      }

      value = value.trim();
      return ( result.replace( max - value.length(), max, value ) ).toString();
    }
    throw new UnsupportedOperationException();

  }

  public void setParsedProperties( Feature feature, Collection collection )
  {
    FeatureType ft = feature.getFeatureType();

    Iterator it = collection.iterator();
    while( it.hasNext() )
    {
      FeatureProperty feProp = (FeatureProperty)it.next();
      if( ft.getProperty( feProp.getName() ) != null )
        feature.setProperty( feProp );
      else
        System.out.println( "property does not exist: >" + feProp.getName() + "="
            + feProp.getValue() + "<" );
    }
  }

  public static String createFormatLine( String name, String format, String separator, int count )
  {
    //    (ngwzu,*)
    if( count < 1 )
      return "";
    StringBuffer b = new StringBuffer( "(" + name + "" + 0 + "," + format + ")" );
    if( count > 1 )
    {
      for( int i = 1; i < count; i++ )
      {
        b.append( separator + "(" + name + "" + i + "," + format + ")" );
      }
    }
    return b.toString();
  }
}